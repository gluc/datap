
#' Loads a meta definition.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="CodaPipeR")
#' context <- Load(filePath)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree FromListExplicit Do isNotRoot
#' @export
Load <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateTree(lol)

  class(tree) <- c("context", class(tree))
  tree$Do(function(node) class(node) <- c("specification", class(node)), filterFun = function(x) x$level == 2)

  tree$Do(fun = ParseFun,
          traversal = "post-order",
          filterFun = isNotRoot)

  #return
  return (tree)
}


#' @importFrom data.tree FromListSimple
#' @export
CreateTree <- function(lol) {

  rawTree <- FromListSimple(lol, nameName = NULL)

  #only children of pipe and junction are true nodes
  RemoveNodes(rawTree, "arguments", lol)
  RemoveNodes(rawTree, "variables", lol)
  RemoveNodes(rawTree, "parameters", lol)

  rawTree$Do(function(node) node$arguments <- as.list(node$arguments), filterFun = function(x) !is.null(x$arguments))

  rawTree$Do(function(tapNode) if (is.null(tapNode$parameters)) tapNode$parameters <- list(),
             filterFun = function(node) identical(node$type, "tap"))

  #convert pipe sequence to children
  rawTree$Do(function(pipeNode) {
                parent <- pipeNode$children[[1]]
                for (child in pipeNode$children[-1]) {
                  pipeNode$RemoveChild(child$name)
                  parent$AddChildNode(child)
                  parent <- child
                }

              },
             filterFun = function(node) identical(node$type, "pipe"))

  #remove unnecessary pipes
  #except under junctions, to avoid
  #name conflicts

  SimplifyTree(rawTree)

  ctx <- rawTree$data
  ctx$parent <- NULL
  ctx$name <- "Context"
  return (ctx)

}


SimplifyTree <- function(tree) {
  tree$Do(function(junctionNode) {
    rf <- function(node) {
      if (!identical(node$type, "pipe") && !identical(node$type, "junction")) return (node)
      lapply(node$children, function(child) rf(child)) %>% unlist %>% return
    }
    nodes <- rf(junctionNode)
    if (any(duplicated(Get(nodes, "name")))) lapply(nodes, function(node) node$name <- node$path[-c(1:junctionNode$level)] %>% paste(., collapse="."))
  },
  filterFun = function(node) return (identical(node$type, "junction"))
  )


  tree$Do(function(pipeNode) {
    child <- pipeNode$children[[1]]
    pipeNode$RemoveChild(child$name)
    pipeNode$parent$AddChildNode(child)
    pipeNode$parent$RemoveChild(pipeNode$name)
  },
  filterFun = function(node) identical(node$type, "pipe"),
  traversal = "post-order")
}

RemoveNodes <- function(rawTree, name, lol) {
  rawTree$Do(function(node) {
    for(n in node$path[-1]) lol <- lol[[n]]
    parent <- node$parent
    parent$RemoveChild(name)
    parent[[name]] <- lol

  },
  filterFun = function(node) node$name == name)
}





#' @importFrom data.tree GetDefaultTooltip
GetPlotTooltip <- function(node) {
  if (node$isRoot) return (GetDefaultTooltip(node))
  res <- paste(paste("type:", node$type),
               paste("function:", node$funName),
               paste("arguments:"),
               #paste(paste0("  ", names(node$arguments)), node$arguments, to = "ASCII", sub = "" , sep = ": ", collapse = "\n"), #causes error in DiagrammeR. Need to escape @ and ^ and possibly \
               sep = "\n")

  if (!is.null(node$description)) {
    res <- paste(res, paste("description:", node$description), sep = "\n")
  }

  return (res)

}


#' Get a timeseries
#'
#' @param id the id of the timeseries
#' @param context the context object
#'
#' @export
GetData <- function(context, id) {
  context$Climb(id)$fun(...)
}



#' @importFrom assertthat assert_that
#' @importFrom data.tree Traverse Get GetAttribute
ParseFun <- function(node) {

  # warning, error and transformation
    funNme <- node$`function`
    funArgs <- node$arguments
    funArgsNms <- names(funArgs)

    print (node$name)

    #parse variables (except @pipe)
    for (argNme in names(funArgs)) {
      v <- funArgs[[argNme]]
      if (!identical(v, "@pipe") && identical(substr(v, 1, 1), "@")) {
        print(v)
        v <- substr(v, 2, nchar(v))
        tr <- Traverse(node, traversal = "ancestor", filterFun = function(x) !is.null(x$variables[[v]]))
        if (length(tr) > 0) {
          vval <- Get(tr[1], function(x) x$variables[[v]])[[1]]
          funArgs[[argNme]] <- vval
        }
      }

    }

    if (node$type %in% c("warning", "error")) {

      parameters <- GetParametersUsedBelow(node)
      funArgs <- ParseParameters(node, funArgs, parameters)

      Wrn <- function() {

        children <- lapply(node$children, function(child) {
          childParameters <- GetParametersUsedBelow(child)
          if (length(childParameters) == 0) args <- list()
          else args <- get(names(formals(CallStep)))[childParameters]
          do.call(child$fun, args)
        })
        if (node$count == 1) children <- children[[1]]
        if ("@pipe" %in% funArgs) {
          funArgs[[which(funArgs == "@pipe")]] <- children
        }
        if ("@pipefun" %in% funArgs) {
          childfuns <- lapply(node$children, function(child) child$fun)
          if (node$count == 1) childfuns <- childfuns[[1]]
          funArgs[[which(funArgs == "@pipefun")]] <- childfuns
        }


        ok <- do.call.intrnl(funNme, funArgs)
        if (!ok[[1]]) {
          if (node$type == "warning") warning(paste0("step ", node$name, " raised warning:", ok[[2]]))
          if (node$type == "error") stop(paste0("step ", node$name, " raised error:", ok[[2]]))
        }
        print(paste0("Processed ", node$name))
        return (children)
      }
      node$fun <- Wrn
    } else if (node$type %in% c("transformation", "junction", "function")) {

      parameters <- GetParametersUsedBelow(node)
      funArgs <- ParseParameters(node, funArgs, parameters)

      CallStep <- function() {

        if ("@pipe" %in% funArgs) {
          children <- lapply(node$children, function(child) {
                                              childParameters <- GetParametersUsedBelow(child)
                                              if (length(childParameters) == 0) args <- list()
                                              else args <- get(names(formals(CallStep)))[childParameters]
                                              do.call(child$fun, args)
                                            })
          if (node$count == 1) children <- children[[1]]
          #if (node$name == "DateRange") browser()
          funArgs[[which(funArgs == "@pipe")]] <- children
        }
        if ("@pipefun" %in% funArgs) {
          children <- lapply(node$children, function(child) child$fun)
          if (node$count == 1) children <- children[[1]]
          funArgs[[which(funArgs == "@pipefun")]] <- children
        }
        res <- do.call.intrnl(funNme, funArgs)
        print(paste0("Processed ", node$name))
        return (res)
      }
      if (length(parameters) > 0) formals(CallStep) <- do.call(alist, parameters)
      if (node$type == "function") {
        node$fun <- do.call(CallStep, parameters)
      } else node$fun <- CallStep
    } else if (node$type == "source") {


      parameters <- GetParametersUsedBelow(node)
      funArgs <- ParseParameters(node, funArgs, parameters)

      CallStep <- function() {

        res <- do.call.intrnl(funNme, funArgs)
        print(paste0("Processed ", node$name))
        return (res)
      }

      if (length(parameters) > 0) formals(CallStep) <- do.call(alist, parameters)

      #if (node$name == "Quandl") browser()
      node$fun <- CallStep

    } else if (node$type == "tap") {
        node$fun <- function() do.call(node$children[[1]]$fun, node$parameters)
        formals(node$fun) <- do.call(alist, node$parameters)
    } else {
       stop(paste0("Unknown node type ", node$type))

    }


}



ParseParameters <- function(node, funArgs, parameters) {

  if (!is.null(parameters)) {
    for (argNme in names(funArgs)) {
      v <- funArgs[[argNme]]
      if (!identical(v, "@pipe") && identical(substr(v, 1, 1), "@")) {
        v <- substr(v, 2, nchar(v))
        if (!is.null(parameters[[v]])) {
          funArgs[[argNme]] <- parameters[[v]]
        }
      }

    }
  }
  return (funArgs)
}


GetParametersUsedBelow <- function(node) {
  parameters <- GetAttribute(node, "parameters", inheritFromAncestors = TRUE, nullAsNa = FALSE)
  if (length(parameters) == 0) return (list())
  node$Get(function(x) parameters[paste0('@', names(parameters)) %in% x$arguments] %>% names) %>% unique %>% unlist %>% extract(parameters, .) -> res
  return (res)
}


do.call.intrnl <- function(what, args) {
  if(is.character(what)) {
    fn <- strsplit(what, "::")[[1]]
    if(length(fn)==1) {
      fun <- fn[[1]]
      envir <- parent.frame()
    } else {
      fun <- fn[[2]]
      envir <- asNamespace(fn[[1]])
    }
  }

  do.call(fun, as.list(args), envir = envir)
}
