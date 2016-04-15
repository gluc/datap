
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
  tree$Do(function(node) class(node) <- c("tap", class(node)), filterFun = function(x) x$level == 2)

  tree$Do(function(node) node$parameters <- GetUpstreamParameters(node))
  tree$Do(function(node) node$arguments <- ParseVariables(node))
  tree$Do(fun = function(node) node$dynamicVariables <- ParseDynamicVariables(node))
  tree$Do(fun = function(node) node$fun <- ParseFun(node),
          traversal = "post-order",
          filterFun = function(x) x$level > 2)

  tree$Do(fun = function(node) node$tap <- ParseTapFun(node),
          filterFun = function(x) identical(x$type, "tap"))

  tree$TapNames <- function() names(tree$children)
  #return
  return (tree)
}


#' @importFrom data.tree FromListSimple
#' @export
CreateTree <- function(lol) {

  rawTree <- FromListSimple(lol, nameName = NULL)
  rawTree$Prune(pruneFun = function(node) node$level != 2 || identical(node$type, "tap"))

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

  ctx <- rawTree
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




#' Get data from a tap
#'
#' @param tapName the name of the tap
#' @param context the context object
#' @param ... any parameters to be passed on to the tap
#'
#' @export
GetData <- function(context, tapName, ...) {
  context$Climb(tapName)$fun(...)
}


VARIABLE_RESERVED_NAMES_CONST <- c( 'pipe',
                                    'pipefun')

#' Finds static variables in arguments and replaces them
#' with their values.
ParseVariables <- function(node) {
  funArgs <- node$arguments
  #parse variables (except @pipe, @pipefun, etc)
  for (i in 1:length(funArgs)) {
    v <- funArgs[[i]]
    if (!v %in% paste0('@', VARIABLE_RESERVED_NAMES_CONST) && identical(substr(v, 1, 1), "@")) {
      v <- substr(v, 2, nchar(v))
      tr <- Traverse(node, traversal = "ancestor", filterFun = function(x) !is.null(x$variables[[v]]))
      if (length(tr) > 0) {
        vval <- Get(tr[1], function(x) x$variables[[v]])[[1]]
        funArgs[[i]] <- vval
      }
    }

  }
  return (funArgs)
}

#' Parse variables that are dynamic
ParseDynamicVariables <- function(node) {
  funArgs <- node$arguments %>% unlist
  funArgs <- funArgs[substr(funArgs, 1, 1) == "@"]
  return (funArgs)
}


#' @importFrom assertthat assert_that
#' @importFrom data.tree Traverse Get GetAttribute
ParseFun <- function(node) {

  # warning, error and transformation
    funNme <- node$`function`
    funArgs <- node$arguments
    funArgsNms <- names(funArgs)

    print (node$name)

    CallStep <- function() {
      #if (node$name == "Cache") browser()
      #parse parameters
      myArgs <- GetFunctionArguments(CallStep)

      funArgs <- ParseParameters(node, funArgs, myArgs)

      if ("@pipe" %in% node$dynamicVariables) {
        children <- lapply(node$children, function(child) do.call(child$fun, myArgs[names(child$parameters)]))
        if (node$count == 1) children <- children[[1]]
        #if (node$name == "DateRange") browser()
        funArgs[[which(funArgs == "@pipe")]] <- children
      }
      if ("@pipefun" %in% node$dynamicVariables) {
        children <- lapply(node$children, function(child) child$fun)
        if (node$count == 1) children <- children[[1]]
        funArgs[[which(funArgs == "@pipefun")]] <- children
      }
      res <- do.call.intrnl(funNme, funArgs)
      if (node$type == "warning" || node$type == "error") {
        if (!res[[1]]) {
          if (node$type == "warning") warning(paste0("step ", node$name, " raised warning:", res[[2]]))
          if (node$type == "error") stop(paste0("step ", node$name, " raised error:", res[[2]]))
        }
        return (children)
      }
      print(paste0("Processed ", node$name))
      return (res)
    }
    if (length(node$parameters) > 0) formals(CallStep) <- do.call(alist, node$parameters)
    if (node$type == "function") {
      fun <- CallStep()
    } else fun <- CallStep

    return (fun)

}




ParseTapFun <- function(node) {
  child <- node$children[[1]]
  CallStep <- function() {
    myArgs <- GetFunctionArguments(CallStep)
    do.call(child$fun, myArgs[names(child$parameters)])
  }
  formals(CallStep) <- do.call(alist, node$parameters)
  return (CallStep)
}


#' Can only be called from inside a function!
GetFunctionArguments <- function(fun) {
  arguments <- lapply(names(formals(fun)), function(x) get(x, envir = sys.parent(3)))
  names(arguments) <- names(formals(fun))
  return(arguments)
}


#' Replaces variables references that point to a tap parameter
ParseParameters <- function(node, funArgs, parameterValues) {

  if (!is.null(parameterValues)) {
    for (i in 1:length(funArgs)) {
      v <- funArgs[[i]]
      if (!v %in% paste0('@', VARIABLE_RESERVED_NAMES_CONST) && identical(substr(v, 1, 1), "@")) {
        v <- substr(v, 2, nchar(v))
        if (!is.null(parameterValues[[v]])) {
          funArgs[[i]] <- parameterValues[[v]]
        }
      }

    }
  }
  return (funArgs)
}


GetUpstreamParameters <- function(node) {
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
