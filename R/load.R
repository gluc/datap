


VARIABLE_RESERVED_NAMES_CONST <- c( 'pipe',
                                    'pipefun',
                                    'context')

JOINT_TYPES_AGGREGATORS <- c('pipe', 'junction')
JOINT_TYPES_JOINTS <- c( 'joint', 'factory')
JOINT_TYPES_QA <- c('warning', 'error')


JOINT_TYPES_NON_TAP <- c(JOINT_TYPES_AGGREGATORS, JOINT_TYPES_JOINTS, JOINT_TYPES_QA)
JOINT_TYPES <- c('tap', JOINT_TYPES_NON_TAP)



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
  tree$Do(function(node) class(node) <- c("tap", class(node)), filterFun = function(x) identical(x$type, "tap"))


  tree$Do(function(node) node$parameters <- GetUpstreamParameters(node))
  tree$Do(function(node) node$arguments <- ParseVariables(node))
  tree$Do(fun = function(node) node$dynamicVariables <- GetDynamicVariables(node))
  tree$Do(fun = function(node) node$fun <- ParseFun(node),
          traversal = "post-order",
          filterFun = function(node) !is.null(node$type) && node$type %in% JOINT_TYPES_NON_TAP)

  tree$Do(fun = function(node) node$tap <- ParseTapFun(node),
          filterFun = function(node) identical(node$type, "tap"))

  tree$TapNames <- function() names(tree$children)
  #return
  return (tree)
}


#' @importFrom data.tree Node FromListSimple
#' @export
CreateTree <- function(lol) {

  rawTree <- FromListSimple(lol, nameName = NULL)

  #only children of pipe and junction are true nodes
  RemoveNodes(rawTree, "arguments", lol)
  RemoveNodes(rawTree, "variables", lol)
  RemoveNodes(rawTree, "parameters", lol)

  rawTree$Do(function(node) node$arguments <- as.list(node$arguments), filterFun = function(x) !is.null(x$arguments))
  #rawTree$Do(function(node) node$parameters <- as.list(node$parameters), filterFun = function(x) !is.null(x$parameters))

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
  rawTree$Prune(pruneFun = function(node) {
    any(node$Get("type") == "tap", na.rm = TRUE) || any(node$Get("type", traversal = "ancestor") == "tap", na.rm = TRUE)
  })
  rawTree$name <- "context"
  return (rawTree)

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

#' Get variables that are dynamic
GetDynamicVariables <- function(node) {
  funArgs <- node$arguments %>% unlist
  funArgs <- funArgs[substr(funArgs, 1, 1) == "@"]
  return (funArgs)
}


#' @importFrom assertthat assert_that
#' @importFrom data.tree Traverse Get GetAttribute
ParseFun <- function(node) {


    funNme <- node$`function`
    funArgs <- node$arguments
    funArgsNms <- names(funArgs)

    print (node$name)
    #if (node$name == "Cache") browser()
    CallStep <- function() {
      #if (node$name == "SMA") browser()
      #parse parameters
      argumentNames <- ls()
      myArgs <- lapply(argumentNames, function(x) get(x))
      names(myArgs) <- argumentNames

      if ("..." %in% names(formals())) ellipsis <- list(...)
      else ellipsis <- list()

      funArgs <- ParseParameters(node, funArgs, myArgs, ellipsis)

      if ("@pipe" %in% node$dynamicVariables) {
        children <- lapply(node$children, function(child) {
          if ("..." %in% names(formals(child$fun))) {
            childParameters <- c(ellipsis, myArgs[names(child$parameters)[names(child$parameters) != "..."]])
          } else {
            childParameters <- myArgs[names(child$parameters)]
          }
          do.call(child$fun, childParameters)
        })
        if (node$count == 1) children <- children[[1]]

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
    CallStep <- SetFormals(CallStep, node)
    if (node$type == "factory") {
      fun <- CallStep()
    } else fun <- CallStep

    return (fun)

}

SetFormals <- function(CallStep, node) {
  if (length(node$parameters) > 0) {
    parametersList <- node$parameters
    GetParam <- function(name, defaultValue) {
      if (is.character(defaultValue)) defaultValue <- paste0("'", defaultValue, "'")
      if (is.null(defaultValue)) return (paste0(name, " ="))
      return (paste(name, defaultValue, sep = " = "))
    }

    1:length(parametersList) %>%
      lapply(function(i) GetParam(names(parametersList)[[i]], parametersList[[i]])) %>%
      paste(collapse = ", ") %>%
      paste0("alist(", ., ")") %>%
      parse(text = .) %>%
      eval -> formals(CallStep)

  }
  return (CallStep)
}

#' Create the function on the tap
ParseTapFun <- function(node) {
  child <- node$children[[1]]
  CallStep <- function() {
    argumentNames <- ls()
    myArgs <- lapply(argumentNames, function(x) get(x))
    names(myArgs) <- argumentNames
    do.call(child$fun, myArgs[names(child$parameters)])
  }
  CallStep <- SetFormals(CallStep, node)
  return (CallStep)
}


#' Can only be called from inside a function!
GetFunctionArguments <- function(fun) {
  argumentNames <- ls()
  arguments <- lapply(argumentNames, function(x) get(x))
  names(arguments) <- argumentNames
  return(arguments)
}


#' Replaces variables references that point to a tap parameter
ParseParameters <- function(node, funArgs, parameterValues, ellipsis) {

  if (!is.null(parameterValues)) {
    for (i in 1:length(funArgs)) {
      v <- funArgs[[i]]
      if (identical(v, "@...")) {
        funArgs <- c(funArgs, ellipsis)
        funArgs <- funArgs[-i]
      } else if (!v %in% paste0('@', VARIABLE_RESERVED_NAMES_CONST) && identical(substr(v, 1, 1), "@")) {
        v <- substr(v, 2, nchar(v))
        if (!is.null(parameterValues[[v]])) {
          funArgs[[i]] <- parameterValues[[v]]
        }
      } else if (identical(v, '@context')) {
        funArgs[[i]] <- node$root
      }

    }
  }
  return (funArgs)
}

#' Finds the tap parameters that will be used on
#' this node or on any of its children
GetUpstreamParameters <- function(node) {
  #if (node$name == "MATap") browser()
  parameters <- GetAttribute(node, "parameters", inheritFromAncestors = TRUE, nullAsNa = FALSE)
  if (length(parameters) == 0) return (list())
  node$Get(function(x) parameters[paste0('@', names(parameters)) %in% x$arguments] %>% names) %>% unique %>% unlist -> prms
  res <- parameters[names(parameters) %in% prms]
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
