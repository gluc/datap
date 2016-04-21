


VARIABLE_RESERVED_NAMES_CONST <- c( 'inflow',
                                    'inflowfun',
                                    'context')

JOINT_TYPES_FLOW <- c('pipe', 'junction')
JOINT_TYPES_JOINTS <- c( 'processor', 'factory')
JOINT_TYPES_QA <- c('warning', 'error')
JOINT_TYPES_STRUCTURE <- c('structure', 'module')

JOINT_TYPES <- c('tap', JOINT_TYPES_FLOW, JOINT_TYPES_JOINTS, JOINT_TYPES_QA, JOINT_TYPES_STRUCTURE)
JOINT_TYPES_FUN <- c(JOINT_TYPES_FLOW, JOINT_TYPES_JOINTS, JOINT_TYPES_QA)


ELEMENTS <- c("attributes", "variables", "parameters", "function", "arguments")

#' Loads a meta definition.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' context <- Load(filePath)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree FromListExplicit Do isNotRoot
#' @export
Load <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateRawTree(lol)

  CheckSyntaxRawTree(tree)

  SimplifyTree(tree)

  ParseTree(tree)

  CheckReferences(tree)
  return (tree)
}


#' @importFrom data.tree Node FromListSimple
#' @export
CreateRawTree <- function(lol) {

  rawTree <- FromListSimple(lol, nameName = NULL)

  #only children of pipe and junction are true nodes
  ReplaceNodesWithLol(rawTree, "arguments", lol)
  ReplaceNodesWithLol(rawTree, "variables", lol)
  ReplaceNodesWithLol(rawTree, "parameters", lol)
  ReplaceNodesWithLol(rawTree, "attributes", lol)



  return (rawTree)

}



SimplifyTree <- function(tree) {

  #prune modules
  tree$Prune(pruneFun = function(node) !identical(node$type, "module"))

  #prune branches without a tap ()
  #really necessary?
  tree$Prune(pruneFun = function(node) {
    any(node$Get("type") == "tap", na.rm = TRUE) || any(node$Get("type", traversal = "ancestor") == "tap", na.rm = TRUE)
  })

  #set empty lists
  tree$Do(function(node) node$arguments <- as.list(node$arguments),
          filterFun = function(x) !is.null(x$arguments))
  tree$Do(function(tapNode) if (is.null(tapNode$parameters)) tapNode$parameters <- list(),
          filterFun = function(node) identical(node$type, "tap"))

  #convert pipe sequence to children
  tree$Do(function(pipeNode) {
    parent <- pipeNode$children[[1]]
    for (child in pipeNode$children[-1]) {
      pipeNode$RemoveChild(child$name)
      parent$AddChildNode(child)
      parent <- child
    }

  },
  filterFun = function(node) identical(node$type, "pipe"))

  if (FALSE) {
    #remove pipes

    #avoid duplicate names
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

    #remove pipes
    tree$Do(function(pipeNode) {
      child <- pipeNode$children[[1]]
      pipeNode$RemoveChild(child$name)
      pipeNode$parent$AddChildNode(child)
      pipeNode$parent$RemoveChild(pipeNode$name)
    },
    filterFun = function(node) identical(node$type, "pipe"),
    traversal = "post-order")
  }
}

ReplaceNodesWithLol <- function(rawTree, name, lol) {
  rawTree$Do(function(node) {
    for(n in node$path[-1]) lol <- lol[[n]]
    parent <- node$parent
    parent$RemoveChild(name)
    parent[[name]] <- lol
  },
  filterFun = function(node) node$name == name)
}



ParseTree <- function(tree) {
  tree$name <- "context"

  class(tree) <- c("context", class(tree))
  tree$Do(function(node) class(node) <- c("tap", class(node)), filterFun = function(x) identical(x$type, "tap"))

  #add dummy function to pipes
  tree$Do(function(node) {
    node$`function` <- 'identity'
    node$arguments <- list(x = "@inflow")
  }, filterFun = function(node) identical(node$type, "pipe"))

  tree$Do(function(node) node$variables <- ParseVariables(node$parent, node$variables))
  tree$Do(function(node) node$parameters <- ParseVariables(node, node$parameters), filterFun = function(node) identical(node$type, "tap"))
  tree$Do(function(node) node$parameters <- GetUpstreamParameters(node))

  tree$Do(function(node) node$arguments <- ParseVariables(node, node$arguments))
  tree$Do(fun = function(node) node$dynamicVariables <- GetDynamicVariables(node))
  tree$Do(fun = function(node) node$fun <- ParseFun(node),
          traversal = "post-order",
          filterFun = function(node) !is.null(node$type) && node$type %in% JOINT_TYPES_FUN)

  tree$Do(fun = function(node) node$tap <- node$children[[1]]$fun,
          filterFun = function(node) identical(node$type, "tap"))

  tree$TapNames <- function() names(tree$children)
}




#' Finds static variables in arguments and replaces them
#' with their values.
ParseVariables <- function(node, funArgs) {
  if (length(funArgs) == 0) return (funArgs)
  #parse variables (except @inflow, @inflowfun, etc)
  for (i in 1:length(funArgs)) {
    v <- funArgs[[i]]
    if (!v %in% paste0('@', VARIABLE_RESERVED_NAMES_CONST) && identical(substr(v, 1, 1), "@")) {
      if (!IsMacro(v)) {
        v <- substr(v, 2, nchar(v))
        tr <- Traverse(node, traversal = "ancestor", filterFun = function(x) !is.null(x$variables[[v]]))
        if (length(tr) > 0) {
          vval <- Get(tr[1], function(x) x$variables[[v]])[[1]]
          funArgs[[i]] <- vval
        }
      } else {
        #macro
        substr(v, 2, nchar(v)) %>% parse(text = .) -> funArgs[[i]]
      }
    }

  }
  return (funArgs)
}

IsMacro <- function(v) {
  identical(substr(v, nchar(v), nchar(v)), ")")
}


#' Get variables that are dynamic, such
#' as @inflow, @inflowfun or @context. Assumes non-dynamic
#' variables have already been parsed.
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
      #if (node$name == "Yahoo") browser()
      #parse parameters
      parameterNames <- ls()
      myArgs <- lapply(parameterNames, function(x) get(x))
      names(myArgs) <- parameterNames

      if ("..." %in% names(formals())) ellipsis <- list(...)
      else ellipsis <- list()

      funArgs <- ParseParameters(node, funArgs, myArgs, ellipsis)

      if ("@inflow" %in% node$dynamicVariables) {
        children <- lapply(node$children, function(child) {
          childArguments <- GetChildArguments(node, child, myArgs, ellipsis)
          do.call(child$fun, childArguments)
        })
        if (node$count == 1) children <- children[[1]]

        funArgs[[which(funArgs == "@inflow")]] <- children
      }
      if ("@inflowfun" %in% node$dynamicVariables) {
        children <- lapply(node$children, function(child) child$fun)
        if (node$count == 1) children <- children[[1]]
        funArgs[[which(funArgs == "@inflowfun")]] <- children
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


# Get a list of arguments, matching the upstream function's
# parameters.
GetChildArguments <- function(node, child, myArgs, ellipsis) {
  if ("..." %in% names(formals(child$fun))) {
    childParameters <- c(ellipsis, myArgs[names(child$parameters)[names(child$parameters) != "..."]])
  } else {
    childParameters <- myArgs[names(child$parameters)]
  }
  return (childParameters)
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



#' Replaces variables in funArgs that point to a tap parameter
#' with the respective argument
#'
#' @param node the node
#' @param funArgs the arguments of the processing step, i.e. the
#' function to be called inside the node$fun
#' @param myArgs the arguments of the node$fun (coming from the parameters
#' declaration)
#' @param ellipsis the ellipsis arguments
ParseParameters <- function(node, funArgs, myArgs, ellipsis) {

  if (!is.null(myArgs)) {
    for (i in 1:length(funArgs)) {
      v <- funArgs[[i]]
      if (identical(v, "@...")) {
        funArgs <- c(funArgs, ellipsis)
        funArgs <- funArgs[-i]
      } else if (!v %in% paste0('@', VARIABLE_RESERVED_NAMES_CONST) && identical(substr(v, 1, 1), "@")) {
        v <- substr(v, 2, nchar(v))
        if (!is.null(myArgs[[v]])) {
          funArgs[[i]] <- myArgs[[v]]
        }
      } else if (identical(v, '@context')) {
        funArgs[[i]] <- node$root
      }

    }
  }
  return (funArgs)
}

# Finds the tap parameters that will be used on
# this joint or on any of its upstream joints
GetUpstreamParameters <- function(node) {
  #if (node$name == "MATap") browser()
  parameters <- GetAttribute(node, "parameters", inheritFromAncestors = TRUE, nullAsNa = FALSE)
  if (length(parameters) == 0) return (list())
  node$Get(function(x) parameters[paste0('@', names(parameters)) %in% x$arguments] %>% names) %>% unique %>% unlist -> prms
  res <- parameters[names(parameters) %in% prms]
  return (res)
}



