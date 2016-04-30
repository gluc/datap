




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
#' @importFrom utils capture.output
#' @export
Load <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateRawTree(lol)

  errors <- CheckSyntaxRawTree(tree)
  if (errors$`.hasErrors`) {
    stop("Context contains syntax errors! Run CheckSyntax(con) to get an error report.")
  }

  ResolveFlow(tree)

  aggregationErrors <- CheckAggregationTree(tree)
  if (aggregationErrors$`.hasErrors`) {
    stop("Context contains aggregation errors! Run CheckAggregation(con) to get an error report.")
  }

  ParseTree(tree)

  refErrors <- CheckReferencesTree(tree)
  if (refErrors$`.hasErrors`) {
    stop("Context contains unresolved references! Run CheckReferences(con) to get an error report.")
  }
  return (tree)
}


#' @importFrom data.tree Node FromListSimple
CreateRawTree <- function(lol) {

  rawTree <- FromListSimple(lol, nameName = NULL)

  #only children of pipe and junction are true nodes
  ReplaceNodesWithLol(rawTree, "arguments", lol)
  ReplaceNodesWithLol(rawTree, "variables", lol)
  ReplaceNodesWithLol(rawTree, "parameters", lol)
  ReplaceNodesWithLol(rawTree, "attributes", lol)



  return (rawTree)

}



ResolveFlow <- function(tree) {

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

  tree$Do(function(node) node$rank <- node$parent$name,
          filterFun = function(x) identical(x$parent$type, "pipe") ||
                                  identical(x$parent$type, "junction") ||
                                  identical(x$parent$type, "tap")
          )


  tree$Do(function(node) node$downstream <- GetDownstreamPath(node), filterFun = isNotRoot)




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


#' @importFrom data.tree isNotRoot
ParseTree <- function(tree) {
  tree$name <- "context"
  tree$type <- "context"

  class(tree) <- c("context", class(tree))

  #data.tree:::print.Node(tree, ds = function(joint) paste0(joint$downstream, collapse = "|"))

  tree$Do(function(joint) {
    ds <- joint$Navigate(joint$downstream)
    ds$upstream[[joint$name]] <- joint
  },
  filterFun = isNotRoot)

  #data.tree:::print.Node(tree, ds = function(joint) paste0(l(joint$upstream, "name"), collapse = "|"))


  tree$Do(function(node) class(node) <- c("tap", class(node)), filterFun = function(x) identical(x$type, "tap"))


  #add dummy function to pipes
  tree$Do(function(node) {
    node$`function` <- 'identity'
    node$arguments <- list(x = "@inflow")
  }, filterFun = function(node) identical(node$type, "pipe"))

  tree$Do(function(node) node$variables <- ParseVariables(node$parent, node$variables))
  tree$Do(function(node) node$parameters <- ParseVariables(node, node$parameters), filterFun = function(node) identical(node$type, "tap"))

  tree %>%
    Traverse(traversal = function(node) node$upstream,
             filterFun = function(node)!(node$type %in% JOINT_TYPES_STRUCTURE)) %>%
    rev %>%
    Do(function(node) node$parameters <- GetParameters(node))

  tree$Do(function(node) node$arguments <- ParseVariables(node, node$arguments))
  tree$Do(fun = function(node) node$dynamicVariables <- GetDynamicVariables(node))

  Traverse(tree,
           filterFun = function(node) !is.null(node$type) && node$type %in% JOINT_TYPES_FUN) -> traversal
  traversal %>% rev %>%
  Do(function(joint) joint$fun <- ParseFun(joint))

  tree$Do(fun = function(node) node$tap <- node$children[[1]]$fun,
          filterFun = function(node) identical(node$type, "tap"))

  tree$TapNames <- function() names(tree$children)
}


GetTap <- function(joint) {
  if (joint$type == "tap") return (joint)
  if (joint$isRoot) return (NULL)
  return (GetTap(joint$parent))
}



# Finds static variables in arguments and replaces them
# with their values.
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


# Get variables that are dynamic, such
# as @inflow, @inflowfun or @context. Assumes non-dynamic
# variables have already been parsed.
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
    xprs <- parse(text = "list(...)") #required to avoid warning in CHECK

    #print (node$name)
    #if (node$name == "Cache") browser()
    CallStep <- function() {
      #if (node$name == "MinLength") browser()
      print (node$name)
      #parse parameters
      parameterNames <- ls()
      myArgs <- lapply(parameterNames, function(x) get(x))
      names(myArgs) <- parameterNames

      if ("..." %in% names(formals())) ellipsis <- eval(xprs)
      else ellipsis <- list()

      funArgs <- ParseParameters(node, funArgs, myArgs, ellipsis)

      if ("@inflow" %in% node$dynamicVariables) {
        upstream <- node$upstream
        if (length(upstream)) stop(paste0("Cannot find @inflowfun for ", node$name))
        children <- lapply(upstream, function(child) {
          childArguments <- GetChildArguments(node, child, myArgs, ellipsis)
          do.call(child$fun, childArguments)
        })
        if (length(upstream) == 1) children <- children[[1]]

        funArgs[[which(funArgs == "@inflow")]] <- children
      }
      if ("@inflowfun" %in% node$dynamicVariables) {
        upstream <- node$upstream
        if (length(upstream) == 0) stop(paste0("Cannot find @inflowfun for ", node$name))
        children <- lapply(upstream, function(child) child$fun)
        if (length(upstream) == 1) children <- children[[1]]
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
      if (!node$type == "factory") print(paste0("Processed ", node$name))
      return (res)
    }
    CallStep <- SetFormals(CallStep, node)
    if (node$type == "factory") {
      fun <- CallStep()
    } else fun <- CallStep

    return (fun)

}






GetDownstreamPath <- function(joint) {
  if (!joint$type %in% JOINT_TYPES_FUN) return ("..") #structure, tap
  parentType <- joint$parent$type
  if (identical(parentType, "tap")) return ("..")
  if (identical(parentType, "junction")) return ("..")
  if (identical(parentType, "pipe")) {
    if (joint$position == 1) return ("..")
    ds <- joint$parent$children[[joint$position - 1]]
    #if (identical(ds$type, "junction")) stop(paste0("No element allowed after junction ", ds$name, "!"))
    if (ds$type %in% c("junction", "pipe")) {
      src <- GetSourcesPath(ds, path = "..")
      if (length(src) > 1) {
        # stop(paste0("Cannot connect ", joint$name, " upstream. Joint ", joint$Navigate(src), " has more than one source."))
      }
      pth <- src
    } else pth <- paste("..", ds$name, sep = "/")
    return (pth)
  }
  else stop(paste0("Unexpected joint parent type ", parentType, " of joint ", joint$name))
}


GetSourcesPath <- function(joint, path = ".") {
  if (identical(joint$type, "junction")) {
    usjs <- joint$children
    path <- paste0(path, "/", joint$name)
  } else if (identical(joint$type, "pipe")) {
    usjs <- joint$children[joint$count]
    path <- paste0(path, "/", joint$name)
  } else if (identical(joint$parent$type, "pipe")) {
    usjs <- joint$parent$children[joint$parent$count]
    #path <- paste0(path, "/", joint$parent$name, "/", joint$name)
    path <- paste0(path, "/", joint$name)
  } else {
    path <- paste0(path, "/", joint$name)
    return (path)
  }
  if (length(usjs) == 1 && identical(usjs[[1]], joint)) return (path)
  res <- sapply(usjs, function(j) GetSourcesPath(j, path))
  return (res)
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



# Replaces variables in funArgs that point to a tap parameter
# with the respective argument
#
# @param node the node
# @param funArgs the arguments of the processing step, i.e. the
# function to be called inside the node$fun
# @param myArgs the arguments of the node$fun (coming from the parameters
# declaration)
# @param ellipsis the ellipsis arguments
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
GetParameters <- function(node) {
  availableParameters <- GetTap(node)$parameters

  if (length(availableParameters) == 0) return (list())

  upstreamParameters <- Get(node$upstream, "parameters")

  availableParameters[paste0('@', names(availableParameters)) %in% node$arguments] %>% names -> myParameters

  myParameters <- c(myParameters, upstreamParameters) %>% unique

  res <- availableParameters[names(availableParameters) %in% myParameters]
  return (res)
}



