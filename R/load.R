




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



#' @importFrom data.tree isNotRoot isLeaf
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
  #tree$Do(function(joint) joint$upstream <- list(), filterFun = isLeaf)

  #data.tree:::print.Node(tree, ds = function(joint) paste0(l(joint$upstream, "name"), collapse = "|"))


  tree$Do(function(node) class(node) <- c("tap", class(node)), filterFun = function(x) identical(x$type, "tap"))


  tree$Do(ParseFunArgs, filterFun = function(joint) length(joint$`function`) > 0)

  #add dummy function to pipes
  tree$Do(function(node) {
    node$`function` <- 'identity'
    node$arguments <- list(x = "$inflow")
  }, filterFun = function(node) identical(node$type, "pipe"))


  tree$Do(function(node) node$variables <- SubstituteVariables(node$parent, node$variables))
  tree$Do(function(node) node$condition <- SubstituteVariables(node, node$condition))
  tree$Do(function(node) node$parameters <- SubstituteVariables(node, node$parameters), filterFun = function(node) identical(node$type, "tap"))
  tree$Do(function(node) node$arguments <- SubstituteVariables(node, node$arguments))
  tree$Do(fun = function(node) node$dynamicVariables <- GetUnresolvedVariablesInFunArguments(node))

  #data.tree:::print.Node(context$FindNode("SPX"), prms = function(j) paste0(j$parameters, collapse = "|"))

  tree %>%
    Traverse(traversal = function(node) node$upstream,
             filterFun = function(node)!(node$type %in% JOINT_TYPES_STRUCTURE)) %>%
    rev %>%
    Do(function(node) node$parameters <- GetRequiredParameters(node))


  Traverse(tree,
           filterFun = function(node) !is.null(node$type) && node$type %in% JOINT_TYPES_FUN) -> traversal
  traversal %>% rev %>%
    Do(function(joint) joint$fun <- ParseFun(joint))

  tree$Do(fun = function(node) node$tap <- node$children[[1]]$fun,
          filterFun = function(node) identical(node$type, "tap"))

  tree$TapNames <- function() names(tree$children)
}



ParseFunArgs <- function(joint) {
  spl <- strsplit(joint$`function`, "(", fixed = TRUE)[[1]]
  funNme <- spl[[1]]
  funArgs <- strsplit(spl[[2]], ")", fixed = TRUE)[[1]]
  if (nchar(funArgs) > 0) {
    funArgs <- strsplit(funArgs, ",", fixed = TRUE)[[1]]
    funArgs <- lapply(funArgs, function(x) strsplit(x, " *= *")[[1]])
    funArgs <- lapply(funArgs, function(x) unname(sapply(x, stringr::str_trim)))
    funArgs <- lapply(funArgs, GetArgument)
  } else {
    funArgs <- list()
  }

  joint$`function` <- funNme
  joint$arguments <- funArgs
}


GetArgument <- function(argItem) {
  if (length(argItem) == 1) nme <- NULL
  else nme <- argItem[[1]]

  if (length(argItem) == 1) arg <- argItem[[1]]
  else arg <- argItem[[2]]

  if (substr(arg, 1, 1) != "$") {
    arg <- eval(parse(text = arg))
  } else {
    class(arg) <- c("variable", class(arg))
  }

  #names(arg) <- nme

  return (arg)

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




GetTap <- function(joint) {
  if (joint$type == "tap") return (joint)
  if (joint$isRoot) return (NULL)
  return (GetTap(joint$parent))
}





IsMacro <- function(v) {
  identical(substr(v, nchar(v), nchar(v)), ")")
}


# Get variables that are dynamic, such
# as @inflow, @joint or @context. Assumes non-dynamic
# variables have already been parsed.
GetUnresolvedVariablesInFunArguments <- function(node) {
  funArgs <- node$arguments %>% unlist
  funArgs <- funArgs[substr(funArgs, 1, 1) == '$']
  return (funArgs)
}






GetDownstreamPath <- function(joint) {
  #if (joint$name == "final") browser()
  if (!joint$type %in% JOINT_TYPES_FUN) return ("..") #structure, tap
  parentType <- joint$parent$type
  if (identical(parentType, "tap")) return ("..")
  if (identical(parentType, "junction")) return ("..")
  if (identical(parentType, "pipe")) {
    if (joint$position == 1) return ("..")
    ds <- joint$parent$children[[joint$position - 1]]
    #if (identical(ds$type, "junction")) stop(paste0("No element allowed after junction ", ds$name, "!"))
    pth <- GetSourcesPath(ds, path = paste("..", ds$name, sep = "/"))
    return (pth)
  }
  else stop(paste0("Unexpected joint parent type ", parentType, " of joint ", joint$name))
}






GetSourcesPath <- function(joint, path = ".") {

  if (joint$isLeaf) return (path)
  if (identical(joint$type, "junction")) {
    usjs <- joint$children
  } else if (identical(joint$type, "pipe")) {
    usjs <- joint$children[joint$count]
  } else {
    stop("Unexpected Error")
  }
  res <- sapply(usjs, function(j) GetSourcesPath(j, paste(path, j$name, sep = "/")))
  return (res)
}



# Finds static variables in arguments and replaces them
# with their values.
SubstituteVariables <- function(node, funArgs) {
  #if (identical(node$name, "C")) browser()
  if (length(funArgs) == 0) return (funArgs)

  #parse variables (except @inflow, @joint, etc)
  for (i in 1:length(funArgs)) {
    v <- funArgs[[i]]

    if (!v %in% paste0('$', VARIABLE_RESERVED_NAMES_CONST) && (is(v, 'variable') || identical(substr(v, 1, 1), '$'))) {
      if (!IsMacro(v)) {
        v <- substr(v, 2, nchar(v))
        tr <- Traverse(node, traversal = "ancestor", filterFun = function(x) !is.null(x$variables[[v]]))
        if (length(tr) > 0) {
          vval <- Get(tr[1], function(x) x$variables[[v]])[[1]]
          if (mode(funArgs) != "list") {
            if (length(funArgs) == 1) funArgs <- vval
            else if (mode(vval) != mode(funArgs)) {
              mode(vval) <- "list"
              funArgs[[i]] <- vval
            }
          } else {
            funArgs[[i]] <- vval
          }
        }
      } else {
        #macro
        substr(v, 2, nchar(v)) %>% parse(text = .) -> funArgs[[i]]
      }
    }

  }
  return (funArgs)
}


# Get a list of arguments, matching the upstream function's
# parameters.
GetUpstreamFunArguments <- function(node, upstreamJoint, myArgs, ellipsis) {
  if ("..." %in% names(formals(upstreamJoint$fun))) {
    childParameters <- c(ellipsis, myArgs[names(upstreamJoint$parameters)[names(upstreamJoint$parameters) != "..."]])
  } else {
    childParameters <- myArgs[names(upstreamJoint$parameters)]
  }
  return (childParameters)
}




# Finds the tap parameters that will be used on
# this joint or on any of its upstream joints
GetRequiredParameters <- function(node) {

  availableParameters <- GetTap(node)$parameters

  if (length(availableParameters) == 0) return (list())

  if (length(node$upstream) == 0) {
    upstreamParameterNames <- vector(mode = "character", length = 0)
    upstreamConditions <- vector(mode = "character", length = 0)
  } else {
    Get(node$upstream, function(j) names(j$parameters), simplify = FALSE) %>%
      unname %>%
      do.call(c, .) %>%
      unique ->
      upstreamParameterNames


    Get(node$upstream, function(j) j$condition %>% substr(., 2, nchar(.)), simplify = FALSE) %>%
      unname %>%
      do.call(c, .) %>%
      unique ->
      upstreamConditions
  }



  availableParameters[paste0('$', names(availableParameters)) %in% c(node$arguments, node$variables)] %>% names -> myParameterNames

  myParameterNames <- c(myParameterNames, upstreamParameterNames, upstreamConditions) %>% unique

  res <- availableParameters[names(availableParameters) %in% myParameterNames]
  return (res)
}



