
#' Loads a context definition and checks its syntax.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' errors <- CheckSyntax(filePath)
#' print(errors, na.print = "")
#'
#' @importFrom yaml yaml.load
#' @export
CheckSyntax <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateTree(lol)

  CheckSyntaxRawTree(tree)

  tree$Do(function(joint) joint$hasErrors <- Aggregate(joint,
                                                       function(usj) {
                                                         if (length(usj$hasErrors) > 0) return(usj$hasErrors)
                                                         if (!usj$isLeaf) return (NULL)
                                                         usj$parent$name == ".errors"
                                                       },
                                                       aggFun = any),
          traversal = "post-order")

 res <- ToDataFrameTree(tree, "type", "message",
          pruneFun = function(joint) joint$hasErrors)

 return (res)
}


HasErrors <- function(joint) {

  Aggregate(joint,
            function(usj) {
              if (!usj$isLeaf) return (NULL)
              usj$parent$name == ".errors"
            },
            aggFun = any)

}


CheckSyntaxRawTree <- function(rawTree) {

  rawTree$Do(function(joint) CheckJointSyntax(joint), filterFun = isNotRoot)

}


print.datapsyntax <- function(x, ...) {
  res <- paste0('  $', x$type, "Name:")
  res <- paste0(res, "\n", '    type: ', x$type)

}


CheckJointSyntax <- function(joint) {

  syntaxDef <- GetSyntaxDefinition(joint)
  do.call(CheckJoint, c(joint, syntaxDef))

}


GetSyntaxDefinition <- function(joint) {
  if (identical(joint$type, "structure") ) {

    res <- list(allowedElements = c("variables"),
                requiredElements = c(),
                allowedParents = c("structure"),
                minChildren = 1,
                maxChildren = .Machine$integer.max)

  } else if (identical(joint$type, "tap") ) {

    res <- list(allowedElements = c("attributes", "parameters", "variables"),
               requiredElements = c(),
               allowedParents = c("structure"),
               minChildren = 1,
               maxChildren = 1)

  } else if(identical(joint$type, "pipe") ) {

    res <- list(allowedElements = c("attributes", "variables"),
                requiredElements = c(),
                allowedParents = c("tap", "pipe", "junction", "module"),
                minChildren = 1,
                maxChildren = .Machine$integer.max)

  } else if(identical(joint$type, "junction") ) {

    res <- list(allowedElements = c("attributes", "variables", "function", "arguments"),
               requiredElements = c("function"),
               allowedParents = c("pipe", "junction", "tap", "module"),
               minChildren = 1,
               maxChildren = .Machine$integer.max)

  } else if(identical(joint$type, "processor") ) {

    res <- list(allowedElements = c("attributes", "function", "arguments"),
               requiredElements = c("function"),
               allowedParents = c("pipe", "junction", "tap"),
               minChildren = 0,
               maxChildren = 0)

  } else if(identical(joint$type, "factory") ) {

    res <- list(allowedElements = c("attributes", "function", "arguments"),
               requiredElements = c("function"),
               allowedParents = c("pipe"),
               minChildren = 0,
               maxChildren = 0)

  } else if(identical(joint$type, "warning") || identical(joint$type, "error") ) {

    res <- list(allowedElements = c("attributes", "function", "arguments"),
               requiredElements = c("function"),
               allowedParents = c("pipe"),
               minChildren = 0,
               maxChildren = 0)

  } else if(identical(joint$type, "module")) {

    res <- list(allowedElements = c("attributes"),
                requiredElements = c(),
                allowedParents = c("module"),
                minChildren = 1,
                maxChildren = .Machine$integer.max)


  } else if (!is.null(joint$type)) {
    AssertSyntax(FALSE, joint, "", 1, "Unknown type ", joint$type, ".")
  } else if (!isRoot(joint)) {
    AssertSyntax(FALSE, joint, "", 2, "Parsing problem!")
  }
  class(res) <- c("datapsyntax", class(res))
  return (res)
}



AssertSyntax <- function(condition, joint, errorSection, errorCode, ...) {
  if (!condition) {
    msg <- list(...)
    msg <- paste0(msg, collapse = "")
    if (!".errors" %in% names(joint$children)) joint$AddChild(".errors")
    err <- joint$`.errors`
    if (nchar(errorSection) > 0) {
      if (!errorSection %in% names(err$children)) err$AddChild(errorSection)
      err <- err[[errorSection]]
    }
    err <- err$AddChild(errorCode)
    err$message <- msg
  }
  return (condition)
}


NonErrorCount <- function(joint) {
  #if (joint$name == "NA handling") browser()
  joint$children %>% extract(., names(.) != ".errors") %>% length -> res
  return (res)
}

CheckJoint <- function(joint, allowedElements, requiredElements, allowedParents, allowedChildren, minChildren, maxChildren) {

  CheckChildCount(joint, minChildren, maxChildren)

  CheckAllowedParents(joint, allowedParents)
  CheckAllowedElements(joint, allowedElements)
  CheckRequiredElements(joint, requiredElements)
  CheckAttributes(joint)
  CheckParameters(joint)
  CheckVariables(joint)
  CheckArguments(joint)


}

CheckChildCount <- function(joint, minChildren, maxChildren) {
  AssertSyntax(NonErrorCount(joint) >= minChildren,
               joint,
               "",
               1000,
               joint$type, " must have at least", minChildren, "upstream joints.")

  AssertSyntax(NonErrorCount(joint) <= maxChildren,
               joint,
               "",
               1001,
               joint$type, " cannot have more than ", maxChildren, "upstream joints.")
}


CheckAllowedChildren <- function(joint, allowedChildren) {
  AssertSyntax(!is.null(child$type) && child$type %in% allowedChildren,
               joint,
               "",
               1100,
               "Upstream of ", joint$type, " must be any of ", paste(allowedChildren, collapse = ", "), ".")

}


CheckAllowedParents <- function(joint, allowedParents) {
  AssertSyntax(joint$parent$isRoot || joint$parent$type %in% allowedParents,
               joint,
               "",
               1200,
               "Downstream of ", joint$type, " must be any of ", paste(allowedParents, collapse = ", "), ".")

}


CheckAllowedElements <- function(joint, allowedElements) {
  AssertSyntax(all(joint$fields %>% extract(., . != "type") %in% allowedElements),
               joint,
               "",
               1300,
               "Only ", paste0(allowedElements, collapse = ", "), " allowed in ", joint$type)
}


CheckRequiredElements <- function(joint, requiredElements) {
  AssertSyntax(all(requiredElements %in% joint$fields %>% extract(., . != "type")),
               joint,
               "",
               1400,
               joint$type, " must have elements ", requiredElements, ".")
}





CheckAttributes <- function(joint) {
  #nothing to check

}

CheckParameters <- function(joint) {
  prms <- joint$parameters
  if (!is.null(prms)) {

    cond <- AssertSyntax(is.list(prms),
                 joint,
                 "parameters",
                 1600,
                 "Parameters must be a list.")
    if (!cond) return()
    AssertSyntax(length(prms) == 0 || !is.null(names(prms)),
                 joint,
                 "parameters",
                 1601,
                 "Parameters must be an associative list.")


  }
}

CheckVariables <- function(joint) {
  vrbls <- joint$variables

  if (!is.null(vrbls)) {

    cond <- AssertSyntax(is.list(vrbls),
                 joint,
                 "variables",
                 1700,
                 "Variables must be a list.")

    if (!cond) return()

    AssertSyntax(!is.null(names(vrbls)),
                 joint,
                 "variables",
                 1701,
                 "Variables must be an associative list.")


  }
}


CheckFunction <- function(joint) {
  fnct <- joint$fun
  if (!is.null(fnct)) {
    AssertSyntax(is.character(fnct),
                 joint,
                 "function",
                 1800,
                 "Function must be a character.")
  }
}


CheckArguments <- function(joint) {
  args <- joint$arguments
  if (!is.null(args)) {
    if (joint$name == "Closing Prices") browser()
    cond <- AssertSyntax(is.vector(args),
                         joint,
                         "arguments",
                         1900,
                         "Arguments must be a list.")


  }
}
