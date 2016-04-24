
#' Loads a context definition and checks its syntax.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' errors <- CheckSyntax(filePath)
#' print(errors)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree Aggregate
#' @export
CheckSyntax <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateRawTree(lol)

  tree <- CheckSyntaxRawTree(tree)

  if (!tree$hasErrors) {
    tree$code <- "0000"
    tree$message <- "No errors"
  } else {
    errNum <- Aggregate(tree, function(joint) {
      if (joint$name == ".errors") return (joint$leafCount)
      if (joint$isLeaf) return (0)
      return (NULL)
    }, aggFun = sum)
    tree$message <- paste0(errNum, " errors!")
  }
  tree$type <- "context"
  tree$name <- "context syntax error report"


  return (tree)

}

#' @export
print.dataperrorreport <- function(x, ...) {
  res <- ToDataFrameTree(x, "type", "code", "message",
                         pruneFun = function(joint) joint$hasErrors)

  res <- print(res, na.print = "", ...)
  invisible (res)

}


CheckSyntaxRawTree <- function(rawTree) {

  syntaxDef <- GetSyntaxDefinition()
  tree <- Clone(rawTree)
  tree$Do(function(joint) CheckSyntaxJoint(joint, syntaxDef), filterFun = isNotRoot)

  tree$Do(function(joint) joint$hasErrors <- Aggregate(joint,
                                                       function(usj) {
                                                         if (length(usj$hasErrors) > 0) return(usj$hasErrors)
                                                         if (!usj$isLeaf) return (NULL)
                                                         return (usj$parent$name == ".errors" || usj$parent$parent$name == ".errors")

                                                       },
                                                       aggFun = any),
          traversal = "post-order")


  tree$Prune(pruneFun = function(joint) joint$hasErrors)
  class(tree) <- c("dataperrorreport", class(tree))
  return (tree)

}

#'@export
print.datapsyntax <- function(x, ...) {
  #parent
  res <- paste0(paste0(">", x$allowedParents), collapse = '|')
  if (!x$mustHaveParent) res <- paste0("[", res, "]")
  res <- paste0(res, '\n')

  #jointname
  res <- paste0(res, '  $', x$type, "Name:", "\n")

  #type
  res <- paste0(res, '    type: ', x$type, "\n")

  #elements
  for (element in ELEMENTS) {
    if (element %in% x$allowedElements) {
      elestring <- paste0(">", element)
      if (!element %in% x$requiredElements) {
        elestring <- paste0("[", elestring, "]")
      }
      res <- paste0(res, "    ", elestring, "\n")
    }
  }

  #children
  if (x$maxChildren > 0) {
    chld <- NULL
    if (x$maxChildren > 1) chld <- paste0(chld, "n* ")
    chld <- paste0(chld, paste0(paste0(">", x$allowedChildren), collapse = "|"))
    if (x$minChildren == 0) chld <- paste0("[", chld, "]")
    chld <- paste0("    ", chld)
    res <- paste0(res, chld)
  }
  cat(res)
  invisible (res)
}




GetSyntaxDefinition <- function(type = NULL) {
    res <- list()
    res$structures <- list(
      type = "structure",
      allowedElements = c("attributes", "variables"),
      requiredElements = c(),
      allowedParents = c("structure"),
      mustHaveParent = FALSE,
      minChildren = 1,
      maxChildren = .Machine$integer.max
    )

    res$tap <- list(
      type = "tap",
      allowedElements = c("attributes", "parameters", "variables"),
      requiredElements = c(),
      allowedParents = c("structure"),
      mustHaveParent = FALSE,
      minChildren = 1,
      maxChildren = 1
    )

    res$pipe <- list(
      type = "pipe",
      allowedElements = c("attributes", "variables"),
      requiredElements = c(),
      allowedParents = c("tap", "pipe", "junction", "module"),
      mustHaveParent = TRUE,
      minChildren = 1,
      maxChildren = .Machine$integer.max
    )

    res$junction <- list(
      type = "junction",
      allowedElements = c("attributes", "variables", "function", "arguments"),
      requiredElements = c("function"),
      allowedParents = c("pipe", "junction", "tap", "module"),
      mustHaveParent = TRUE,
      minChildren = 1,
      maxChildren = .Machine$integer.max
    )

    res$processor <- list(
      type = "processor",
      allowedElements = c("attributes", "function", "arguments"),
      requiredElements = c("function"),
      allowedParents = c("pipe", "junction", "tap"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$factory <- list(
      type = "factory",
      allowedElements = c("attributes", "function", "arguments"),
      requiredElements = c("function"),
      allowedParents = c("pipe"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$warning <- list(
      type = "warning",
      allowedElements = c("attributes", "function", "arguments"),
      requiredElements = c("function"),
      allowedParents = c("pipe"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$error <- list(
      type = "error",
      allowedElements = c("attributes", "function", "arguments"),
      requiredElements = c("function"),
      allowedParents = c("pipe"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$module <- list(
      type = "module",
      allowedElements = c("attributes"),
      requiredElements = c(),
      allowedParents = c("module"),
      mustHaveParent = FALSE,
      minChildren = 1,
      maxChildren = .Machine$integer.max
    )


  for(joint in res) {
    res[sapply(res, function(childCandidate) joint$type %in% childCandidate$allowedParents)] %>%
    sapply(function(e) e$type) %>%
    unname ->
    joint$allowedChildren
    class(joint) <- c("datapsyntax", class(joint))
    res[[joint$type]] <- joint
  }


  if (is.null(type)) return (res)
  else return (res[[type]])
}



AssertSyntax <- function(condition, joint, errorSection, errorSubsection, errorCode, ...) {
  if (!condition) {
    msg <- list(...)
    msg <- paste0(msg, collapse = "")
    if (!".errors" %in% names(joint$children)) joint$AddChild(".errors")
    err <- joint$`.errors`
    if (nchar(errorSection) > 0) {
      if (!errorSection %in% names(err$children)) err$AddChild(errorSection)
      err <- err[[errorSection]]
    }
    if (nchar(errorSubsection) > 0) {
      if (!errorSubsection %in% names(err$children)) err$AddChild(errorSubsection)
      err <- err[[errorSubsection]]
    }
    err$code <- errorCode
    err$message <- msg
  }
  return (condition)
}


NonErrorCount <- function(joint) {
  #if (joint$name == "NA handling") browser()
  joint$children %>% extract(., names(.) != ".errors") %>% length -> res
  return (res)
}

CheckSyntaxJoint <- function(joint, syntaxDefinition) {
  mySyn <- syntaxDefinition[[joint$type]]
  CheckSyntaxChildCount(joint, mySyn$minChildren, mySyn$maxChildren)

  CheckSyntaxAllowedParents(joint, mySyn$allowedParents, mySyn$mustHaveParent)
  CheckSyntaxAllowedElements(joint, mySyn$allowedElements)
  CheckSyntaxRequiredElements(joint, mySyn$requiredElements)
  CheckSyntaxAttributes(joint)
  CheckSyntaxParameters(joint)
  CheckSyntaxVariables(joint)
  CheckSyntaxArguments(joint)


}

CheckSyntaxChildCount <- function(joint, minChildren, maxChildren) {
  AssertSyntax(NonErrorCount(joint) >= minChildren,
               joint,
               "upstream",
               "mincount",
               "1000",
               joint$type, " '", joint$name, "' must have at least", minChildren, "upstream joints.")

  AssertSyntax(NonErrorCount(joint) <= maxChildren,
               joint,
               "upstream",
               "maxcount",
               "1001",
               joint$type, " '", joint$name, "' cannot have more than ", maxChildren, "upstream joints.")
}




CheckSyntaxAllowedParents <- function(joint, allowedParents, mustHaveParent) {
  #if (joint$name == "XYZ") browser()
  AssertSyntax(!mustHaveParent || !joint$parent$isRoot,
               joint,
               "downstream",
               "",
               "1200",
               joint$type, " '", joint$name, "' requires a downstream joint.")

  AssertSyntax(joint$parent$isRoot || joint$parent$type %in% allowedParents,
               joint,
               "downstream",
               "",
               "1201",
               "Downstream of ", joint$type, " '", joint$name, "' must be any of ", paste(allowedParents, collapse = ", "), ".")

}


CheckSyntaxAllowedElements <- function(joint, allowedElements) {
  for (element in joint$fields %>% extract(., . != "type")) {
    AssertSyntax(element %in% allowedElements,
               joint,
               "allowedElements",
               element,
               "1300",
               "Only ", paste0(allowedElements, collapse = ", "), " allowed in ", joint$type, " '", joint$name, "'")
  }
}

CheckSyntaxRequiredElements <- function(joint, requiredElements) {
  for (requiredElement in requiredElements) {
    AssertSyntax(requiredElement %in% (joint$fields %>% extract(., . != "type")),
               joint,
               "requiredElements",
               requiredElement,
               "1400",
               joint$type, " '", joint$name, "' must have element ", requiredElement, ".")
  }

}



CheckSyntaxAttributes <- function(joint) {
  #nothing to check

}

CheckSyntaxParameters <- function(joint) {
  prms <- joint$parameters
  if (!is.null(prms)) {

    cond <- AssertSyntax(is.list(prms),
                 joint,
                 "parameters",
                 "",
                 "1600",
                 "Parameters must be a list.")
    if (!cond) return()
    AssertSyntax(length(prms) == 0 || !is.null(names(prms)),
                 joint,
                 "parameters",
                 "",
                 "1601",
                 "Parameters must be an associative list.")


  }
}

CheckSyntaxVariables <- function(joint) {
  vrbls <- joint$variables

  if (!is.null(vrbls)) {

    cond <- AssertSyntax(is.list(vrbls),
                 joint,
                 "variables",
                 "",
                 "1700",
                 "Variables must be a list.")

    if (!cond) return()

    AssertSyntax(!is.null(names(vrbls)),
                 joint,
                 "variables",
                 "",
                 "1701",
                 "Variables must be an associative list.")


  }
}


CheckSyntaxFunction <- function(joint) {
  fnct <- joint$fun
  if (!is.null(fnct)) {
    AssertSyntax(is.character(fnct),
                 joint,
                 "function",
                 "",
                 "1800",
                 "Function must be a character.")
  }
}


CheckSyntaxArguments <- function(joint) {
  args <- joint$arguments
  if (!is.null(args)) {
    if (joint$name == "Closing Prices") browser()
    cond <- AssertSyntax(is.vector(args),
                         joint,
                         "arguments",
                         "",
                         "1900",
                         "Arguments must be a list.")


  }
}
