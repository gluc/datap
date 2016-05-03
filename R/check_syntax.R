
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

  return (tree)

}




CheckSyntaxRawTree <- function(rawTree) {

  tree <- Clone(rawTree)
  syntaxDef <- GetSyntaxDefinition()

  tree$Do(function(joint) CheckSyntaxType(joint), filterFun = isNotRoot)

  if (!FindErrors(tree)) {

    tree$Do(function(joint) CheckSyntaxJoint(joint, syntaxDef), filterFun = isNotRoot)

    FindErrors(tree)

  }
  EnrichErrorReport(tree, "syntax")

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
      allowedElements = c("attributes", "variables", "condition"),
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
      allowedElements = c("attributes", "function", "arguments", "condition"),
      requiredElements = c("function"),
      allowedParents = c("pipe", "junction", "tap"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$factory <- list(
      type = "factory",
      allowedElements = c("attributes", "function", "arguments", "condition"),
      requiredElements = c("function"),
      allowedParents = c("pipe"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$warning <- list(
      type = "warning",
      allowedElements = c("attributes", "function", "arguments", "condition"),
      requiredElements = c("function"),
      allowedParents = c("pipe"),
      mustHaveParent = TRUE,
      minChildren = 0,
      maxChildren = 0
    )

    res$error <- list(
      type = "error",
      allowedElements = c("attributes", "function", "arguments", "condition"),
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


CheckSyntaxType <- function(joint) {
  AssertSyntax(length(joint$type) > 0,
               joint,
               "node type",
               "not set",
               NULL,
               "2000",
               "Joint has no type.")
}


CheckSyntaxChildCount <- function(joint, minChildren, maxChildren) {
  AssertSyntax(NonErrorCount(joint) >= minChildren,
               joint,
               "upstream",
               "mincount",
               NULL,
               "1000",
               joint$type, " '", joint$name, "' must have at least ", minChildren, " upstream joints.")

  AssertSyntax(NonErrorCount(joint) <= maxChildren,
               joint,
               "upstream",
               "maxcount",
               NULL,
               "1001",
               joint$type, " '", joint$name, "' cannot have more than ", maxChildren, " upstream joints.")
}




CheckSyntaxAllowedParents <- function(joint, allowedParents, mustHaveParent) {
  #if (joint$name == "XYZ") browser()
  AssertSyntax(!mustHaveParent || !joint$parent$isRoot,
               joint,
               "downstream",
               "",
               NULL,
               "1200",
               joint$type, " '", joint$name, "' requires a downstream joint.")

  AssertSyntax(joint$parent$isRoot || joint$parent$type %in% allowedParents,
               joint,
               "downstream",
               "",
               NULL,
               "1201",
               "Downstream of ", joint$type, " '", joint$name, "' must be any of ", paste(allowedParents, collapse = ", "), ".")

}


CheckSyntaxAllowedElements <- function(joint, allowedElements) {
  for (element in joint$fields %>% extract(., . != "type")) {
    AssertSyntax(element %in% allowedElements,
               joint,
               "allowedElements",
               element,
               NULL,
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
               NULL,
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
                 NULL,
                 "1600",
                 "Parameters must be a list.")
    if (!cond) return()
    AssertSyntax(length(prms) == 0 || !is.null(names(prms)),
                 joint,
                 "parameters",
                 "",
                 NULL,
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
                 NULL,
                 "1700",
                 "Variables must be a list.")

    if (!cond) return()

    AssertSyntax(!is.null(names(vrbls)),
                 joint,
                 "variables",
                 "",
                 NULL,
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
                 NULL,
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
                         NULL,
                         "1900",
                         "Arguments must be a list.")


  }
}
