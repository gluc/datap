#' Loads a context definition and checks its syntax.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' errors <- CheckReferences(filePath)
#' print(errors)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree Aggregate
#' @export
CheckReferences <- function(con) {
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

  tree <- CheckReferencesTree(tree)

  return (tree)

}



CheckReferencesTree <- function(tree) {
  tree <- Clone(tree)
  tree$Do(CheckReferencesJoint, filterFun = isNotRoot)
  FindErrors(tree)
  EnrichErrorReport(tree, "reference")
  return (tree)
}


CheckReferencesJoint <- function(joint) {
  #only reserved references, parameters, or macros allowed
  #all other variable references should have been replaced already

  #references can be in
  # variables
  # arguments (named or unnamed)
  # parameters

  CheckReferencesElements(joint, "variables")
  CheckReferencesElements(joint, "arguments")
  CheckReferencesElements(joint, "parameters")
  CheckReferencesElements(joint, "condition")

}


CheckReferencesElements <- function(joint, elementName) {
  elements <- joint[[elementName]]
  for (element in elements) {
    AssertSyntax(CheckReferencesElement(joint, element, elementName),
                 joint,
                 "references",
                 element,
                 NULL,
                 "3000",
                 "Reference '", element, "' cannot be resolved."
    )

  }
}


CheckReferencesElement <- function(joint, element, elementName) {

  if (length(element) == 0) return (TRUE)
  if (length(element) > 1) return (TRUE)
  if (element %in% paste0("$", VARIABLE_RESERVED_NAMES_CONST)) return (TRUE)
  if (IsMacro(element)) return (TRUE)
  if (!is.character(element)) return (TRUE)
  if (!is(element, "variable")) return (TRUE)
  if (elementName == "condition") {
    if (element %in% paste0("$", names(joint$Navigate(joint$downstream)$parameters))) return (TRUE)
  } else {
    if (element %in% paste0("$", names(joint$parameters))) return (TRUE)
  }
  return (FALSE)
}
