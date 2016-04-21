CheckReferences <- function(tree) {
  tree$Do(CheckReferencesJoint, filterFun = isNotRoot)
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

}


CheckReferencesElements <- function(joint, elementName) {
  elements <- joint[[elementName]]
  for (element in elements) {
    AssertSyntax(CheckReferencesElement(joint, element),
                 joint,
                 "references",
                 element,
                 2000,
                 "Reference ", element, " cannot be resolved."
    )

  }
}


CheckReferencesElement <- function(joint, element) {
  if (length(element) == 0) return (TRUE)
  if (length(element) > 1) return (TRUE)
  if (element %in% paste0("@", VARIABLE_RESERVED_NAMES_CONST)) return (TRUE)
  if (IsMacro(element)) return (TRUE)
  if (!is.character(element)) return (TRUE)
  if (!identical(substr(element, 1, 1), "@")) return (TRUE)
  if (element %in% paste0("@", names(joint$parameters))) return (TRUE)
  return (FALSE)
}
