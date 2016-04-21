CheckReferences <- function(tree) {
  tree$Do(CheckReferencesJoint, filterFun = isNotRoot)
}


CheckReferencesJoint <- function(joint) {
  #only reserved references, parameters, or macros allowed

}
