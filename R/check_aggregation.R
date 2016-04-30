#' Loads a context definition and checks its syntax.
#'
#' @param con a connection containing the meta data
#'
#' @examples
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' errors <- CheckAggregation(filePath)
#' print(errors)
#'
#' @importFrom yaml yaml.load
#' @importFrom data.tree Aggregate
#' @export
CheckAggregation <- function(con) {
  yamlString <- paste0(readLines(con), collapse = "\n")
  lol <- yaml.load(yamlString)

  tree <- CreateRawTree(lol)

  errors <- CheckSyntaxRawTree(tree)
  if (errors$`.hasErrors`) {
    stop("Context contains syntax errors! Run CheckSyntax(con) to get an error report.")
  }

  ResolveFlow(tree)

  tree <- CheckAggregationTree(tree)
  return (tree)

}


CheckAggregationTree <- function(tree) {
  tree <- Clone(tree)

  tree$Do(CheckSingleDownstream, filterFun = function(joint) !is.null(joint$type) && joint$type %in% JOINT_TYPES_FUN)

  FindErrors(tree)
  EnrichErrorReport(tree, "aggregation")

  return (tree)
}


CheckSingleDownstream <- function(joint) {


  st <- AssertSyntax(length(joint$downstream) > 0,
               joint,
               "aggregation",
               "downstream",
               joint$downstream,
               "4000",
               "No downstream connection found for '", joint$name, "'."
  )

  if (!st) return(st)

  AssertSyntax(length(joint$downstream) == 1,
               joint,
               "aggregation",
               "downstream",
               joint$downstream,
               "4000",
               "Cannot connect '", joint$name, "' to more than one downstream joints."
               )

}


