
#' @importFrom data.tree ToDataFrameTree
#' @export
print.context <- function(x, ...) {
  df <- ToDataFrameTree(x, pruneFun = function(node) {
    !identical(node$parent$type, "tap")
  })
  print(df)
}





