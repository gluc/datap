#' @importFrom data.tree ToDataFrameTree
#' @export
print.context <- function(x, ...) {
  df <- ToDataFrameTree(x, pruneFun = function(node) {
    !identical(node$parent$type, "tap")
  })
  print(df)
}


#' @export
print.tap <- function(x, ...) {
  NextMethod(x,
             NULL, "type",
             downstream = function(n) paste(n$downstream, collapse = "/")
  )
}


#' @export
print.dataperrorreport <- function(x, ...) {
  NextMethod(x,
             NULL, "type", "errorCount", "code", "message",
             pruneFun = function(joint) joint$`.hasErrors`
             )
}
