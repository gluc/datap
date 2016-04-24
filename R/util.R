
#' @importFrom data.tree ToDataFrameTree
#' @export
print.context <- function(x, ...) {
  df <- ToDataFrameTree(x, pruneFun = function(node) {
    !identical(node$parent$type, "tap")
  })
  print(df)
}


#' Alias for \code{Sys.Date()}
#'
#' @export
Today <- function() Sys.Date()




do.call.intrnl <- function(what, args) {
  if(is.character(what)) {
    fn <- strsplit(what, "::")[[1]]
    if(length(fn)==1) {
      fun <- fn[[1]]
      envir <- parent.frame()
    } else {
      fun <- fn[[2]]
      envir <- asNamespace(fn[[1]])
    }
  }

  do.call(fun, as.list(args), envir = envir)
}
