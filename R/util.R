
#' @importFrom data.tree ToDataFrameTree
#' @export
PrintTaps <- function(context) {
  df <- ToDataFrameTree(context, pruneFun = function(node) {
    !identical(node$parent$type, "tap")
  })
  print(df)
}




#' Get data from a tap
#'
#' @param tapPath the name of the tap
#' @param context the context object
#' @param ... any parameters to be passed on to the tap
#'
#' @importFrom data.tree Climb
#' @export
Tap <- function(context, tapPath, ...) {
  tap <- do.call(Climb, strsplit(tapPath, "/", fixed = TRUE)[[1]] %>% as.list %>% c(context, .))
  tap$tap(...)
}
