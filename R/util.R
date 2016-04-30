


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



AssertSyntax <- function(condition, joint, errorSection, errorSubsection, errorSubsubsections, errorCode, ...) {
  #if (joint$name == "doA") browser()
  if (!condition) {
    msg <- list(...)
    msg <- paste0(msg, collapse = "")
    if (!".errors" %in% names(joint$children)) {
      e <- joint$AddChild(".errors")
      e$type <- "error"
    }
    err <- joint$`.errors`
    if (nchar(errorSection) > 0) {
      if (!errorSection %in% names(err$children)) err$AddChild(errorSection)
      err <- err[[errorSection]]
    }
    if (nchar(errorSubsection) > 0) {
      if (!errorSubsection %in% names(err$children)) err$AddChild(errorSubsection)
      err <- err[[errorSubsection]]
    }
    err$code <- as.character(errorCode)
    err$message <- msg

    if (!is.null(errorSubsubsections)) for (i in 1:length(errorSubsubsections)) err$AddChild(i, message = errorSubsubsections[[i]])

  }
  return (condition)
}


FindErrors <- function(tree) {
  tree$Do(function(joint) joint$`.hasErrors` <- Aggregate(joint,
                                                       function(usj) {
                                                         if (usj$name == ".errors" ||
                                                             usj$parent$name == ".errors" ||
                                                             (usj$level > 2 && usj$parent$parent$name == ".errors") ||
                                                             (usj$level > 3 && usj$parent$parent$parent$name == ".errors")   ) return (TRUE)
                                                         if (usj$isLeaf) return (FALSE)
                                                         return (NULL)

                                                       },
                                                       aggFun = any),
          traversal = "post-order")

  invisible (tree$`.hasErrors`)

}


EnrichErrorReport <- function(tree, errorReportType) {


  if (!tree$`.hasErrors`) {
    tree$code <- "0000"
    tree$message <- "No errors"
    tree$errorCount <- 0
  } else {
    errCount <- Traverse(tree, filterFun = function(joint) identical(joint$type, "error")) %>% length
    tree$message <- paste(errCount, "reference errors!")
    tree$errorCount <- errCount
  }

  tree$name <- paste("context", errorReportType, "error report")

  class(tree) <- c("dataperrorreport", "Node", "R6")



}
