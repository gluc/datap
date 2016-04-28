


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


PruneErrorReport <- function(tree, errorReportType) {
  tree$Do(function(joint) joint$hasErrors <- Aggregate(joint,
                                                       function(usj) {
                                                         if (length(usj$hasErrors) > 0) return(usj$hasErrors)
                                                         if (!usj$isLeaf) return (NULL)
                                                         return (usj$parent$name == ".errors" ||
                                                                   (usj$level > 2 && usj$parent$parent$name == ".errors") ||
                                                                   (usj$level > 3 && usj$parent$parent$parent$name == ".errors")   )

                                                       },
                                                       aggFun = any),
          traversal = "post-order")


  tree$Prune(pruneFun = function(joint) joint$hasErrors)


  if (!tree$hasErrors) {
    tree$code <- "0000"
    tree$message <- "No errors"
    tree$errorCount <- 0
  } else {
    errCount <- Aggregate(tree, function(joint) {
      if (joint$name == ".errors") return (joint$leafCount)
      if (joint$isLeaf) return (0)
      return (NULL)
    }, aggFun = sum)
    tree$message <- paste(errCount, "reference errors!")
    tree$errorCount <- errCount
  }

  tree$name <- paste("context", errorReportType, "error report")

  class(tree) <- c("dataperrorreport", "Node", "R6")



}
