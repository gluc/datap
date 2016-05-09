
#' Data aquisition and pre-processing using datap contexts
#'
#' datap is a DSL to acquire and pre-process data from various sources.
#' datapR is a datap interpreter for R.
#'
#' @examples
#' library(datapR)
#' filePath <- system.file("extdata", "context1.yaml", package="datapR")
#' context <- Load(filePath)
#' context
#' context$`Closing Prices`$Indices$SPX
#' plot(context$`Closing Prices`$Indices$SPX)
#' context$`Closing Prices`$Indices$SPX$tap()
#' context$`Closing Prices`$Indices$SPX$tap(dteRange = "/")
#'
#'
#' @docType package
#' @name datapR
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
