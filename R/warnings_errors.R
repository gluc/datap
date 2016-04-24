#'Check the ratio of NA values compared to non-NA values.
#'
#'
#'@param timeseries the timeseries object (can be mostly any class)
#'@param variable the name of the column
#'@param maxRatio the maximum ratio
#'
#'
#'@export
NaRatio <- function(timeseries, variable, maxRatio) {
  naRatio <- length(which(is.na(timeseries[ , variable]))) / length(timeseries)
  res <- naRatio <= maxRatio
  text <- paste0("NA ratio: ", naRatio, " (max: ", maxRatio, ")")
  res <- list(ok = res, msg = text)
  return (res)
}
