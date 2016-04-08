
#' Create a timeseries of 1
#'
#'
#' @inheritParams ConstantReturn
#' @export
Ones <- function(startDate = "1990-01-01", endDate = Sys.Date()) {
  orderby <- seq(as.Date(startDate), as.Date(endDate), 1)
  xts(rep(1, length(orderby)), orderby)
}



#' Create a daily timeseries with a constant yearly return.
#'
#' @param startDate the first date of the xts to be returned
#' @param endDate the last date of the xts to be returned
#' @param r the yearly return
#' @param startValue the first value of the xts
#' @param dayCountConvention the convention to be used
#' @param holiday for convention \code{holidays}, a \code{timeDate} holiday calendar
#'
#' @return an \code{xts} object
#'
#' @importFrom timeDate timeSequence isWeekday
#' @import magrittr
#' @export
ConstantReturn <- function(startDate = "1990-01-01",
                           endDate = Sys.Date(),
                           r,
                           startValue = 1,
                           dayCountConvention = c("weekdays", "calendardays", "holidays"),
                           holiday = NULL) {

  dayCountConvention <- dayCountConvention[1]
  endDate <- as.Date(endDate)
  if (dayCountConvention == "weekdays") {
    dailyReturn <- expm1(1/365 * log1p(r))
    orderby <- seq(as.Date(startDate), endDate, 1)

  } else if (dayCountConvention == "calendardays") {
    dailyReturn <- expm1(1/252 * log1p(r))
    # A ’timeDate’ Sequence
    tS <- timeSequence(as.Date(startDate), endDate)
    # Subset weekdays
    tS[isWeekday(tS)] %>% as.Date -> orderby

  } else if (dayCountConvention == "holiday") {
    stop("Not yet implementede!")
  }

  res <- xts(startValue * exp(dailyReturn * (1:length(orderby) - 1)), orderby)
  return (res)
}


#' Generate an index in which each component has a fixed weight.
#'
#' @export
FixedWeightIndex <- function(weights, children) {

}


#' Regularize the index of a daily xts by adding NAs to
#' non-existent days
#'
#' @param xts the xts object to regularize
#'
#' @examples
#' library(Quandl)
#' library(magrittr)
#' Quandl("GOOG/VTX_UBSN", type = "xts", force_irregular = TRUE) %>%
#' Regularize %>% na.locf -> ubs
#'
#'
#' @export
Regularize <- function(xts) {

  idx <- index(xts)
  idx2 <- seq.Date(idx[1], tail(idx, 1), by = 1)
  xts2 <- xts(order.by = idx2)
  xts3 <- merge(xts, xts2)
  return(xts3)
}


#' Combines two xts objects with similar data,
#' giving preference to objects first in the list.
#'
#' @param ... xts objects to combine
#'
#' @examples
#' library(Quandl)
#' ubs1 <- Quandl("GOOG/VTX_UBSN", type = "xts")
#' ubs2 <- ubs1
#' ubs3 <- ubs1
#'
#' #assume ubs1 and ubs2 and ubs3 come from different
#' #data sources, each of which may have missing data.
#' #We simulate by messing with the data:
#'
#' ubs1 <- ubs1[-c(3,5), ]
#' ubs2 <- ubs2[-3, ]
#' ubs3[1, 1] <- 58
#'
#' #Source 1 is considered best, but sometimes
#' #it may have missing data which is available in 2
#' #or 3.
#' ubs <- Combine(list(ubs1, ubs2, ubs3))
#'
#' @export
Combine <- function(listofxts) {
  #listofxts <- list(...)
  xts <- listofxts[[length(listofxts)]]
  for (i in (length(listofxts) - 1):1) {
    xts <- CombinePairs(listofxts[[i]], xts)
  }
  return (xts)
}


CombinePairs <- function(xts1, xts2) {

  xts <- merge(xts1, xts2)
  for (col in colnames(xts)[1:ncol(xts1)]) xts[is.na(xts[ , col]), col] <- xts[is.na(xts[ , col]), paste0(col, ".1")]
  xts <- xts[, colnames(xts1)]

  return (xts)
}


MinLength <- function(timeseries, minLength) {

  res <- length(timeseries) >= minLength
  text <- paste0("Length: ", length(timeseries), " (max: ", minLength, ")")
  res <- list(ok = res, msg = text)

  return (res)
}


#'@export
SetNames <- function(x, names) {
  names(x) <- names
  return (x)
}



#'@export
NaRatio <- function(timeseries, variable, maxRatio) {
  naRatio <- length(which(is.na(timeseries[ , variable]))) / length(timeseries)
  res <- naRatio <= maxRatio
  text <- paste0("NA ratio: ", naRatio, " (max: ", maxRatio, ")")
  res <- list(ok = res, msg = text)
  return (res)
}



#' @importFrom memoise timeout memoise
#' @export
Cache <- function(f, timeoutSec) {
  return ( memoise(f, ~timeout(timeoutSec)) )
}

