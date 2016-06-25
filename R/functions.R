
#' Create a timeseries of 1
#'
#' @param startDate the start date
#' @param endDate the end date
#' @param colname the name the series should get
#'
#' @inheritParams ConstantReturn
#' @export
Ones <- function(startDate = "1990-01-01", endDate = Sys.Date(), colname = "Close") {
  orderby <- seq(as.Date(startDate), as.Date(endDate), 1)
  res <- xts(rep(1, length(orderby)), orderby)
  colnames(res) <- colname
  return (res)
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
#' @param weights the weights of each component
#' @param constituents the constituent historic prices
#'
FixedWeightIndex <- function(weights, constituents) {

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
#' @importFrom xts xts
#' @importFrom zoo index
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
#' @param listofxts xts objects to combine
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
  text <- paste0("Length: ", length(timeseries), " (min: ", minLength, ")")
  res <- list(ok = res, msg = text)

  return (res)
}


#' Set the names to an object
#'
#' @param x the object on which to set the names
#' @param names the names
#'
#' @export
SetNames <- function(x, names) {
  names(x) <- names
  return (x)
}




#' Cache / memoise a function for some time
#'
#' @param joint the joint whose upstream joint function should be cached
#' @param timeoutSec the number of seconds ot cache
#'
#' @importFrom memoise timeout memoise
#' @export
Cache <- function(joint, timeoutSec) {
  us <- joint$upstream
  if (length(us) == 0) stop(paste0("Cannot cache ", joint$name, "! It has no upstream joint."))
  if (length(us) > 1) stop(paste0("Cannot cache ", joint$name, "! It has ", length(us), " upstream joints."))
  f <- us[[1]]$tap
  return ( memoise(f, ~timeout(timeoutSec)) )
}

#' Forget memoised/cached function
#'
#' @param joint the joint whose downstream joint function should be forgotten
#' @param inflow a value passed as a result
#'
#' @importFrom memoise forget
#' @export
ForgetCache <- function(joint, inflow) {
  ds <- joint$Navigate(joint$downstream)
  f <- ds$tap
  forget(f)
  invisible (inflow)
}




GetJointName <- function(joint) joint$name

