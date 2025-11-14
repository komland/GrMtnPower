# GrMtnPower data-processing module
# Standardized data transformations
# Kristian Omland

#' Add standardized date/time columns
#'
#' @param dat data.table with 'date' (ISO string) column
#' @param tz timezone (default: "America/New_York")
#' @return data.table with new columns: dateTimeUTC, dateForm
add_datetime_columns <- function(dat, tz = "America/New_York") {
  dat[, dateTime := as.POSIXct(sub("Z", "", sub("T", " ", date)), tz = tz)]
  dat[, dateForm := as.IDate(date)]
  return(dat)
}

#' Add solar year column
#'
#' @param dat data.table with 'dateForm' column
#' @details
#' Solar year resets in November (winter solstice orientation).
#' Nov/Dec of year Y is considered part of solar year Y+1.
#' @return data.table with new column: solarYear
add_solar_year <- function(dat) {
  dat[, solarYear := fifelse(month(dateForm) %in% c(11, 12),
                              year(dateForm) + 1,
                              year(dateForm))]
  return(dat)
}

#' Mark periods with battery storage system
#'
#' @param dat data.table with 'dateForm' column
#' @param startDate IDate or character ("YYYY-MM-DD") for BEV system install
#' @param endDate IDate or character ("YYYY-MM-DD") for BEV system removal/end of analysis
#' @return data.table with new logical column: bev (TRUE during BEV period)
add_bev_flag <- function(dat, startDate = "2016-06-01", endDate = "2019-05-31") {
  startDate <- as.IDate(startDate)
  endDate <- as.IDate(endDate)
  dat[, bev := dateForm %between% c(startDate, endDate)]
  return(dat)
}

#' Compute derived consumption columns
#'
#' @param dat data.table with 'generation', 'returnedGeneration', 'consumedFromGrid' columns
#' @return data.table with new columns: consumedGeneration, totalConsumed
add_consumption_derived <- function(dat) {
  dat[, consumedGeneration := generation - returnedGeneration]
  dat[, totalConsumed := consumedFromGrid + consumedGeneration]
  return(dat)
}

#' Add solar position columns (azimuth, zenith)
#'
#' @param dat data.table with 'dateTime' (POSIXct) column
#' @param lon longitude of observation site (default: Green Mountain Power location)
#' @param lat latitude of observation site (default: Green Mountain Power location)
#' @return data.table with new columns: JD, zenith, azimuth (for non-NA generation rows)
#' @details
#' Requires solarPos package with solarPosition() and julianDay() functions.
#' Only adds columns for rows with non-NA generation (PV system data).
add_solar_position <- function(dat, lon = -72.979348, lat = 44.468674) {
  # Filter to rows with generation data
  subdat <- dat[!is.na(generation), .(dateTime, generation)]
  
  # Compute Julian Day
  subdat[, JD := julianDay(
    year(dateTime),
    month(dateTime),
    mday(dateTime),
    hour(dateTime)
  )]
  
  # Get solar position
  sunPos <- solarPosition(jd = subdat[, JD], lon = lon, lat = lat)
  
  # Add to subdat
  subdat[, `:=`(
    zenith = sunPos[, 1],
    azimuth = sunPos[, 2]
  )]
  
  # Merge back to full dataset (will have NA for non-generation rows)
  dat <- dat[subdat, on = "dateTime", `:=`(
    JD = i.JD,
    zenith = i.zenith,
    azimuth = i.azimuth
  )]
  
  return(dat)
}

#' Aggregate data to daily totals
#'
#' @param dat data.table with hourly data and 'dateForm', 'generation', 'consumedTotal' columns
#' @return data.table aggregated by date
aggregate_daily <- function(dat) {
  return(dat[, .(
    generated = sum(generation, na.rm = TRUE),
    consumed = sum(consumedTotal, na.rm = TRUE),
    returned = sum(returnedGeneration, na.rm = TRUE),
    solarYear = max(solarYear, na.rm = TRUE),
    bev = max(bev, na.rm = TRUE)
  ), dateForm])
}

#' Aggregate data to weekly totals
#'
#' @param dat data.table with daily or hourly data and 'dateForm', 'generation', 'consumedTotal' columns
#' @param minObs minimum number of hourly observations per week to include (default: 167 = 7 days * 24 hrs - some tolerance)
#' @return data.table aggregated by year and ISO week
aggregate_weekly <- function(dat, minObs = 167) {
  weekly <- dat[, .(
    nObs = .N,
    generated = sum(generation, na.rm = TRUE),
    consumed = sum(consumedTotal, na.rm = TRUE),
    returned = sum(returnedGeneration, na.rm = TRUE),
    solarYear = max(solarYear, na.rm = TRUE),
    bev = max(bev, na.rm = TRUE)
  ), .(year(dateForm), isoweek(dateForm))]
  
  names(weekly)[1:2] <- c("year", "isoweek")
  
  return(weekly[nObs >= minObs])
}

#' Compute rolling 365-day consumption/generation ratio
#'
#' @param dat data.table with daily aggregation: dateForm, generated, consumed columns
#' @param startDate IDate or character start of rolling period
#' @param endDate IDate or character end of rolling period
#' @return data.table with columns: date, p (proportion consumed)
rolling_annual_consumption_ratio <- function(dat, startDate = NULL, endDate = NULL) {
  # Default: from end of BEV period to reasonable recent date
  if (is.null(startDate)) startDate <- as.IDate("2020-09-14")
  if (is.null(endDate)) endDate <- Sys.Date()
  
  startDate <- as.IDate(startDate)
  endDate <- as.IDate(endDate)
  
  stDateSeq <- seq(startDate, endDate, "day")
  results <- NULL
  
  for (focalD in stDateSeq) {
    windowDat <- dat[dateForm >= focalD & dateForm < focalD + 365]
    
    if (nrow(windowDat) == 0) next
    
    ratio <- windowDat[, sum(consumed) / sum(generated)]
    
    y <- data.table(date = focalD, p = ratio)
    results <- rbind(results, y)
  }
  
  return(results)
}

#' Compute last N days of data
#'
#' @param dat data.table with 'dateTime' POSIXct column
#' @param nDays number of days to retrieve (default: 30)
#' @return data.table subset to last nDays
get_last_n_days <- function(dat, nDays = 30) {
  maxTime <- dat[, max(dateTime)]
  cutoffTime <- maxTime - (nDays * 24 * 60 * 60)
  return(dat[dateTime >= cutoffTime])
}

#' Apply all standard transformations to raw API data
#'
#' @param dat data.table from API with columns: date, consumed, generation, returnedGeneration, consumedTotal
#' @param addSolarPos logical: compute solar position (requires solarPos package)
#' @return data.table with all standardized columns added
apply_standard_transformations <- function(dat, addSolarPos = FALSE) {
  dat <- add_datetime_columns(dat)
  dat <- add_solar_year(dat)
  dat <- add_bev_flag(dat)
  dat <- add_consumption_derived(dat)
  
  if (addSolarPos) {
    dat <- add_solar_position(dat)
  }
  
  return(dat)
}
