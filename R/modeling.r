# GrMtnPower modeling module
# Solar generation analysis and GAM models
# Kristian Omland

library(mgcv)

#' Fit GAM model for generation based on solar position
#'
#' @param dat data.table with azimuth, zenith, generation columns
#' @param maxZenith maximum zenith angle to include (sun above horizon, default: 90)
#' @param aziRange vector c(min, max) azimuth range to include
#' @return gam model object
#' @details
#' Fits smooth surface: generation ~ s(azimuth, zenith)
fit_solar_generation_model <- function(dat, maxZenith = 90,
                                       aziRange = c(135, 315)) {
  dat_clean <- dat[zenith <= maxZenith & azimuth %between% aziRange & !is.na(generation)]
  
  model <- gam(
    generation ~ s(azimuth, zenith),
    data = dat_clean,
    family = gaussian(link = "identity")
  )
  
  return(model)
}

#' Fit GAM model for generation by time of day and solar season
#'
#' @param dat data.table with dateTime POSIXct, generation columns
#' @return gam model object
#' @details
#' Fits: generation ~ s(timeOfDay, within = 24h) + s(dayOfYear, within = 365d)
fit_temporal_generation_model <- function(dat) {
  dat_clean <- dat[!is.na(generation)]
  
  # Compute cyclic time features
  dat_clean[, hour_frac := (hour(dateTime) + minute(dateTime)/60) / 24]
  dat_clean[, day_frac := (yday(dateTime) - 1) / 365]
  
  model <- gam(
    generation ~ s(hour_frac, bs = "cc", k = 8) + s(day_frac, bs = "cc", k = 12),
    data = dat_clean,
    family = gaussian(link = "identity"),
    knots = list(hour_frac = c(0, 1), day_frac = c(0, 1))
  )
  
  return(model)
}

#' Extract generation statistics by solar position bins
#'
#' @param dat data.table with azimuth, zenith, generation columns
#' @param aziBins number of azimuth bins (default: 8)
#' @param zenBins number of zenith bins (default: 6)
#' @return data.table with bin statistics: median, mean, q25, q75 generation
solar_position_statistics <- function(dat, aziBins = 8, zenBins = 6) {
  dat_clean <- dat[zenith <= 90 & azimuth %between% c(135, 315) & !is.na(generation)]
  
  # Create bins
  dat_clean[, azi_bin := cut(azimuth, breaks = aziBins, include.lowest = TRUE)]
  dat_clean[, zen_bin := cut(zenith, breaks = zenBins, include.lowest = TRUE)]
  
  # Aggregate
  stats <- dat_clean[, .(
    n = .N,
    med_gen = median(generation, na.rm = TRUE),
    mean_gen = mean(generation, na.rm = TRUE),
    q25_gen = quantile(generation, 0.25, na.rm = TRUE),
    q75_gen = quantile(generation, 0.75, na.rm = TRUE),
    sd_gen = sd(generation, na.rm = TRUE)
  ), .(azi_bin, zen_bin)]
  
  return(stats)
}

#' Compute predicted generation grid from solar position model
#'
#' @param model gam model from fit_solar_generation_model()
#' @param aziSeq sequence of azimuth values (default: seq(135, 315, 5))
#' @param zenSeq sequence of zenith values (default: seq(0, 90, 5))
#' @return data.table with grid and predictions
predict_solar_generation_grid <- function(model, 
                                          aziSeq = seq(135, 315, 5),
                                          zenSeq = seq(0, 90, 5)) {
  # Create prediction grid
  grid <- expand.grid(azimuth = aziSeq, zenith = zenSeq)
  grid <- as.data.table(grid)
  
  # Predict
  grid[, generation := predict(model, newdata = grid)]
  
  return(grid)
}

#' Summarize generation statistics by hour of day
#'
#' @param dat data.table with hourly records and dateTime POSIXct column
#' @return data.table with hour, median, mean, q25, q75 generation
generation_by_hour_statistics <- function(dat) {
  dat_clean <- dat[!is.na(generation)]
  dat_clean[, hour_of_day := hour(dateTime)]
  
  stats <- dat_clean[, .(
    n = .N,
    med_gen = median(generation, na.rm = TRUE),
    mean_gen = mean(generation, na.rm = TRUE),
    q25_gen = quantile(generation, 0.25, na.rm = TRUE),
    q75_gen = quantile(generation, 0.75, na.rm = TRUE),
    sd_gen = sd(generation, na.rm = TRUE)
  ), hour_of_day]
  
  setorder(stats, hour_of_day)
  return(stats)
}

#' Summarize generation statistics by day of year
#'
#' @param dat data.table with hourly records and dateTime POSIXct column
#' @param dayBins binning for days (default: 30 = monthly)
#' @return data.table with day_bin, median, mean generation
generation_by_season_statistics <- function(dat, dayBins = 30) {
  dat_clean <- dat[!is.na(generation)]
  dat_clean[, day_of_year := yday(dateTime)]
  dat_clean[, season_bin := cut(day_of_year, breaks = seq(1, 366, dayBins))]
  
  stats <- dat_clean[, .(
    n = .N,
    med_gen = median(generation, na.rm = TRUE),
    mean_gen = mean(generation, na.rm = TRUE),
    q25_gen = quantile(generation, 0.25, na.rm = TRUE),
    q75_gen = quantile(generation, 0.75, na.rm = TRUE),
    sd_gen = sd(generation, na.rm = TRUE)
  ), season_bin]
  
  return(stats)
}

#' Generate comprehensive model summary report
#'
#' @param model gam model object
#' @param data_used data.table used to fit model
#' @return list with summary statistics and diagnostics
model_summary_report <- function(model, data_used) {
  return(list(
    model = model,
    summary = summary(model),
    gam_check_output = capture.output(gam.check(model)),
    data_rows = nrow(data_used),
    deviance_explained = paste0(round(summary(model)$dev.expl * 100, 2), "%")
  ))
}
