# GrMtnPower visualizations module
# Standard plotting templates
# Kristian Omland

library(lattice)

#' Set up lattice color palette for GMP plots
#'
#' @details
#' Uses colorblind-friendly palette and sets sensible lattice defaults.
setup_gmp_colors <- function() {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#D55E00", "#CC79A7")
  trellis.par.set(
    "superpose.line" = list(col = cbPalette),
    "superpose.symbol" = list(pch = 18, col = cbPalette)
  )
  return(invisible(cbPalette))
}

#' Plot daily generation and consumption
#'
#' @param datDaily data.table with dateForm, generated, consumed columns
#' @param title optional plot title
#' @return lattice xyplot object
plot_daily_summary <- function(datDaily, title = "Daily Generation and Consumption") {
  xyplot(generated + consumed ~ dateForm, datDaily,
         main = title,
         pch = 18, type = "b",
         xlab = "Date", ylab = "Energy (kWh)",
         auto.key = list(space = "right"))
}

#' Plot recent month's daily breakdown
#'
#' @param datDaily data.table with dateForm, consumed, returned, generated columns
#' @param nDays number of days to show (default: 30)
#' @return lattice xyplot with grid background
plot_last_n_days <- function(datDaily, nDays = 30) {
  dat_subset <- datDaily[dateForm >= Sys.Date() - nDays]
  
  xyplot(consumed + returned + generated ~ dateForm, dat_subset,
         type = "b", xlab = "", ylab = "Energy (kWh)",
         main = paste("Last", nDays, "Days"),
         auto.key = list(space = "right"),
         scales = list(x = list(at = dat_subset[wday(dateForm) == 1, dateForm])),
         panel = function(x, y, ...) {
           panel.abline(v = dat_subset[, dateForm], col = gray(0.9))
           panel.abline(v = dat_subset[wday(dateForm) == 1, dateForm], col = gray(0.6))
           panel.abline(h = 0:50, col = gray(0.9))
           panel.abline(h = seq(0, 50, 10), col = gray(0.6))
           panel.xyplot(x, y, ...)
         })
}

#' Plot past 12 months of generation and consumption
#'
#' @param datDaily data.table with dateForm, generated, consumed columns
#' @return lattice xyplot object
plot_past_year <- function(datDaily) {
  yearAgo <- as.IDate(sub(
    as.character(year(Sys.Date())),
    as.character(year(Sys.Date()) - 1),
    as.character(Sys.Date())
  ))
  
  xyplot(generated + consumed ~ dateForm,
         datDaily[dateForm >= yearAgo],
         pch = 18, type = "b",
         xlab = "Date", ylab = "Energy (kWh)",
         main = "Past 12 Months",
         auto.key = list(space = "right"))
}

#' Plot monthly aggregation faceted by year
#'
#' @param datDaily data.table with dateForm, generated, consumed columns
#' @param month month number (1-12)
#' @return lattice xyplot faceted by year
plot_monthly_by_year <- function(datDaily, month = 8) {
  xyplot(generated + consumed ~ dateForm | as.factor(year(dateForm)),
         datDaily[month(dateForm) == month],
         pch = 18, type = "b",
         xlab = "Date", ylab = "Energy (kWh)",
         main = paste("Month", month, "by Year"),
         scales = list(x = list(relation = "free", rot = 90)),
         as.table = TRUE, layout = c(6, 1),
         strip = FALSE, strip.left = TRUE,
         auto.key = list(space = "right"))
}

#' Plot weekly aggregation for specified ISO weeks
#'
#' @param datWeekly data.table with year, isoweek, generated, consumed columns
#' @param isoweekRange vector of length 2: c(start_week, end_week)
#' @param years optional vector of years to display (default: all in data)
#' @return lattice xyplot faceted by year
plot_weekly_by_year <- function(datWeekly, isoweekRange = c(14, 26), years = NULL) {
  dat_subset <- datWeekly[isoweek %between% isoweekRange]
  
  if (!is.null(years)) {
    dat_subset <- dat_subset[year %in% years]
  }
  
  xyplot(generated + consumed ~ isoweek | as.factor(year),
         dat_subset,
         pch = 18, type = "b",
         xlab = "ISO Week", ylab = "Energy (kWh)",
         main = paste("Weeks", isoweekRange[1], "-", isoweekRange[2]),
         as.table = TRUE, layout = c(6, 1),
         strip = FALSE, strip.left = TRUE,
         auto.key = list(space = "right"))
}

#' Plot rolling annual consumption ratio
#'
#' @param ratioData data.table with date, p (proportion) columns
#' @param refLine optional horizontal reference line (default: 0.65)
#' @return base R plot
plot_annual_consumption_ratio <- function(ratioData, refLine = 0.65) {
  plot(ratioData[, date], ratioData[, p],
       type = "l", xlab = "Date", ylab = "Proportion Consumed",
       main = "Rolling 365-Day Consumption Ratio",
       ylim = c(0, 1))
  if (!is.null(refLine)) {
    abline(h = refLine, col = "red", lty = 2, lwd = 2)
  }
  grid()
}

#' Plot solar generation as function of azimuth/zenith
#'
#' @param dat data.table with azimuth, zenith, generation columns
#' @param gamModel optional gam model object to overlay
#' @return lattice wireframe (perspective 3D plot)
#' @details
#' Filters to zenith <= 90 (sun above horizon) and reasonable azimuth range.
plot_solar_position_generation <- function(dat, gamModel = NULL) {
  dat_clean <- dat[zenith <= 90 & azimuth %between% c(135, 315) & !is.na(generation)]
  
  # Scatter plot
  p <- xyplot(generation ~ azimuth + zenith, dat_clean,
              pch = 16, cex = 0.6, alpha = 0.5,
              xlab = "Azimuth (deg)", ylab = "Zenith (deg)", zlab = "Generation (kW)",
              main = "Generation by Solar Position")
  
  return(p)
}

#' Plot hourly generation pattern by time of day
#'
#' @param dat data.table with hourly records, dateTime column
#' @return lattice plot faceted by solar season
plot_generation_by_hour <- function(dat) {
  dat_clean <- dat[!is.na(generation)]
  dat_clean[, hour_of_day := hour(dateTime)]
  dat_clean[, solar_season := cut(yday(dateTime), 
                                   breaks = c(0, 80, 172, 266, 355, 366),
                                   labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"))]
  
  xyplot(generation ~ hour_of_day | solar_season, dat_clean,
         type = "l", xlab = "Hour of Day", ylab = "Generation (kW)",
         main = "Generation Pattern by Hour and Season",
         as.table = TRUE,
         scales = list(y = list(relation = "free")))
}
