# GrMtnPower solar modeling and analysis
# Advanced analysis: solar generation patterns, azimuth/zenith relationships
# Kristian Omland

source("R/utilities.r")
source("R/data-processing.r")
source("R/visualizations.r")
source("R/modeling.r")

# Set up colors and load data
setup_gmp_colors()
dat <- load_latest_data()
dat <- apply_standard_transformations(dat, addSolarPos = TRUE)

cat("=== SOLAR GENERATION ANALYSIS ===\n\n")

# Fit solar position model
cat("Fitting solar position model (generation ~ s(azimuth, zenith))...\n")
fm_solar <- fit_solar_generation_model(dat)
cat("Deviance explained:", round(summary(fm_solar)$dev.expl * 100, 2), "%\n\n")

# Get solar position statistics
cat("Computing solar position statistics...\n")
solar_stats <- solar_position_statistics(dat, aziBins = 8, zenBins = 6)

# Hourly pattern statistics
cat("Computing generation by hour of day...\n")
hourly_stats <- generation_by_hour_statistics(dat)

# Seasonal pattern statistics
cat("Computing generation by season...\n")
seasonal_stats <- generation_by_season_statistics(dat, dayBins = 30)

# Predict on regular grid for publication
pred_grid <- predict_solar_generation_grid(fm_solar)

cat("\n=== LAST 30 DAYS DETAILED BREAKDOWN ===\n")
lastMonth <- get_last_n_days(dat, nDays = 30)
datLastMonth <- aggregate_daily(lastMonth)
print(plot_last_n_days(datLastMonth, nDays = 30))

cat("\n=== SOLAR ANALYSIS COMPLETE ===\n")
cat("Models and statistics available:\n")
cat("  fm_solar: GAM model for azimuth/zenith\n")
cat("  solar_stats: Position-binned generation statistics\n")
cat("  hourly_stats: Generation by hour of day\n")
cat("  seasonal_stats: Generation by season\n")
cat("  pred_grid: Predicted generation surface\n")
