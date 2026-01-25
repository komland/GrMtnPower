# GrMtnPower loss model visualizations
# Diagnostic plots for Y and q models
# Run this after analyze-loss.r
# Kristian Omland

# Assumes the following objects exist in workspace:
# - model_Y0: fitted GAM model for capacity envelope
# - dat_model: data with Y0 and q columns
# - dat_loss: subset with valid q values

source("R/modeling.r")

library(lattice)
library(data.table)
library(lubridate)
library(mgcv)

cat("=== GENERATING DIAGNOSTIC PLOTS ===\n\n")

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Plot 1: Observed vs Potential Generation
cat("1. Observed vs. Potential Generation (Y0)...\n")
png("plots/01_observed_vs_Y0.png", width = 800, height = 600)
print(plot_Y_vs_observed(dat_model, sample_frac = 0.2))
dev.off()

# Plot 2: Y0 surface over solar position
cat("2. Potential Generation surface Y0(azimuth, zenith)...\n")
png("plots/02_Y0_surface.png", width = 800, height = 600)
print(plot_Y_surface(model_Y0, n_grid = 40))
dev.off()

# Plot 3: Distribution of q
cat("3. Distribution of loss parameter q...\n")
png("plots/03_q_distribution.png", width = 800, height = 600)
plot_q_distribution(dat_loss)
dev.off()

# Plot 4: Loss over time (check for degradation trends)
cat("4. Loss over time (monthly aggregation)...\n")
png("plots/04_q_over_time.png", width = 800, height = 600)
print(plot_q_over_time(dat_loss, aggregate_by = "month"))
dev.off()

# Plot 5: GAM diagnostic plots
cat("5. GAM model diagnostics...\n")
png("plots/05_gam_diagnostics.png", width = 1000, height = 1000)
par(mfrow = c(2, 2))
gam.check(model_Y0)
par(mfrow = c(1, 1))
dev.off()

# Plot 6: Residuals vs solar position
cat("6. Residuals by solar position...\n")
dat_model[Y0 > 0 & !is.na(generation), residual := generation - Y0]

sample_dat <- dat_model[!is.na(residual)][sample(.N, min(.N, 5000))]

png("plots/06_residuals_by_position.png", width = 1000, height = 800)
print(xyplot(residual ~ zenith | cut(azimuth, breaks = 4), sample_dat,
             pch = 16, cex = 0.3, alpha = 0.3,
             xlab = "Zenith (degrees)",
             ylab = "Residual (Observed - Y0)",
             main = "Residuals by Solar Position",
             panel = function(x, y, ...) {
               panel.abline(h = 0, col = "red", lwd = 2)
               panel.xyplot(x, y, ...)
             }))
dev.off()

# Plot 7: Capacity factor by year (control chart)
cat("7. Capacity factor by year (control chart)...\n")
png("plots/07_capacity_factor_by_year.png", width = 800, height = 600)

# Calculate annual capacity factors for complete years
annual_cf <- annual_capacity[hours_total > 8000, .(
  solarYear,
  capacity_factor = total_gen_kWh / total_Y0_kWh
)]

# Calculate control chart statistics
cf_median <- median(annual_cf$capacity_factor)
cf_q1 <- quantile(annual_cf$capacity_factor, 0.25)
cf_q3 <- quantile(annual_cf$capacity_factor, 0.75)
cf_iqr <- cf_q3 - cf_q1
cf_lower <- cf_median - 1.5 * cf_iqr
cf_upper <- cf_median + 1.5 * cf_iqr

# Set y-axis limits to median +/- 1.6*IQR or slightly beyond data range
ylim_lower <- min(cf_median - 1.6 * cf_iqr, min(annual_cf$capacity_factor) - 0.01)
ylim_upper <- max(cf_median + 1.6 * cf_iqr, max(annual_cf$capacity_factor) + 0.01)

# Create control chart (black and white)
plot(annual_cf$solarYear, annual_cf$capacity_factor,
     type = "b", pch = 19, col = "black",
     xlab = "Solar Year", ylab = "Capacity Factor",
     main = "Annual Capacity Factor Control Chart",
     ylim = c(ylim_lower, ylim_upper))

# Add control lines
abline(h = cf_median, lty = 2, lwd = 2)           # Median (dashed)
abline(h = cf_lower, lty = 3, lwd = 1)            # Lower control limit (dotted)
abline(h = cf_upper, lty = 3, lwd = 1)            # Upper control limit (dotted)

grid()
dev.off()

# Plot 8: Capacity factor by week of year (three-panel visualization)
cat("8. Capacity factor by week of year (three panels)...\n")
png("plots/08_capacity_factor_by_week.png", width = 1200, height = 900)
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))

# Calculate weekly summaries
weekly_cf <- dat_model[Y0 > 0, .(
  week_y0 = sum(Y0, na.rm = TRUE),
  week_gen = sum(generation, na.rm = TRUE),
  year = year(dateTime),
  isoweek = isoweek(dateTime)
), by = .(year = year(dateTime), isoweek = isoweek(dateTime))]

# Filter to complete years
complete_year_range <- annual_capacity[hours_total > 8000, range(solarYear)]
weekly_cf <- weekly_cf[year >= complete_year_range[1] & year <= complete_year_range[2]]

# Calculate capacity factor
weekly_cf[, week_cf := week_gen / week_y0]

# Filter to weeks 2-51 to avoid ISO week boundary weirdness
weekly_cf_filtered <- weekly_cf[isoweek >= 2 & isoweek <= 51]

# Top panel: Maximum Y0 by week (line plot)
y0_max_by_week <- weekly_cf_filtered[, .(max_y0 = max(week_y0)), by = isoweek]
setorder(y0_max_by_week, isoweek)

plot(y0_max_by_week$isoweek, y0_max_by_week$max_y0,
     type = "l", lwd = 2, col = "black",
     xlab = "", ylab = "kWh",
     main = "Capacity by Week",
     xaxt = "n", las = 1)
grid()

# Middle panel: Capacity factor by week (boxplot)
boxplot(week_cf ~ isoweek, data = weekly_cf_filtered,
        xlab = "", ylab = "p",
        main = "Capacity Factor by Week",
        col = "lightgray", border = "black",
        xaxt = "n", las = 1)
grid()

# Bottom panel: Actual generation by week (boxplot)
par(mar = c(4, 4, 2, 1))
boxplot(week_gen ~ isoweek, data = weekly_cf_filtered,
        xlab = "ISO Week of Year", ylab = "kWh",
        main = "Actual Generation by Week",
        col = "lightgray", border = "black",
        las = 1)
grid()

par(mfrow = c(1, 1))
dev.off()

cat("\n=== VISUALIZATIONS COMPLETE ===\n")
