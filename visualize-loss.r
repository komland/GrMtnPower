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

# Plot 7: Capacity factor by year (degradation analysis)
cat("7. Capacity factor by year (degradation check)...\n")
png("plots/07_capacity_factor_by_year.png", width = 800, height = 600)

# Calculate annual capacity factors for complete years
annual_cf <- annual_capacity[hours_total > 8000, .(
  solarYear,
  capacity_factor = total_gen_kWh / total_Y0_kWh
)]

# Create plot
plot(annual_cf$solarYear, annual_cf$capacity_factor,
     type = "b", pch = 19, col = "blue",
     xlab = "Solar Year", ylab = "Capacity Factor (Actual / Y0)",
     main = "Annual Capacity Factor - Degradation Analysis",
     ylim = c(0.45, 0.55))

# Add trend line
if (nrow(annual_cf) > 2) {
  lm_fit <- lm(capacity_factor ~ solarYear, data = annual_cf)
  abline(lm_fit, col = "red", lty = 2, lwd = 2)
  
  # Calculate annual degradation rate
  slope <- coef(lm_fit)[2]
  mean_cf <- mean(annual_cf$capacity_factor)
  annual_deg_pct <- abs(slope / mean_cf * 100)
  
  # Add legend with degradation rate
  legend("topright", 
         legend = c("Annual CF", 
                    "Trend line",
                    sprintf("Degradation: %.2f%%/year", annual_deg_pct)),
         col = c("blue", "red", NA),
         lty = c(1, 2, NA),
         pch = c(19, NA, NA),
         lwd = c(1, 2, NA))
}

grid()
dev.off()

cat("\n=== VISUALIZATIONS COMPLETE ===\n")
