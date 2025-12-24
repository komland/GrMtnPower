# GrMtnPower loss model visualizations
# Diagnostic plots for Y and q models
# Run this after analyze-loss.r
# Kristian Omland

# Assumes the following objects exist in workspace:
# - model_Y: fitted GAM model
# - dat_model: data with Y and q columns
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
cat("1. Observed vs. Potential Generation (Y)...\n")
png("plots/01_observed_vs_Y.png", width = 800, height = 600)
print(plot_Y_vs_observed(dat_model, sample_frac = 0.2))
dev.off()

# Plot 2: Y surface over solar position
cat("2. Potential Generation surface Y(azimuth, zenith)...\n")
png("plots/02_Y_surface.png", width = 800, height = 600)
print(plot_Y_surface(model_Y, n_grid = 40))
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
gam.check(model_Y)
par(mfrow = c(1, 1))
dev.off()

# Plot 6: Residuals vs solar position
cat("6. Residuals by solar position...\n")
dat_model[Y > 0 & !is.na(generation), residual := generation - Y]

sample_dat <- dat_model[!is.na(residual)][sample(.N, min(.N, 5000))]

png("plots/06_residuals_by_position.png", width = 1000, height = 800)
print(xyplot(residual ~ zenith | cut(azimuth, breaks = 4), sample_dat,
             pch = 16, cex = 0.3, alpha = 0.3,
             xlab = "Zenith (degrees)",
             ylab = "Residual (Observed - Y)",
             main = "Residuals by Solar Position",
             panel = function(x, y, ...) {
               panel.abline(h = 0, col = "red", lwd = 2)
               panel.xyplot(x, y, ...)
             }))
dev.off()

cat("\n=== VISUALIZATIONS COMPLETE ===\n")
