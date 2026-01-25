# GrMtnPower loss model analysis
# Estimate potential generation Y and loss parameter q
# Kristian Omland

source("R/utilities.r")
source("R/data-processing.r")
source("R/modeling.r")

library(data.table)
library(lubridate)
library(mgcv)
library(qgam)

# Load data with solar positions
cat("Loading data...\n")
dat <- load_latest_data()
dat <- apply_standard_transformations(dat, addSolarPos = TRUE)

# Filter to observations with solar position data
cat("Filtering to valid generation data...\n")
dat_model <- dat[!is.na(zenith) & !is.na(azimuth)]
cat("  Total observations with solar position:", nrow(dat_model), "\n")

# Treat missing generation as zero (system offline/not reporting)
dat_model[is.na(generation), generation := 0]

# Note: We keep all observations including daytime zeros
# Daytime zeros could be: clouds, snow cover, system offline, or data gaps
# Y0 represents capacity regardless of whether system was running
cat("  Observations with generation > 0:", dat_model[generation > 0, .N], "\n")

# Summary statistics
cat("\nData summary:\n")
cat("  Date range:", as.character(min(dat_model$dateForm)), "to", 
    as.character(max(dat_model$dateForm)), "\n")
cat("  Solar years:", paste(unique(dat_model$solarYear), collapse = ", "), "\n")
cat("  Generation range:", round(min(dat_model$generation), 2), "to", 
    round(max(dat_model$generation), 2), "kWh\n")
cat("  Zenith range:", round(min(dat_model$zenith), 1), "to", 
    round(max(dat_model$zenith), 1), "degrees\n")
cat("  Azimuth range:", round(min(dat_model$azimuth), 1), "to", 
    round(max(dat_model$azimuth), 1), "degrees\n")

# ============================================================================
# STEP 1: Fit capacity upper envelope Y0
# ============================================================================

cat("\n=== FITTING CAPACITY UPPER ENVELOPE Y0 ===\n")

# Strategy: Bin data by (azimuth, zenith), take max generation in each bin,
# fit smooth GAM to those maxima, then enforce Y0 >= observed everywhere

cat("Creating (azimuth, zenith) grid bins...\n")

# Create bins (5-degree resolution for azimuth and zenith)
dat_model[, az_bin := cut(azimuth, breaks = seq(0, 360, by = 5), include.lowest = TRUE)]
dat_model[, zen_bin := cut(zenith, breaks = seq(0, 180, by = 5), include.lowest = TRUE)]

# Get max generation in each bin
bin_maxima <- dat_model[zenith <= 90, .(
  max_gen = max(generation, na.rm = TRUE),
  mean_az = mean(azimuth, na.rm = TRUE),
  mean_zen = mean(zenith, na.rm = TRUE),
  n_obs = .N
), by = .(az_bin, zen_bin)]

# Remove bins with too few observations or infinite maxima
bin_maxima <- bin_maxima[is.finite(max_gen) & n_obs >= 5]

cat("  Valid bins:", nrow(bin_maxima), "\n")
cat("  Max generation in bins: ", round(max(bin_maxima$max_gen), 2), "kWh\n")

# Fit GAM to bin maxima
cat("\nFitting smooth GAM to bin maxima...\n")

model_Y0 <- gam(
  max_gen ~ s(mean_az, mean_zen, k = 30),
  data = bin_maxima,
  family = gaussian(link = "identity"),
  method = "REML"
)

cat("Model fit complete!\n")
cat("Deviance explained:", round(summary(model_Y0)$dev.expl * 100, 2), "%\n")
cat("R-squared:", round(summary(model_Y0)$r.sq, 3), "\n")

# Predict Y0 for all observations
cat("\nPredicting capacity Y0 for all observations...\n")
dat_model[, Y0_smooth := predict(model_Y0, newdata = data.table(
  mean_az = azimuth,
  mean_zen = zenith
))]

# Enforce hard constraint: Y0 >= observed generation (never exceeded)
# First apply the hard constraint for daytime
dat_model[zenith <= 90, Y0 := pmax(Y0_smooth, generation, na.rm = TRUE)]

# Set Y0 to zero for nighttime (but ensure it's at least as large as any nighttime generation)
dat_model[zenith > 90, Y0 := pmax(0, generation, na.rm = TRUE)]

# Ensure non-negative everywhere
dat_model[is.na(Y0) | Y0 < 0, Y0 := 0]

cat("  Y0 range:", round(min(dat_model$Y0, na.rm = TRUE), 2), "to", 
    round(max(dat_model$Y0, na.rm = TRUE), 2), "kWh\n")

# Check: verify no observations exceed Y0
n_exceed <- dat_model[generation > Y0 & !is.na(generation) & !is.na(Y0), .N]
cat("  Observations exceeding Y0:", n_exceed, "(should be 0)\n")

# ============================================================================
# STEP 2: Estimate loss parameter q
# ============================================================================

cat("\n=== ESTIMATING LOSS PARAMETER q ===\n")

# Calculate q = 1 - (observed / Y0) for daytime hours where Y0 > 0
dat_model[Y0 > 0.01, q := 1 - (generation / Y0)]

# Constrain q to [0, 1] interval (should already be satisfied by hard constraint)
dat_model[q < 0, q := 0]
dat_model[q > 1, q := 1]

# Remove rows where q is undefined
dat_loss <- dat_model[!is.na(q) & Y0 > 0.01]

cat("  Observations with valid q:", nrow(dat_loss), "\n")
cat("  Mean loss q:", round(mean(dat_loss$q), 3), "\n")
cat("  Median loss q:", round(median(dat_loss$q), 3), "\n")
cat("  SD loss q:", round(sd(dat_loss$q), 3), "\n")
cat("  Q25 loss q:", round(quantile(dat_loss$q, 0.25), 3), "\n")
cat("  Q75 loss q:", round(quantile(dat_loss$q, 0.75), 3), "\n")

# Check distribution of q at boundaries
n_q_zero <- dat_loss[q == 0, .N]
n_q_one <- dat_loss[q == 1, .N]
cat("  Observations with q=0 (at capacity):", n_q_zero, 
    sprintf("(%.1f%%)", 100 * n_q_zero / nrow(dat_loss)), "\n")
cat("  Observations with q=1 (zero generation):", n_q_one,
    sprintf("(%.1f%%)", 100 * n_q_one / nrow(dat_loss)), "\n")

# Fit beta distribution to q
# Beta distribution parameters: alpha, beta
# Mean = alpha / (alpha + beta)
# We can use method of moments or MLE

library(MASS)
q_vec <- dat_loss$q

# Remove exact 0s and 1s for beta fitting (beta is defined on (0,1))
q_vec_trim <- q_vec[q_vec > 0.001 & q_vec < 0.999]

if (length(q_vec_trim) > 100) {
  cat("\nFitting beta distribution to q...\n")
  
  # Method of moments for initial estimates
  q_mean <- mean(q_vec_trim)
  q_var <- var(q_vec_trim)
  
  # Beta distribution moments:
  # mean = a/(a+b), var = ab/((a+b)^2(a+b+1))
  # Solving: a = mean * (mean(1-mean)/var - 1)
  alpha_init <- q_mean * (q_mean * (1 - q_mean) / q_var - 1)
  beta_init <- alpha_init * (1 - q_mean) / q_mean
  
  cat("  Initial estimates: alpha =", round(alpha_init, 2), 
      ", beta =", round(beta_init, 2), "\n")
  cat("  Implied mean:", round(alpha_init / (alpha_init + beta_init), 3), "\n")
} else {
  cat("Not enough data points for beta distribution fitting.\n")
}

# ============================================================================
# STEP 3: Compute annual capacity
# ============================================================================

cat("\n=== ANNUAL CAPACITY ESTIMATION ===\n")

# Compute annual integral of Y0 by solar year
annual_capacity <- dat_model[, .(
  total_Y0_kWh = sum(Y0, na.rm = TRUE),
  total_gen_kWh = sum(generation, na.rm = TRUE),
  hours_Y0_positive = sum(Y0 > 0, na.rm = TRUE),
  hours_total = .N
), solarYear]

setorder(annual_capacity, solarYear)

cat("\nAnnual capacity by solar year:\n")
print(annual_capacity)

# Average across years (excluding incomplete first/last years)
complete_years <- annual_capacity[hours_total > 8000]  # At least ~330 days
if (nrow(complete_years) > 0) {
  cat("\nAverage across complete solar years:\n")
  cat("  Mean potential generation (Y0):", 
      round(mean(complete_years$total_Y0_kWh), 0), "kWh/year\n")
  cat("  Mean observed generation:", 
      round(mean(complete_years$total_gen_kWh), 0), "kWh/year\n")
  cat("  Mean sunlight hours (Y0 > 0):", 
      round(mean(complete_years$hours_Y0_positive), 0), "hours/year\n")
  
  # Compare to theoretical
  theoretical_capacity <- 21 * 0.321 * 2300
  cat("\nComparison to theoretical:\n")
  cat("  Theoretical (21 × 0.321 kW × 2300 hrs):", 
      round(theoretical_capacity, 0), "kWh/year\n")
  cat("  Modeled potential Y0:", 
      round(mean(complete_years$total_Y0_kWh), 0), "kWh/year\n")
  cat("  Ratio (modeled/theoretical):", 
      round(mean(complete_years$total_Y0_kWh) / theoretical_capacity, 3), "\n")
  
  # Overall capacity factor (actual vs. potential)
  overall_cf <- mean(complete_years$total_gen_kWh) / mean(complete_years$total_Y0_kWh)
  cat("  Overall capacity factor (actual/Y0):", round(overall_cf, 3), "\n")
  cat("  Implied mean loss (1 - CF):", round(1 - overall_cf, 3), "\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Modeling results saved in workspace variables:\n")
cat("  model_Y0: GAM model for upper capacity envelope\n")
cat("  dat_model: full dataset with Y0 and q columns\n")
cat("  dat_loss: subset with valid loss estimates\n")
cat("  annual_capacity: yearly summaries\n")
