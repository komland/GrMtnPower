# GrMtnPower loss model analysis
# Estimate potential generation Y and loss parameter q
# Kristian Omland

source("R/utilities.r")
source("R/data-processing.r")
source("R/modeling.r")

library(data.table)
library(lubridate)
library(mgcv)
library(scam)

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

# Strategy: Use ISO week as the step function period
# Each of 53 ISO weeks gets a step height = max generation in that week
# This is optimal: minimizes sum(Y0 - generation) subject to Y0 >= generation

# Add ISO week
dat_model[, isoweek := isoweek(dateForm)]

cat("Using ISO week as step function periods (53 steps)\n")

# For each ISO week, find the maximum observed generation (across all years)
# Only consider daytime with generation > 0 to avoid setting Y0 based on zeros
week_max <- dat_model[zenith <= 90 & generation > 0, .(
  Y0_week = max(generation, na.rm = TRUE),
  n_obs = .N
), by = isoweek]

cat("  Number of ISO weeks with data:", nrow(week_max), "\n")
cat("  Weekly max generation range:", 
    round(min(week_max$Y0_week), 2), "to",
    round(max(week_max$Y0_week), 2), "kWh\n")

# Check for missing weeks and fill with interpolation or nearby weeks
if (nrow(week_max) < 53) {
  cat("  WARNING:", 53 - nrow(week_max), "weeks have no positive generation data\n")
  # Create full week sequence
  all_weeks <- data.table(isoweek = 1:53)
  week_max <- merge(all_weeks, week_max, by = "isoweek", all.x = TRUE)
  
  # Fill missing Y0 values with linear interpolation
  if (any(is.na(week_max$Y0_week))) {
    setorder(week_max, isoweek)
    for (i in which(is.na(week_max$Y0_week))) {
      # Find nearest non-NA values
      before <- max(which(!is.na(week_max$Y0_week) & 1:nrow(week_max) < i), 0)
      after <- min(which(!is.na(week_max$Y0_week) & 1:nrow(week_max) > i), nrow(week_max) + 1)
      
      if (before > 0 && after <= nrow(week_max)) {
        # Linear interpolation
        week_max$Y0_week[i] <- (week_max$Y0_week[before] + week_max$Y0_week[after]) / 2
      } else if (before > 0) {
        # Use before value
        week_max$Y0_week[i] <- week_max$Y0_week[before]
      } else if (after <= nrow(week_max)) {
        # Use after value
        week_max$Y0_week[i] <- week_max$Y0_week[after]
      }
    }
    cat("  Filled missing weeks using interpolation\n")
  }
}

# Assign Y0 based on ISO week
dat_model <- merge(dat_model, week_max, by = "isoweek", all.x = TRUE)
setnames(dat_model, "Y0_week", "Y0")

# Set nighttime to 0
dat_model[zenith > 90 | is.na(Y0), Y0 := 0]
dat_model[Y0 < 0, Y0 := 0]

# Compute total difference
total_diff <- dat_model[zenith <= 90, sum(Y0 - generation, na.rm = TRUE)]

cat("  Y0 range:", round(min(dat_model$Y0, na.rm = TRUE), 2), "to", 
    round(max(dat_model$Y0, na.rm = TRUE), 2), "kWh\n")
cat("  Total difference (sum of Y0 - generation):", round(total_diff, 0), "kWh\n")

# Verify no violations (allow small floating-point tolerance)
n_exceed <- dat_model[generation > (Y0 + 1e-10) & !is.na(generation) & !is.na(Y0), .N]
cat("  Observations exceeding Y0:", n_exceed, "(should be 0)\n")

if (n_exceed > 0) {
  cat("  WARNING: Violations detected. Checking details...\n")
  violations <- dat_model[generation > Y0 & !is.na(generation) & !is.na(Y0)]
  cat("  Max violation:", max(violations$generation - violations$Y0), "kWh\n")
  cat("  Sample violations:\n")
  print(head(violations[, .(dateForm, isoweek, generation, Y0, diff = generation - Y0)][order(-diff)], 5))
}

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
