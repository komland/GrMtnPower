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

# Filter to valid generation periods
cat("Filtering to valid generation data...\n")
dat_model <- dat[!is.na(generation) & !is.na(zenith) & !is.na(azimuth)]
cat("  Total observations with solar position:", nrow(dat_model), "\n")

# Remove rows where generation is zero during daytime (system offline)
# Keep nighttime zeros (zenith > 90)
dat_model <- dat_model[!(generation == 0 & zenith <= 90)]
cat("  After removing daytime zeros (offline periods):", nrow(dat_model), "\n")

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
# STEP 1: Fit potential generation model Y
# ============================================================================

cat("\n=== FITTING POTENTIAL GENERATION MODEL Y ===\n")

# Fit GAM model to UPPER ENVELOPE using quantile regression
# Y represents theoretical maximum generation for each sun position
cat("Fitting quantile GAM model (tau = 0.98) for upper envelope...\n")
cat("This may take a few minutes...\n")

library(qgam)

model_Y <- qgam(
  generation ~ s(azimuth, zenith, k = 30),
  data = dat_model[zenith <= 90],
  qu = 0.98  # 98th percentile - upper envelope
)

cat("Model fit complete!\n")
cat("Quantile: 0.98 (upper envelope)\n")
cat("Deviance explained:", round(summary(model_Y)$dev.expl * 100, 2), "%\n")
cat("R-squared:", round(summary(model_Y)$r.sq, 3), "\n")

# Predict Y for all observations
cat("\nPredicting potential generation Y...\n")
dat_model[, Y := predict(model_Y, newdata = dat_model)]

# Set Y to zero for nighttime (zenith > 90)
dat_model[zenith > 90, Y := 0]

# Ensure Y is non-negative
dat_model[Y < 0, Y := 0]

cat("  Y range:", round(min(dat_model$Y), 2), "to", round(max(dat_model$Y), 2), "kWh\n")

# ============================================================================
# STEP 2: Estimate loss parameter q
# ============================================================================

cat("\n=== ESTIMATING LOSS PARAMETER q ===\n")

# Calculate q = 1 - (observed / Y) for daytime hours where Y > 0
dat_model[Y > 0.01, q := 1 - (generation / Y)]

# Constrain q to [0, 1] interval
dat_model[q < 0, q := 0]
dat_model[q > 1, q := 1]

# Remove rows where q is undefined
dat_loss <- dat_model[!is.na(q) & Y > 0.01]

cat("  Observations with valid q:", nrow(dat_loss), "\n")
cat("  Mean loss q:", round(mean(dat_loss$q), 3), "\n")
cat("  Median loss q:", round(median(dat_loss$q), 3), "\n")
cat("  SD loss q:", round(sd(dat_loss$q), 3), "\n")
cat("  Q25 loss q:", round(quantile(dat_loss$q, 0.25), 3), "\n")
cat("  Q75 loss q:", round(quantile(dat_loss$q, 0.75), 3), "\n")

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

# Compute annual integral of Y by solar year
annual_capacity <- dat_model[, .(
  total_Y_kWh = sum(Y, na.rm = TRUE),
  total_gen_kWh = sum(generation, na.rm = TRUE),
  hours_Y_positive = sum(Y > 0, na.rm = TRUE),
  hours_total = .N
), solarYear]

setorder(annual_capacity, solarYear)

cat("\nAnnual capacity by solar year:\n")
print(annual_capacity)

# Average across years (excluding incomplete first/last years)
complete_years <- annual_capacity[hours_total > 8000]  # At least ~330 days
if (nrow(complete_years) > 0) {
  cat("\nAverage across complete solar years:\n")
  cat("  Mean potential generation (Y):", 
      round(mean(complete_years$total_Y_kWh), 0), "kWh/year\n")
  cat("  Mean observed generation:", 
      round(mean(complete_years$total_gen_kWh), 0), "kWh/year\n")
  cat("  Mean sunlight hours (Y > 0):", 
      round(mean(complete_years$hours_Y_positive), 0), "hours/year\n")
  
  # Compare to theoretical
  theoretical_capacity <- 21 * 0.321 * 2300
  cat("\nComparison to theoretical:\n")
  cat("  Theoretical (21 × 0.321 kW × 2300 hrs):", 
      round(theoretical_capacity, 0), "kWh/year\n")
  cat("  Modeled potential Y:", 
      round(mean(complete_years$total_Y_kWh), 0), "kWh/year\n")
  cat("  Ratio (modeled/theoretical):", 
      round(mean(complete_years$total_Y_kWh) / theoretical_capacity, 3), "\n")
  
  # Overall capacity factor (actual vs. potential)
  overall_cf <- mean(complete_years$total_gen_kWh) / mean(complete_years$total_Y_kWh)
  cat("  Overall capacity factor (actual/Y):", round(overall_cf, 3), "\n")
  cat("  Implied mean loss (1 - CF):", round(1 - overall_cf, 3), "\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Modeling results saved in workspace variables:\n")
cat("  model_Y: GAM model for potential generation\n")
cat("  dat_model: full dataset with Y and q columns\n")
cat("  dat_loss: subset with valid loss estimates\n")
cat("  annual_capacity: yearly summaries\n")
