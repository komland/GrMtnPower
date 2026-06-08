# Fit Solar Capacity Model
# Compile Stan model and run MCMC sampling
# Kristian Omland

library(data.table)
library(cmdstanr)
library(posterior)
library(bayesplot)

# Load utilities and data preparation
source("R/utilities.r")
source("R/data-processing.r")
source("R/prepare_stan_data.r")

# ---- Configuration ----
ZENITH_THRESHOLD <- 95         # degrees, exclude night observations
K_SOLAR <- 25                  # solar position basis functions
K_SEASON <- 12                 # seasonal basis functions
ESTIMATE_DEGRADATION <- TRUE   # estimate degradation parameter rho?

# MCMC settings
N_CHAINS <- 2
N_ITER_WARMUP <- 500
N_ITER_SAMPLING <- 500
N_PARALLEL_CHAINS <- 2
ADAPT_DELTA <- 0.95            # target acceptance rate (increase if divergences)
MAX_TREEDEPTH <- 12            # increase if max treedepth warnings

# Test mode: use subset of data for quick verification
TEST_MODE <- TRUE
TEST_MONTHS <- 2               # if TEST_MODE, use this many recent months

# ---- Load and prepare data ----
cat("Loading data...\n")
dat <- load_latest_data()
dat <- apply_standard_transformations(dat, addSolarPos = TRUE)

# Remove rows with missing solar position
dat <- dat[!is.na(zenith) & !is.na(azimuth)]

cat("Total observations:", nrow(dat), "\n")

# Subset for testing if requested
if (TEST_MODE) {
  max_date <- dat[, max(dateForm)]
  min_date <- max_date - (TEST_MONTHS * 30)
  dat <- dat[dateForm >= min_date]
  cat("TEST MODE: Using", TEST_MONTHS, "months of data (", nrow(dat), "observations)\n")
}

# Prepare Stan data
stan_data <- prepare_stan_data(
  dat, 
  zenith_threshold = ZENITH_THRESHOLD,
  k_solar = K_SOLAR,
  k_season = K_SEASON,
  estimate_degradation = ESTIMATE_DEGRADATION
)

# Initialize parameters
# Use Stan's default initialization which generates random values in (-2, 2)
# This should be sufficient for the log-link model
cat("\nUsing Stan's default random initialization...\n")

# ---- Compile Stan model ----
cat("\n==== Compiling Stan model ====\n")
model <- cmdstan_model("stan/solar_capacity.stan")

# ---- Fit model ----
cat("\n==== Fitting model ====\n")
cat("Chains:", N_CHAINS, "\n")
cat("Warmup iterations:", N_ITER_WARMUP, "\n")
cat("Sampling iterations:", N_ITER_SAMPLING, "\n")
cat("Adapt delta:", ADAPT_DELTA, "\n")

fit <- model$sample(
  data = stan_data,
  chains = N_CHAINS,
  parallel_chains = N_PARALLEL_CHAINS,
  iter_warmup = N_ITER_WARMUP,
  iter_sampling = N_ITER_SAMPLING,
  adapt_delta = ADAPT_DELTA,
  max_treedepth = MAX_TREEDEPTH,
  refresh = 100,
  show_messages = TRUE
)

# ---- Save results ----
cat("\n==== Saving results ====\n")

# Save fit object
fit$save_object(file = "models/solar_capacity_fit.rds")
cat("Saved fit object to models/solar_capacity_fit.rds\n")

# Save diagnostics
diagnostics <- fit$diagnostic_summary()
saveRDS(diagnostics, "models/solar_capacity_diagnostics.rds")
cat("Saved diagnostics to models/solar_capacity_diagnostics.rds\n")

# Save summary
summary <- fit$summary()
write.csv(summary, "models/solar_capacity_summary.csv", row.names = FALSE)
cat("Saved summary to models/solar_capacity_summary.csv\n")

# ---- Quick diagnostic check ----
cat("\n==== Quick Diagnostic Check ====\n")

# Check for divergences
n_divergences <- sum(fit$diagnostic_summary()$num_divergent)
cat("Divergent transitions:", n_divergences, "\n")
if (n_divergences > 0) {
  cat("  WARNING: Divergences detected. Consider increasing adapt_delta.\n")
}

# Check Rhat
max_rhat <- max(summary$rhat, na.rm = TRUE)
cat("Max Rhat:", round(max_rhat, 3), "\n")
if (max_rhat > 1.01) {
  cat("  WARNING: Rhat > 1.01 indicates convergence issues.\n")
}

# Check ESS
min_ess_bulk <- min(summary$ess_bulk, na.rm = TRUE)
cat("Min ESS (bulk):", round(min_ess_bulk, 0), "\n")
if (min_ess_bulk < 400) {
  cat("  WARNING: Low ESS. Consider increasing sampling iterations.\n")
}

# Check constraint violations
violations_summary <- fit$summary("n_violations")
mean_violations <- violations_summary$mean
cat("Mean constraint violations (Y > mu):", round(mean_violations, 2), "\n")
if (mean_violations > 0) {
  cat("  WARNING: Constraint violations detected. Check initialization and model specification.\n")
}

# Check degradation (if estimated)
if (ESTIMATE_DEGRADATION) {
  rho_summary <- fit$summary("rho")
  cat("\nDegradation rate (rho):\n")
  cat("  Mean:", round(rho_summary$mean, 4), "\n")
  cat("  95% CI: [", round(rho_summary$q5, 4), ",", round(rho_summary$q95, 4), "]\n")
  if (rho_summary$q5 < 0.001 && rho_summary$q95 > 0.010) {
    cat("  NOTE: Wide credible interval suggests degradation may not be well-identified.\n")
    cat("        Consider refitting with estimate_degradation = FALSE.\n")
  }
}

cat("\n==== Fitting complete ====\n")
cat("Run diagnostics script for detailed analysis.\n")
