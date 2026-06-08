# Solar Capacity Model Diagnostics
# Comprehensive convergence and model checking
# Kristian Omland

library(data.table)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2)

# ---- Load fitted model ----
cat("Loading fitted model...\n")
fit <- readRDS("models/solar_capacity_fit.rds")

# ---- Convergence Diagnostics ----
cat("\n==== CONVERGENCE DIAGNOSTICS ====\n")

# Get summary
summary_all <- fit$summary()

# Check Rhat for all parameters
cat("\nRhat check:\n")
rhat_issues <- summary_all[summary_all$rhat > 1.01 & !is.na(summary_all$rhat), ]
if (nrow(rhat_issues) > 0) {
  cat("  WARNING:", nrow(rhat_issues), "parameters have Rhat > 1.01\n")
  cat("  Parameters with worst Rhat:\n")
  print(head(rhat_issues[order(-rhat_issues$rhat), c("variable", "rhat")], 10))
} else {
  cat("  ✓ All parameters have Rhat <= 1.01 (good convergence)\n")
}

# Check ESS
cat("\nEffective sample size (ESS) check:\n")
ess_bulk_low <- summary_all[summary_all$ess_bulk < 400 & !is.na(summary_all$ess_bulk), ]
if (nrow(ess_bulk_low) > 0) {
  cat("  WARNING:", nrow(ess_bulk_low), "parameters have ESS (bulk) < 400\n")
  cat("  Parameters with lowest ESS:\n")
  print(head(ess_bulk_low[order(ess_bulk_low$ess_bulk), c("variable", "ess_bulk", "ess_tail")], 10))
} else {
  cat("  ✓ All parameters have ESS (bulk) >= 400 (adequate sampling)\n")
}

# Check for divergences
cat("\nDivergent transitions:\n")
diagnostics <- fit$diagnostic_summary()
total_divergences <- sum(diagnostics$num_divergent)
cat("  Total divergences:", total_divergences, "\n")
if (total_divergences > 0) {
  cat("  WARNING: Divergences indicate geometry problems. Consider:\n")
  cat("    - Increasing adapt_delta (current value in fit script)\n")
  cat("    - Reparameterizing the model\n")
  cat("    - Checking for label switching or multimodality\n")
} else {
  cat("  ✓ No divergent transitions (good geometry)\n")
}

# Check max treedepth
cat("\nMax treedepth warnings:\n")
total_max_treedepth <- sum(diagnostics$num_max_treedepth)
cat("  Total max treedepth warnings:", total_max_treedepth, "\n")
if (total_max_treedepth > 0) {
  cat("  NOTE: Some iterations hit max treedepth. Consider increasing max_treedepth.\n")
} else {
  cat("  ✓ No max treedepth warnings\n")
}

# ---- Model-specific Diagnostics ----
cat("\n==== MODEL-SPECIFIC DIAGNOSTICS ====\n")

# Constraint violations
cat("\nConstraint violations (Y > mu):\n")
violations <- fit$summary("n_violations")
cat("  Mean violations:", round(violations$mean, 2), "\n")
cat("  SD violations:", round(violations$sd, 2), "\n")
cat("  95% CI: [", round(violations$q5, 1), ",", round(violations$q95, 1), "]\n")
if (violations$mean > 0.1) {
  cat("  WARNING: Constraint violations suggest model misspecification or initialization issues.\n")
} else {
  cat("  ✓ Negligible constraint violations (mu >= Y enforced successfully)\n")
}

# Degradation rate (if estimated)
cat("\nDegradation rate (rho):\n")
rho <- fit$summary("rho")
cat("  Mean:", round(rho$mean, 4), "(", round(rho$mean * 100, 2), "% per year)\n")
cat("  SD:", round(rho$sd, 4), "\n")
cat("  95% CI: [", round(rho$q5, 4), ",", round(rho$q95, 4), "]\n")
if (rho$q5 < 0.001) {
  cat("  NOTE: Lower bound near zero suggests degradation may be minimal or unidentifiable.\n")
}

# Loss parameters
cat("\nLoss distribution (Gamma shape):\n")
alpha_loss <- fit$summary("alpha_loss")
cat("  Mean:", round(alpha_loss$mean, 2), "\n")
cat("  95% CI: [", round(alpha_loss$q5, 2), ",", round(alpha_loss$q95, 2), "]\n")
cat("  Interpretation: Higher shape = more consistent loss, lower shape = more variable\n")

# Summary statistics
cat("\nGeneration summary:\n")
total_ideal <- fit$summary("total_ideal")
total_realized <- fit$summary("total_realized")
total_loss <- fit$summary("total_loss")
mean_loss_rate <- fit$summary("mean_loss_rate")

cat("  Total ideal capacity:", round(total_ideal$mean, 0), "kWh [", 
    round(total_ideal$q5, 0), ",", round(total_ideal$q95, 0), "]\n")
cat("  Total realized generation:", round(total_realized$mean, 0), "kWh [", 
    round(total_realized$q5, 0), ",", round(total_realized$q95, 0), "]\n")
cat("  Total loss:", round(total_loss$mean, 0), "kWh [", 
    round(total_loss$q5, 0), ",", round(total_loss$q95, 0), "]\n")
cat("  Mean loss rate:", round(mean_loss_rate$mean * 100, 1), "% [", 
    round(mean_loss_rate$q5 * 100, 1), "%,", round(mean_loss_rate$q95 * 100, 1), "%]\n")

# ---- Trace Plots ----
cat("\n==== GENERATING TRACE PLOTS ====\n")

# Extract draws
draws <- fit$draws(variables = c("rho", "alpha_loss", "gamma0", "sigma_beta", "sigma_gamma"))

# Trace plots for key parameters
png("plots/trace_key_params.png", width = 1200, height = 800)
mcmc_trace(draws) + 
  ggtitle("Trace plots for key parameters")
dev.off()
cat("Saved trace_key_params.png\n")

# ---- Density Plots ----
cat("\n==== GENERATING DENSITY PLOTS ====\n")

png("plots/density_key_params.png", width = 1200, height = 800)
mcmc_dens(draws) + 
  ggtitle("Posterior densities for key parameters")
dev.off()
cat("Saved density_key_params.png\n")

# ---- Pairs Plot ----
cat("\n==== GENERATING PAIRS PLOT ====\n")

png("plots/pairs_key_params.png", width = 1200, height = 1200)
mcmc_pairs(draws, 
           pars = c("rho", "alpha_loss", "gamma0"),
           diag_fun = "dens",
           off_diag_fun = "hex")
dev.off()
cat("Saved pairs_key_params.png\n")

# ---- Posterior Predictive Check ----
cat("\n==== POSTERIOR PREDICTIVE CHECK ====\n")

# Extract Y_rep (posterior predictive samples)
Y_rep_draws <- fit$draws("Y_rep", format = "matrix")

# Get observed Y from stan data
# Note: need to reload stan_data to get Y
source("R/utilities.r")
source("R/data-processing.r")
source("R/prepare_stan_data.r")
dat <- load_latest_data()
dat <- apply_standard_transformations(dat, addSolarPos = TRUE)
dat <- dat[!is.na(zenith) & !is.na(azimuth)]
stan_data <- prepare_stan_data(dat, zenith_threshold = 95, k_solar = 25, k_season = 12)
Y_obs <- stan_data$Y

# PPC density overlay
png("plots/ppc_density.png", width = 1200, height = 800)
ppc_dens_overlay(Y_obs, Y_rep_draws[1:100, ]) +  # plot 100 draws
  ggtitle("Posterior Predictive Check: Generation Distribution") +
  xlab("Generation (kWh/hr)")
dev.off()
cat("Saved ppc_density.png\n")

# PPC intervals
png("plots/ppc_intervals.png", width = 1600, height = 800)
ppc_intervals(Y_obs[1:500], Y_rep_draws[, 1:500], prob = 0.5, prob_outer = 0.95) +
  ggtitle("Posterior Predictive Check: 50% and 95% Intervals (first 500 obs)") +
  xlab("Observation index") +
  ylab("Generation (kWh/hr)")
dev.off()
cat("Saved ppc_intervals.png\n")

cat("\n==== DIAGNOSTICS COMPLETE ====\n")
cat("Review plots in plots/ directory\n")
cat("Check models/solar_capacity_summary.csv for full parameter estimates\n")
