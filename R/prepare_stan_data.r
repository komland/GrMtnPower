# GrMtnPower Stan data preparation
# Construct spline bases and format data for Stan model
# Kristian Omland

library(data.table)
library(mgcv)
library(lubridate)

#' Prepare data for Stan solar capacity model
#'
#' @param dat data.table with columns: dateForm, dateTime, generation, zenith, azimuth
#' @param zenith_threshold zenith angle cutoff for nighttime (degrees, default 95)
#' @param k_solar number of basis functions for solar position tensor product (default 25)
#' @param k_season number of basis functions for seasonal cycle (default 12)
#' @param estimate_degradation logical, whether to estimate degradation parameter rho (default TRUE)
#' @param commission_date IDate of system commissioning (default 2015-10-26)
#' @return list with components for Stan: N, Y, K, B_solar, S, B_season, t, estimate_degradation
prepare_stan_data <- function(dat, 
                               zenith_threshold = 95,
                               k_solar = 25,
                               k_season = 12,
                               estimate_degradation = TRUE,
                               commission_date = as.IDate("2015-10-26")) {
  
  # Filter to daytime observations with valid generation data
  dat_day <- dat[zenith < zenith_threshold & !is.na(generation) & !is.na(zenith) & !is.na(azimuth)]
  
  cat("Filtered to", nrow(dat_day), "daytime observations (zenith <", zenith_threshold, "degrees)\n")
  cat("Date range:", as.character(min(dat_day$dateForm)), "to", as.character(max(dat_day$dateForm)), "\n")
  cat("Generation range:", round(min(dat_day$generation), 2), "to", round(max(dat_day$generation), 2), "kWh/hr\n")
  
  # Time since commissioning (years)
  dat_day[, t_years := as.numeric(difftime(dateForm, commission_date, units = "days")) / 365.25]
  
  # Day of year (for seasonal cycle)
  dat_day[, yday := yday(dateForm)]
  
  # Construct tensor-product spline basis for solar position (zenith, azimuth)
  # Azimuth is circular (0 = 360), so use cyclic basis
  # Zenith is bounded [0, zenith_threshold], so use regular thin-plate spline
  
  cat("\nConstructing solar position spline basis (k =", k_solar, ")...\n")
  
  # Create smooth specification
  # te() creates tensor product, bs="cc" makes azimuth cyclic
  smooth_solar <- smoothCon(
    s(zenith, azimuth, bs = "tp", k = k_solar),
    data = data.frame(
      zenith = dat_day$zenith,
      azimuth = dat_day$azimuth
    ),
    absorb.cons = TRUE  # Remove constraint (we want unpenalized intercept)
  )[[1]]
  
  B_solar <- smooth_solar$X
  K <- ncol(B_solar)
  
  cat("  Basis dimensions:", nrow(B_solar), "x", K, "\n")
  
  # Construct cyclic spline basis for seasonal pattern (day of year)
  # Use cyclic cubic spline so day 1 = day 365/366
  
  cat("\nConstructing seasonal spline basis (k =", k_season, ")...\n")
  
  smooth_season <- smoothCon(
    s(yday, bs = "cc", k = k_season),
    data = data.frame(yday = dat_day$yday),
    absorb.cons = TRUE
  )[[1]]
  
  B_season <- smooth_season$X
  S <- ncol(B_season)
  
  cat("  Basis dimensions:", nrow(B_season), "x", S, "\n")
  
  # Assemble Stan data list
  stan_data <- list(
    N = nrow(dat_day),
    Y = dat_day$generation,
    K = K,
    B_solar = B_solar,
    S = S,
    B_season = B_season,
    t = dat_day$t_years,
    estimate_degradation = as.integer(estimate_degradation)
  )
  
  # Store data.table for initialization
  attr(stan_data, "dat") <- dat_day
  
  cat("\nStan data prepared successfully.\n")
  cat("  Observations:", stan_data$N, "\n")
  cat("  Solar basis functions:", stan_data$K, "\n")
  cat("  Seasonal basis functions:", stan_data$S, "\n")
  cat("  Time range:", round(min(stan_data$t), 2), "to", round(max(stan_data$t), 2), "years\n")
  
  return(stan_data)
}


#' Initialize Stan parameters with feasible values
#'
#' @param stan_data list returned by prepare_stan_data()
#' @return list of initial parameter values for Stan
initialize_stan_params <- function(stan_data) {
  
  dat <- attr(stan_data, "dat")
  if (is.null(dat)) {
    stop("stan_data must have 'dat' attribute (use prepare_stan_data())")
  }
  
  cat("\nInitializing parameters...\n")
  
  # Simple approach: initialize beta to create a flat surface well above max(Y)
  # Model uses mu = exp(B_solar * beta), so we need B * beta ≈ log(target_mu)
  max_gen <- max(dat$generation)
  target_mu <- max_gen * 1.5  # 50% above maximum observed generation
  
  cat("  Max generation:", round(max_gen, 2), "kWh/hr\n")
  cat("  Target mu (flat):", round(target_mu, 2), "kWh/hr\n")
  
  # Initialize beta to create constant surface at log(target_mu)
  log_target <- log(target_mu)
  target_vec <- rep(log_target, nrow(stan_data$B_solar))
  beta_init <- as.numeric(solve(
    t(stan_data$B_solar) %*% stan_data$B_solar + diag(0.1, stan_data$K),
    t(stan_data$B_solar) %*% target_vec
  ))
  
  # Check reconstruction
  log_mu_reconstruct <- as.numeric(stan_data$B_solar %*% beta_init)
  mu_reconstruct <- exp(log_mu_reconstruct)
  min_mu <- min(mu_reconstruct)
  max_mu <- max(mu_reconstruct)
  min_delta <- min(mu_reconstruct - dat$generation)
  
  cat("  Reconstructed mu range: [", round(min_mu, 2), ",", round(max_mu, 2), "] kWh/hr\n")
  cat("  Minimum delta (mu - Y):", round(min_delta, 3), "kWh/hr\n")
  
  if (min_delta <= 0) {
    cat("  WARNING: Some delta values near zero or negative. Increasing target mu.\n")
    # Use higher target
    target_mu <- max_gen * 2.0
    log_target <- log(target_mu)
    target_vec <- rep(log_target, nrow(stan_data$B_solar))
    beta_init <- as.numeric(solve(
      t(stan_data$B_solar) %*% stan_data$B_solar + diag(0.1, stan_data$K),
      t(stan_data$B_solar) %*% target_vec
    ))
    mu_reconstruct <- exp(as.numeric(stan_data$B_solar %*% beta_init))
    min_delta <- min(mu_reconstruct - dat$generation)
    cat("  After adjustment, minimum delta:", round(min_delta, 3), "kWh/hr\n")
  }
  
  # Initialize loss parameters based on empirical losses
  delta_empirical <- mu_reconstruct - dat$generation
  delta_mean_empirical <- mean(delta_empirical)
  delta_var_empirical <- var(delta_empirical)
  
  # Method of moments for Gamma
  alpha_init <- delta_mean_empirical^2 / delta_var_empirical
  alpha_init <- max(1.0, min(alpha_init, 10))  # keep in reasonable range
  
  cat("  Loss parameters:\n")
  cat("    Mean loss:", round(delta_mean_empirical, 3), "kWh/hr\n")
  cat("    Alpha (shape):", round(alpha_init, 2), "\n")
  
  # Initialize seasonal pattern (start flat)
  gamma_s_init <- rep(0, stan_data$S)
  gamma0_init <- log(max(delta_mean_empirical, 0.1))
  
  cat("    Gamma0:", round(gamma0_init, 3), "\n")
  
  # Initialize degradation
  rho_init <- 0.005  # 0.5% per year
  
  init_list <- list(
    beta = beta_init,
    sigma_beta = 1.0,
    gamma0 = gamma0_init,
    gamma_s = gamma_s_init,
    sigma_gamma = 1.0,
    rho_raw = rho_init,
    alpha_loss = alpha_init
  )
  
  cat("\nInitialization complete.\n")
  
  return(init_list)
}
