// Solar Generation Model: Ideal Capacity and Loss
// Two-component Bayesian hierarchical model for rooftop solar generation
// 
// Model structure:
//   Y_i = mu_i - delta_i
//   mu_i = (1 - rho * t_i) * f(zenith_i, azimuth_i)  [ideal capacity]
//   delta_i ~ Gamma(alpha_loss, alpha_loss / lambda_i)  [generation loss]
//   log(lambda_i) = gamma0 + g(day_of_year_i)  [seasonal loss pattern]

data {
  int<lower=1> N;                    // number of observations
  vector<lower=0>[N] Y;              // realized generation (kWh/hr)
  
  // Spline bases
  int<lower=1> K;                    // number of basis functions for solar position
  matrix[N, K] B_solar;              // tensor-product basis (zenith, azimuth)
  int<lower=1> S;                    // number of basis functions for seasonality
  matrix[N, S] B_season;             // cyclic spline basis (day-of-year)
  
  // Time variable
  vector<lower=0>[N] t;              // years since system commissioning
  
  // Model selection
  int<lower=0, upper=1> estimate_degradation;  // 1 = estimate rho, 0 = fix rho=0
}

parameters {
  // Ideal capacity surface f(zenith, azimuth)
  vector[K] beta;
  real<lower=0> sigma_beta;
  
  // Seasonal loss pattern g(day-of-year)
  real gamma0;
  vector[S] gamma_s;
  real<lower=0> sigma_gamma;
  
  // Degradation (optional)
  real<lower=0, upper=1> rho_raw;
  
  // Loss distribution
  real<lower=0> alpha_loss;
}

transformed parameters {
  vector[N] mu;                      // ideal capacity
  vector[N] delta_mean;              // mean generation loss
  vector[N] delta;                   // realized loss
  real<lower=0, upper=1> rho;        // degradation rate
  
  // Apply degradation flag
  if (estimate_degradation == 1) {
    rho = rho_raw;
  } else {
    rho = 0.0;
  }
  
  // Ideal capacity: (1 - rho * t) * exp(f(zenith, azimuth))
  // Use exp to guarantee positivity
  mu = (1 - rho * t) .* exp(B_solar * beta);
  
  // Mean loss: exp(gamma0 + g(day_of_year))
  delta_mean = exp(gamma0 + B_season * gamma_s);
  
  // Realized loss
  delta = mu - Y;
}

model {
  // Priors
  beta ~ normal(0, sigma_beta);
  sigma_beta ~ normal(0, 1);
  
  gamma_s ~ normal(0, sigma_gamma);
  sigma_gamma ~ normal(0, 1);
  gamma0 ~ normal(0, 2);
  
  if (estimate_degradation == 1) {
    rho_raw ~ normal(0.005, 0.002);  // ~0.5%/yr crystalline Si degradation
  }
  
  alpha_loss ~ gamma(2, 0.1);
  
  // Likelihood: delta ~ Gamma(alpha_loss, alpha_loss / delta_mean)
  // This enforces delta > 0, which means mu > Y (the frontier constraint)
  delta ~ gamma(alpha_loss, alpha_loss ./ delta_mean);
}

generated quantities {
  // Diagnostics
  array[N] int violation_flag;
  for (i in 1:N) {
    violation_flag[i] = (Y[i] > mu[i]) ? 1 : 0;
  }
  int<lower=0, upper=N> n_violations = sum(violation_flag);
  
  // Posterior predictive checks
  vector[N] Y_rep;
  for (i in 1:N) {
    real delta_rep = gamma_rng(alpha_loss, alpha_loss / delta_mean[i]);
    Y_rep[i] = fmax(0, mu[i] - delta_rep);  // generation cannot be negative
  }
  
  // Summary statistics
  real total_ideal = sum(mu);
  real total_realized = sum(Y);
  real total_loss = sum(delta);
  real mean_loss_rate = mean(delta ./ mu);
}
