# Solar Generation Model: Specification

## Overview

A two-component Bayesian hierarchical model fit via Stan (R interface: `cmdstanr`) to hourly rooftop solar generation data spanning 2015-10-26 to present. The goal is to estimate **ideal capacity** — a smooth latent upper bound on generation — and **generation loss** — the gap between ideal and realized, attributable to all suboptimal conditions without source attribution.

---

## Data

| Field | Description |
|---|---|
| Timestamp | Hourly, continuous since 2015-10-26, day and night |
| Zenith $\zeta_i$ | Solar zenith angle, computed from timestamp + site coordinates |
| Azimuth $\alpha_i$ | Solar azimuth angle, computed from timestamp + site coordinates |
| $Y_i$ | Realized generation (kWh/hr = mean kW) |

**Preprocessing:**
- Exclude nighttime observations ($\zeta_i \geq \zeta^*$; initial threshold 95°, possibly relaxed 100° allowing ideal generation from bright sky with the sun below the horizon)
- Flag any $Y_i > \hat{\mu}_i$ as model diagnostics (should not occur in valid posterior draws)
- Time covariate $t_i$ = years since 2015-10-26

**System geometry:**
- Azimuth: ~225° (southwest)
- Pitch: steep (possibly 8:12)
- Consequence: generation peaks mid-to-late afternoon; morning/evening asymmetry is real and must not be constrained away

---

## Model Structure

### Decomposition

$$Y_i = \mu_i - \delta_i$$

- $\mu_i$ — **ideal capacity**: latent, smooth, strictly $> Y_i$
- $\delta_i$ — **generation loss**: latent, $\geq 0$, fully unsourced

### Sub-model 1: Ideal Capacity

$$\mu_i = (1 - \rho \cdot t_i) \cdot f(\zeta_i, \alpha_i)$$

$$f(\zeta, \alpha) = \mathbf{B}(\zeta, \alpha)^\top \boldsymbol{\beta}$$

- $\mathbf{B}(\zeta, \alpha)$: tensor-product spline basis in $(\zeta, \alpha)$, constructed via `mgcv` and passed to Stan as data
- Azimuth treated as circular (0° = 360° continuity enforced in basis construction)
- No symmetry constraints; no physical irradiance model; purely phenomenological
- $\rho$: annual decay rate (panel degradation)
- $\mu_i$ is a **stochastic frontier** — a smooth upper bound identified by the data, not a conditional mean or quantile

### Sub-model 2: Generation Loss

$$\delta_i = \mu_i - Y_i \sim \text{Gamma}\!\left(\alpha_{\text{loss}},\ \frac{\alpha_{\text{loss}}}{\lambda_i}\right)$$

$$\log \lambda_i = \gamma_0 + g(d_i)$$

- $g(d_i)$: cyclic spline in day-of-year, capturing seasonal variation in mean loss
- $\gamma_0$: global loss intercept
- $\alpha_{\text{loss}}$: Gamma shape; controls concentration of loss around its mean
- No weather covariates; no zero-inflation; loss sources are entirely latent

---

## Identification

$\mu_i$ is identified by the tension between:
1. **The likelihood**: any draw where $\mu_i \leq Y_i$ has $-\infty$ log probability (Gamma undefined for $\delta \leq 0$), pushing the surface upward
2. **The smoothing penalty**: $\boldsymbol{\beta} \sim \text{Normal}(0, \sigma_\beta)$ prevents the surface from rising arbitrarily, pulling it toward the tightest smooth envelope

The result is a smooth upper envelope of realized generation across $(\zeta, \alpha)$ space — the tightest surface consistent with the data and the smoothness prior.

---

## Priors

| Parameter | Prior | Rationale |
|---|---|---|
| $\boldsymbol{\beta}$ | $\text{Normal}(0, \sigma_\beta)$ | Spline smoothing penalty |
| $\sigma_\beta$ | $\text{Half-Normal}(0, 1)$ | Weakly informative scale |
| $\boldsymbol{\gamma}_s$ | $\text{Normal}(0, \sigma_\gamma)$ | Seasonal spline smoothing |
| $\sigma_\gamma$ | $\text{Half-Normal}(0, 1)$ | Weakly informative scale |
| $\rho$ | $\text{Normal}(0.005, 0.002)$ truncated $(0,1)$ | ~0.5%/yr crystalline Si degradation |
| $\alpha_{\text{loss}}$ | $\text{Gamma}(2, 0.1)$ | Weakly informative, positive |

---

## Stan Model Outline

```stan
data {
  int<lower=1> N;
  vector[N] Y;
  matrix[N, K] B_solar;       // tensor-product spline basis (zeta, alpha)
  matrix[N, S] B_season;      // cyclic spline basis (day-of-year)
  vector[N] t;                // years since commissioning
}
parameters {
  vector[K] beta;
  real<lower=0> sigma_beta;
  real gamma0;
  vector[S] gamma_s;
  real<lower=0> sigma_gamma;
  real<lower=0, upper=1> rho;
  real<lower=0> alpha_loss;
}
transformed parameters {
  vector[N] mu = (1 - rho * t) .* (B_solar * beta);
  vector[N] delta_mean = exp(gamma0 + B_season * gamma_s);
}
model {
  beta       ~ normal(0, sigma_beta);
  sigma_beta ~ normal(0, 1);
  gamma_s    ~ normal(0, sigma_gamma);
  sigma_gamma ~ normal(0, 1);
  rho        ~ normal(0.005, 0.002);
  alpha_loss ~ gamma(2, 0.1);

  (mu - Y)   ~ gamma(alpha_loss, alpha_loss ./ delta_mean);
}
```

**Initialization note**: Stan must be initialized with spline coefficients that place $\mu_i$ above all observed $Y_i$. Recommend initializing $f(\zeta, \alpha)$ to a slightly inflated empirical upper envelope.

---

## Software Stack

| Component | Tool |
|---|---|
| Stan interface | `cmdstanr` |
| Spline basis construction | `mgcv` |
| Solar position computation | `suncalc` or `suntools` |
| Posterior visualization | `tidybayes`, `bayesplot` |
| Model comparison | `loo` |

---

## Inferential Goals

1. **Ideal capacity surface**: posterior of $f(\zeta, \alpha)$ — what could the system generate under perfect conditions at any solar position?
2. **Seasonal loss pattern**: posterior of $g(d)$ — how does expected loss vary across the calendar year?
3. **Degradation**: posterior of $\rho$ — is there evidence of performance decay over the 10-year record, and at what rate?
4. **Realized vs. ideal**: for any period, posterior estimate of total generation lost vs. total ideal capacity.

---

## Open Questions

- Exact roof pitch (would allow angle-of-incidence validation, though not required for fitting)
- Any known gaps, outages, or meter resets in the generation record
- Whether zenith threshold $\zeta^*$ should be fixed or estimated
