# GrMtnPower standard visualizations
# Generate publication-ready plots of generation/consumption
# Kristian Omland

source("R/utilities.r")
source("R/data-processing.r")
source("R/visualizations.r")

# Set up colors
setup_gmp_colors()

# Load data
dat <- load_latest_data()
dat <- apply_standard_transformations(dat, addSolarPos = FALSE)

# Aggregate to daily
datDaily <- aggregate_daily(dat)

# Aggregate to weekly
datWeekly <- aggregate_weekly(dat)

cat("=== STANDARD VISUALIZATIONS ===\n\n")

# Plot 1: Full history daily
cat("1. Entire generation and usage history (daily aggregation)...\n")
print(plot_daily_summary(datDaily, "Full History: Daily Generation and Consumption"))

# Plot 2: Past year weekly
cat("2. Past year generation and usage (weekly aggregation)...\n")
print(plot_past_year(datDaily))

# Plot 3: Last month daily
cat("3. Past month generation, usage, and returns (daily)...\n")
print(plot_last_n_days(datDaily, nDays = 30))

cat("\nVisualizations complete!\n")
