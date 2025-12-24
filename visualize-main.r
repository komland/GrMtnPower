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

# Remove incomplete days (e.g., today with only midnight reading)
dat <- remove_incomplete_days(dat)

# Aggregate to daily
datDaily <- aggregate_daily(dat)

# Aggregate to weekly
datWeekly <- aggregate_weekly(dat)

cat("=== STANDARD VISUALIZATIONS ===\n\n")

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Plot 1: Full history weekly
cat("1. Entire generation and usage history (weekly aggregation)...\n")
png("plots/full_history_weekly.png", width = 1200, height = 600)
print(plot_weekly_summary(datWeekly, "Full History: Weekly Generation and Consumption"))
dev.off()

# Plot 2: Past year weekly
cat("2. Past year generation and usage (weekly aggregation)...\n")
png("plots/past_year_weekly.png", width = 1200, height = 600)
print(plot_past_year_weekly(datWeekly))
dev.off()

# Plot 3: Last month daily
cat("3. Past month generation, usage, and returns (daily)...\n")
png("plots/past_month_daily.png", width = 1200, height = 600)
print(plot_last_n_days(datDaily, nDays = 30))
dev.off()

cat("\nVisualizations complete!\n")
