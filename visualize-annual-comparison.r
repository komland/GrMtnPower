# GrMtnPower Annual Comparison Visualization
# Annual generation by ISO week with historical context
# Kristian Omland

library(data.table)
library(lubridate)

# Load weekly summary data
if (!file.exists("data/weekly_summary.RDS")) {
  stop("Weekly summary data not found. Run scripts/generate_weekly_data.R first.")
}

weekly_summary <- readRDS("data/weekly_summary.RDS")

# Calculate weekly generation by isoweek
weekly_gen <- weekly_summary[, .(
  year = year,
  isoweek = isoweek,
  total_gen = total_gen
)]

# Filter to ISO weeks 2-51 (avoid boundary weeks)
weekly_gen <- weekly_gen[isoweek >= 2 & isoweek <= 51]

# Get current year
current_year <- year(Sys.Date())

# Separate historical (prior years) from current year
historical <- weekly_gen[year < current_year]
current <- weekly_gen[year == current_year]

# Calculate fixed capacity for capacity factor
capacity_fixed <- ceiling(max(weekly_summary$total_Y0, na.rm = TRUE))

# Calculate summary statistics by ISO week (historical data)
weekly_stats <- historical[, .(
  median_gen = median(total_gen, na.rm = TRUE),
  q25 = quantile(total_gen, 0.25, na.rm = TRUE),
  q75 = quantile(total_gen, 0.75, na.rm = TRUE),
  min_gen = min(total_gen, na.rm = TRUE),
  max_gen = max(total_gen, na.rm = TRUE),
  median_cf = median(total_gen / capacity_fixed, na.rm = TRUE),
  q25_cf = quantile(total_gen / capacity_fixed, 0.25, na.rm = TRUE),
  q75_cf = quantile(total_gen / capacity_fixed, 0.75, na.rm = TRUE),
  min_cf = min(total_gen / capacity_fixed, na.rm = TRUE),
  max_cf = max(total_gen / capacity_fixed, na.rm = TRUE)
), by = isoweek]

# Add capacity factor to current year data
current[, capacity_factor := total_gen / capacity_fixed]

setorder(weekly_stats, isoweek)
setorder(current, isoweek)

# Create plot
cat("Creating annual comparison plot...\n")

png("plots/09_annual_comparison.png", width = 1600, height = 800)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# === LEFT PANEL: Generation (kWh) ===
# Set up empty plot with appropriate limits
y_max <- max(weekly_stats$max_gen, current$total_gen, na.rm = TRUE)
y_min <- 0

plot(weekly_stats$isoweek, weekly_stats$median_gen, 
     type = "n",
     xlim = c(2, 51),
     ylim = c(y_min, y_max * 1.05),
     xlab = "ISO Week of Year",
     ylab = "Generation (kWh)",
     main = sprintf("Weekly Generation: %d vs Historical", current_year),
     las = 1)

grid(col = "gray80", lty = "dotted")

# Add full range (light gray polygon)
polygon(c(weekly_stats$isoweek, rev(weekly_stats$isoweek)),
        c(weekly_stats$min_gen, rev(weekly_stats$max_gen)),
        col = "gray90", border = NA)

# Add IQR (dark gray polygon)
polygon(c(weekly_stats$isoweek, rev(weekly_stats$isoweek)),
        c(weekly_stats$q25, rev(weekly_stats$q75)),
        col = "gray70", border = NA)

# Add median line (black)
lines(weekly_stats$isoweek, weekly_stats$median_gen,
      col = "black", lwd = 2)

# Add current year line (bright green)
if (nrow(current) > 0) {
  lines(current$isoweek, current$total_gen,
        col = "green3", lwd = 3)
  points(current$isoweek, current$total_gen,
         col = "green3", pch = 19, cex = 0.8)
}

# Add legend
legend("topright",
       legend = c("Median", "IQR", "Full Range", sprintf("%d YTD", current_year)),
       col = c("black", "gray70", "gray90", "green3"),
       lwd = c(2, 8, 8, 3),
       pch = c(NA, NA, NA, 19),
       bg = "white",
       box.lty = 1,
       cex = 0.9)

# === RIGHT PANEL: Capacity Factor ===
# Set up empty plot with appropriate limits
cf_max <- max(weekly_stats$max_cf, current$capacity_factor, na.rm = TRUE)
cf_min <- 0

plot(weekly_stats$isoweek, weekly_stats$median_cf, 
     type = "n",
     xlim = c(2, 51),
     ylim = c(cf_min, cf_max * 1.05),
     xlab = "ISO Week of Year",
     ylab = "Capacity Factor",
     main = sprintf("Weekly Capacity Factor: %d vs Historical", current_year),
     las = 1)

grid(col = "gray80", lty = "dotted")

# Add full range (light gray polygon)
polygon(c(weekly_stats$isoweek, rev(weekly_stats$isoweek)),
        c(weekly_stats$min_cf, rev(weekly_stats$max_cf)),
        col = "gray90", border = NA)

# Add IQR (dark gray polygon)
polygon(c(weekly_stats$isoweek, rev(weekly_stats$isoweek)),
        c(weekly_stats$q25_cf, rev(weekly_stats$q75_cf)),
        col = "gray70", border = NA)

# Add median line (black)
lines(weekly_stats$isoweek, weekly_stats$median_cf,
      col = "black", lwd = 2)

# Add current year line (bright green)
if (nrow(current) > 0) {
  lines(current$isoweek, current$capacity_factor,
        col = "green3", lwd = 3)
  points(current$isoweek, current$capacity_factor,
         col = "green3", pch = 19, cex = 0.8)
}

# Add legend
legend("topright",
       legend = c("Median", "IQR", "Full Range", sprintf("%d YTD", current_year)),
       col = c("black", "gray70", "gray90", "green3"),
       lwd = c(2, 8, 8, 3),
       pch = c(NA, NA, NA, 19),
       bg = "white",
       box.lty = 1,
       cex = 0.9)

dev.off()

cat("Annual comparison plot saved to plots/09_annual_comparison.png\n")
