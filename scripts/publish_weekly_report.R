#!/usr/bin/env Rscript
# Format and publish weekly performance report from saved data
# Reads data/weekly_summary.RDS and generates reports/week_WW_YYYY.txt
args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(lubridate)

# Load weekly summary data
if (!file.exists("data/weekly_summary.RDS")) {
  stop("Weekly summary data not found. Run scripts/generate_weekly_data.R first.")
}

weekly_summary <- readRDS("data/weekly_summary.RDS")

# Determine target week (most recent complete week, or specified week/year)
if (length(args) >= 2) {
  target_week <- as.integer(args[1])
  target_year <- as.integer(args[2])
} else {
  # Find most recent complete week from the data
  max_year <- max(weekly_summary$year)
  max_week_in_year <- weekly_summary[year == max_year, max(isoweek)]
  
  # Assume current week might be incomplete; use previous week
  # (In production, check if current week has 168 hours, but for now use simple logic)
  target_year <- max_year
  target_week <- max_week_in_year
  
  cat("Auto-detected most recent week: ISO Week", target_week, "Year", target_year, "\n")
}

# Extract target data
target_data <- weekly_summary[isoweek == target_week]
setorder(target_data, year)

if (nrow(target_data) == 0) {
  stop("No data for ISO week ", target_week)
}

# Fixed capacity: maximum total_Y0 across all years for this week
capacity_fixed <- ceiling(max(target_data$total_Y0, na.rm = TRUE))

# Compute capacity factor
target_data[, capacity_factor := total_gen / capacity_fixed]

# Date range for the week (approximate)
week_start <- ISOdate(target_year, 1, 1) + (target_week - 1) * 7 * 24 * 3600
week_start <- as.Date(week_start)
week_start <- week_start - (as.numeric(format(week_start, "%u")) - 1)
week_end <- week_start + 6

date_range <- sprintf("%s - %s, %d", 
                      format(week_start, "%b %d"),
                      format(week_end, "%b %d"),
                      target_year)

# Build report text
out_lines <- c()
out_lines <- c(out_lines, "===========================================")
out_lines <- c(out_lines, "         WEEKLY PERFORMANCE REPORT         ")
out_lines <- c(out_lines, "===========================================")
out_lines <- c(out_lines, sprintf("ISO Week %d (%s)", target_week, date_range))
out_lines <- c(out_lines, sprintf("Estimated Capacity: %d kWh", capacity_fixed))
out_lines <- c(out_lines, "-------------------------------------------")
out_lines <- c(out_lines, "  Year  Generation (kWh)  Capacity Factor")
out_lines <- c(out_lines, "-------------------------------------------")

for (i in seq_len(nrow(target_data))) {
  out_lines <- c(out_lines, sprintf("  %4d  %16.1f  %15.3f",
                                    target_data$year[i],
                                    target_data$total_gen[i],
                                    target_data$capacity_factor[i]))
}

out_lines <- c(out_lines, "-------------------------------------------")
out_lines <- c(out_lines, sprintf("  Median%14.1f  %15.3f",
                                  median(target_data$total_gen),
                                  median(target_data$capacity_factor)))
out_lines <- c(out_lines, "===========================================")

# Ensure reports directory
if (!dir.exists("reports")) dir.create("reports")

# Write to file
outfile <- sprintf("reports/week_%02d_%d.txt", target_week, target_year)
writeLines(out_lines, con = outfile)

cat("Published weekly report to", outfile, "\n")
