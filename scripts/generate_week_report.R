#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Defaults
if (length(args) < 2) {
  target_week <- 2
  target_year <- as.integer(format(Sys.Date(), "%Y"))
} else {
  target_week <- as.integer(args[1])
  target_year <- as.integer(args[2])
}

# Load environment
source("R/utilities.r")
source("R/data-processing.r")

# If dat_model not present, run analyze-loss.r to produce it
if (!exists("dat_model")) {
  source("analyze-loss.r")
}

library(data.table)

# Build weekly summary
if (!"isoweek" %in% names(dat_model)) dat_model[, isoweek := isoweek(dateTime)]
if (!"year" %in% names(dat_model)) dat_model[, year := year(dateTime)]

weekly_summary <- dat_model[Y0 > 0, .(
  total_Y0 = sum(Y0, na.rm = TRUE),
  total_gen = sum(generation, na.rm = TRUE)
), by = .(year, isoweek)]

# Extract target data
target_data <- weekly_summary[isoweek == target_week]
setorder(target_data, year)

if (nrow(target_data) == 0) {
  stop("No data for specified ISO week")
}

capacity_fixed <- ceiling(max(target_data$total_Y0, na.rm = TRUE))

# Compute capacity factor
target_data[, capacity_factor := total_gen / capacity_fixed]

# Date range for the week (approximate)
week_start <- ISOdate(target_year, 1, 1) + (target_week - 1) * 7 * 24 * 3600
week_start <- as.Date(week_start)
week_start <- week_start - (as.numeric(format(week_start, "%u")) - 1)
week_end <- week_start + 6

# Format strings
date_range <- sprintf("%s - %s, %d", format(week_start, "%b %d"), format(week_end, "%b %d"), target_year)

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
  out_lines <- c(out_lines, sprintf("  %4d  %16.1f  %15.3f", target_data$year[i], target_data$total_gen[i], target_data$capacity_factor[i]))
}

out_lines <- c(out_lines, "-------------------------------------------")
out_lines <- c(out_lines, sprintf("  Median%14.1f  %15.3f", median(target_data$total_gen), median(target_data$capacity_factor)))
out_lines <- c(out_lines, "===========================================")

# Ensure reports dir
if (!dir.exists("reports")) dir.create("reports")
outfile <- sprintf("reports/week_%02d_%d.txt", target_week, target_year)
writeLines(out_lines, con = outfile)
cat(sprintf("Written %s\n", outfile))
