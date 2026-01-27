#!/usr/bin/env Rscript
# Generate weekly performance summary data and save as RDS
# This creates the data file; formatting/reporting is separate

# Load environment
source("R/utilities.r")
source("R/data-processing.r")

# If dat_model not present, run analyze-loss.r
if (!exists("dat_model")) {
  source("analyze-loss.r")
}

library(data.table)

# Ensure ISO week and year columns exist
if (!"isoweek" %in% names(dat_model)) dat_model[, isoweek := isoweek(dateTime)]
if (!"year" %in% names(dat_model)) dat_model[, year := year(dateTime)]

# Calculate weekly summaries for all years
weekly_summary <- dat_model[Y0 > 0, .(
  total_Y0 = sum(Y0, na.rm = TRUE),
  total_gen = sum(generation, na.rm = TRUE)
), by = .(year, isoweek)]

# Save to data directory
if (!dir.exists("data")) dir.create("data")
saveRDS(weekly_summary, "data/weekly_summary.RDS")

cat("Saved weekly summary data to data/weekly_summary.RDS\n")
cat("Rows:", nrow(weekly_summary), "\n")
cat("ISO weeks covered:", length(unique(weekly_summary$isoweek)), "\n")
cat("Years covered:", paste(sort(unique(weekly_summary$year)), collapse = ", "), "\n")
