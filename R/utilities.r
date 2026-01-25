# GrMtnPower utilities module
# Shared functions for data loading, saving, and validation
# Kristian Omland

library(data.table)
library(lubridate)

#' Load the most recent RDS data file from data/ directory
#'
#' @return data.table with columns: date, consumed, generation, returnedGeneration, etc.
#' @details
#' Looks for power_data.RDS as canonical file. Falls back to timestamped backups.
load_latest_data <- function() {
  canonical <- "data/power_data.RDS"
  
  # Try canonical file first
  if (file.exists(canonical)) {
    return(readRDS(canonical))
  }
  
  # Fall back to timestamped backups (legacy or recovery)
  availableFiles <- list.files("data/", pattern = "^power_data_.*\\.RDS$")
  
  if (length(availableFiles) == 0) {
    stop("No data files found in data/ directory")
  }
  
  # Extract dates and find most recent
  fileDates <- as.numeric(gsub("power_data_(\\d{8})\\.RDS", "\\1", availableFiles))
  mostRecentIdx <- which.max(fileDates)
  filePath <- paste0("data/", availableFiles[mostRecentIdx])
  
  cat("Warning: Using backup file", filePath, "\n")
  return(readRDS(filePath))
}

#' Save data checkpoint with timestamp
#'
#' @param dat data.table to save
#' @param dateStr optional date string (default: Sys.Date() formatted as YYYYMMDD)
#' @param createBackup logical: if TRUE, also create timestamped backup (default: TRUE)
#' @return invisibly returns the filepath saved
#' @details
#' Saves as "data/power_data.RDS" (canonical) and optionally "data/power_data_{YYYYMMDD}.RDS" (backup)
save_data_checkpoint <- function(dat, dateStr = NULL, createBackup = TRUE) {
  if (is.null(dateStr)) {
    dateStr <- gsub("-", "", as.character(Sys.Date()))
  }
  
  canonicalPath <- "data/power_data.RDS"
  
  # Save canonical file
  saveRDS(dat, file = canonicalPath)
  cat("Saved canonical dataset:", canonicalPath, "\n")
  
  # Create timestamped backup
  if (createBackup) {
    backupPath <- paste0("data/power_data_", dateStr, ".RDS")
    saveRDS(dat, file = backupPath)
    cat("Saved timestamped backup:", backupPath, "\n")
  }
  
  return(invisible(canonicalPath))
}

#' Verify column agreement and compatibility
#'
#' @param dat_old data.table with existing structure
#' @param dat_new data.table with new data
#' @param allowedMissing optional vector of column names that may be missing from dat_new
#' @return TRUE if compatible, stops with error if not
#' @details
#' Checks that dat_new has all columns from dat_old (except allowedMissing).
#' Useful before rbind operations.
validate_column_agreement <- function(dat_old, dat_new, allowedMissing = NULL) {
  missing <- setdiff(names(dat_old), names(dat_new))
  
  if (length(missing) > 0) {
    if (!all(missing %in% allowedMissing)) {
      stop("Missing columns in new data: ", paste(missing, collapse = ", "),
           "\nExpected columns: ", paste(names(dat_old), collapse = ", "))
    }
  }
  
  return(TRUE)
}

#' Verify consumed == consumedTotal consistency
#'
#' @param dat data.table with 'consumed' and 'consumedTotal' columns
#' @return invisibly returns TRUE if consistent, stops with error if not
verify_consumed_consistency <- function(dat) {
  if (!all(dat[, consumed] == dat[, consumedTotal], na.rm = TRUE)) {
    stop("Inconsistency: 'consumed' != 'consumedTotal'")
  }
  return(invisible(TRUE))
}

#' Check for unpopulated rows (missing consumed/generation data)
#'
#' @param dat data.table with 'consumedFromGrid' and 'dateForm' columns
#' @param removeIfToday logical: if TRUE, remove missing rows only if they're from today or very recent
#' @return data.table with unpopulated rows handled or error if unexpected
check_unpopulated_rows <- function(dat, removeIfToday = TRUE) {
  missing_rows <- dat[is.na(consumedFromGrid) | is.na(generation)]
  
  if (nrow(missing_rows) == 0) {
    return(dat)
  }
  
  minMissingDate <- missing_rows[, min(dateForm, na.rm = TRUE)]
  
  if (removeIfToday && minMissingDate >= Sys.Date() - 2) {
    # Remove rows from today and yesterday (API may not have complete recent data)
    cat("Removing incomplete rows from", as.character(minMissingDate), "\n")
    return(dat[!is.na(consumedFromGrid) & !is.na(generation)])
  } else {
    stop("Unexpected unpopulated rows found with minDate = ", minMissingDate,
         " (today = ", Sys.Date(), ")")
  }
}

#' Verify no information loss when merging datasets
#'
#' @param dat_old data.table (existing data to be partially replaced)
#' @param dat_new data.table (updated data)
#' @param dat_merged data.table (result of merge)
#' @param keyCol column name to use for comparison (default: "date")
#' @return invisibly returns TRUE if no loss, stops with error if mismatch
verify_no_data_loss <- function(dat_old, dat_new, dat_merged, keyCol = "date") {
  # Extract rows from old data that should be kept
  oldRows <- dat_old[!get(keyCol) %in% dat_new[[keyCol]]]
  
  # Compare old rows that weren't updated with merged result
  setkeyv(oldRows, keyCol)
  mergedSubset <- dat_merged[get(keyCol) %in% oldRows[[keyCol]]]
  setkeyv(mergedSubset, keyCol)
  
  comparison <- all.equal(oldRows, mergedSubset, check.attributes = FALSE)
  
  if (!isTRUE(comparison)) {
    stop("Data loss detected during merge:\n", comparison)
  }
  
  return(invisible(TRUE))
}

#' Parse GMP API datetime string to POSIXct
#'
#' @param dateStr character vector in ISO 8601 format (e.g., "2025-11-05T12:00:00Z")
#' @param tz timezone for result (default: "America/New_York")
#' @return POSIXct vector
parse_gmp_datetime <- function(dateStr, tz = "America/New_York") {
  # Remove Z suffix, replace T with space, parse as POSIXct
  cleaned <- sub("Z", "", sub("T", " ", dateStr))
  return(as.POSIXct(cleaned, tz = tz))
}

#' Validate updated dataset before saving
#'
#' @param dat_new data.table with newly merged data
#' @param dat_old data.table with previous data (for comparison)
#' @return list with validation results: passed (logical), messages (character vector)
#' @details
#' Checks:
#' 1. New data has more rows than old
#' 2. Date range extended (not gaps)
#' 3. No duplicate dates
#' 4. Required columns present
#' 5. No excessive NAs in key columns
validate_updated_dataset <- function(dat_new, dat_old) {
  messages <- character()
  passed <- TRUE
  
  # Check 1: Row count increased
  if (nrow(dat_new) <= nrow(dat_old)) {
    passed <- FALSE
    messages <- c(messages, sprintf(
      "FAIL: Row count didn't increase (old: %d, new: %d)",
      nrow(dat_old), nrow(dat_new)
    ))
  } else {
    messages <- c(messages, sprintf(
      "PASS: Added %d new rows (old: %d, new: %d)",
      nrow(dat_new) - nrow(dat_old), nrow(dat_old), nrow(dat_new)
    ))
  }
  
  # Check 2: Date range extended
  old_max_date <- dat_old[, max(dateTime, na.rm = TRUE)]
  new_max_date <- dat_new[, max(dateTime, na.rm = TRUE)]
  
  if (new_max_date <= old_max_date) {
    passed <- FALSE
    messages <- c(messages, sprintf(
      "FAIL: Date range didn't extend (old max: %s, new max: %s)",
      old_max_date, new_max_date
    ))
  } else {
    messages <- c(messages, sprintf(
      "PASS: Date range extended to %s (was %s)",
      new_max_date, old_max_date
    ))
  }
  
  # Check 3: No duplicate dates
  dup_count <- dat_new[, sum(duplicated(date))]
  if (dup_count > 0) {
    passed <- FALSE
    messages <- c(messages, sprintf(
      "FAIL: Found %d duplicate dates",
      dup_count
    ))
  } else {
    messages <- c(messages, "PASS: No duplicate dates")
  }
  
  # Check 4: Required columns present
  required_cols <- c("date", "dateTime", "dateForm", "consumedFromGrid", 
                     "generation", "returnedGeneration", "totalConsumed")
  missing_cols <- setdiff(required_cols, names(dat_new))
  if (length(missing_cols) > 0) {
    passed <- FALSE
    messages <- c(messages, sprintf(
      "FAIL: Missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  } else {
    messages <- c(messages, "PASS: All required columns present")
  }
  
  # Check 5: NA counts in key columns
  na_consumed <- dat_new[, sum(is.na(consumedFromGrid))]
  na_generation <- dat_new[, sum(is.na(generation))]
  total_rows <- nrow(dat_new)
  
  if (na_consumed / total_rows > 0.1) {
    passed <- FALSE
    messages <- c(messages, sprintf(
      "FAIL: Excessive NAs in consumedFromGrid: %d (%.1f%%)",
      na_consumed, 100 * na_consumed / total_rows
    ))
  } else {
    messages <- c(messages, sprintf(
      "PASS: Acceptable NA rate in consumedFromGrid: %d (%.1f%%)",
      na_consumed, 100 * na_consumed / total_rows
    ))
  }
  
  return(list(passed = passed, messages = messages))
}

#' Generate weekly performance report for most recent complete week
#'
#' @param dat_model data.table with Y0, generation, dateTime, and ISO week columns
#' @return invisibly returns data.table with weekly statistics
#' @details
#' Reports on the most recent complete ISO week:
#' - Total generation (kWh)
#' - Total capacity Y0 (kWh)
#' - Realized capacity factor (generation/Y0)
#' - Historical comparison: same week in prior years
weekly_performance_report <- function(dat_model) {

  # Ensure we have required columns
  if (!all(c("Y0", "generation", "dateTime") %in% names(dat_model))) {
    stop("dat_model must contain Y0, generation, and dateTime columns")
  }

  # Add ISO week and year if not present
  if (!"isoweek" %in% names(dat_model)) {
    dat_model[, isoweek := isoweek(dateTime)]
  }
  if (!"year" %in% names(dat_model)) {
    dat_model[, year := year(dateTime)]
  }

  # Find most recent complete week
  max_date <- dat_model[, max(dateTime, na.rm = TRUE)]
  current_week <- isoweek(max_date)
  current_year <- year(max_date)

  # If current week incomplete, use previous week
  hours_in_current_week <- dat_model[year == current_year & isoweek == current_week, .N]
  expected_hours <- 7 * 24

  if (hours_in_current_week < expected_hours) {
    target_date <- max_date - 7
    target_week <- isoweek(target_date)
    target_year <- year(target_date)
  } else {
    target_week <- current_week
    target_year <- current_year
  }

  # Calculate weekly summaries for all years (total Y0 and generation)
  weekly_summary <- dat_model[Y0 > 0, .(
    total_Y0 = sum(Y0, na.rm = TRUE),
    total_gen = sum(generation, na.rm = TRUE)
  ), by = .(year, isoweek)]

  # Get data for target week across all years
  target_data <- weekly_summary[isoweek == target_week]
  setorder(target_data, year)

  if (nrow(target_data) == 0) {
    cat("ERROR: No data found for target week\n")
    return(invisible(NULL))
  }

  # Determine fixed capacity: maximum total_Y0 across all years for this week
  capacity_fixed <- ceiling(max(target_data$total_Y0, na.rm = TRUE))

  # Compute capacity factor for each year using the fixed capacity
  target_data[, capacity_factor := total_gen / capacity_fixed]

  # Get date range for the week (approximate)
  # ISO weeks run Monday-Sunday
  week_start <- ISOdate(target_year, 1, 1) + (target_week - 1) * 7 * 24 * 3600
  week_start <- as.Date(week_start)
  # Adjust to nearest Monday
  week_start <- week_start - (as.numeric(format(week_start, "%u")) - 1)
  week_end <- week_start + 6
  
  # Format date range with year
  date_range <- sprintf("%s - %s, %d", 
                        format(week_start, "%b %d"),
                        format(week_end, "%b %d"),
                        target_year)

  # Print report in compact format
  cat("===========================================\n")
  cat("         WEEKLY PERFORMANCE REPORT         \n")
  cat("===========================================\n")
  cat(sprintf("ISO Week %d (%s)\n", target_week, date_range))
  cat(sprintf("Estimated Capacity: %d kWh\n", capacity_fixed))
  cat("-------------------------------------------\n")
  cat("  Year  Generation (kWh)  Capacity Factor\n")
  cat("-------------------------------------------\n")

  # Print all years
  for (i in 1:nrow(target_data)) {
    cat(sprintf("  %4d  %16.1f  %15.3f\n",
                target_data$year[i],
                target_data$total_gen[i],
                target_data$capacity_factor[i]))
  }

  # Print median
  cat("-------------------------------------------\n")
  cat(sprintf("  Median%14.1f  %15.3f\n",
              median(target_data$total_gen),
              median(target_data$capacity_factor)))
  cat("===========================================\n\n")

  return(invisible(target_data))
}
