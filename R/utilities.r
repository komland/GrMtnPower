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
