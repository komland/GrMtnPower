# GrMtnPower utilities module
# Shared functions for data loading, saving, and validation
# Kristian Omland

#' Load the most recent RDS data file from data/ directory
#'
#' @return data.table with columns: date, consumed, generation, returnedGeneration, etc.
#' @details
#' Looks for files matching pattern "dat4Gotten*.RDS" and loads the most recent.
#' Falls back to loading any single RDS if only one exists.
load_latest_data <- function() {
  availableFiles <- list.files("data/", pattern = "^dat4Gotten.*\\.RDS$")
  
  if (length(availableFiles) == 0) {
    stop("No data files found in data/ directory")
  }
  
  if (length(availableFiles) == 1) {
    filePath <- paste0("data/", availableFiles[1])
  } else {
    # Extract date from filename (format: dat4Gotten20251105.RDS)
    fileDates <- as.numeric(substr(availableFiles, 11, 18))
    mostRecentIdx <- which.max(fileDates)
    filePath <- paste0("data/", availableFiles[mostRecentIdx])
  }
  
  return(readRDS(filePath))
}

#' Save data checkpoint with timestamp
#'
#' @param dat data.table to save
#' @param dateStr optional date string (default: Sys.Date() formatted as YYYYMMDD)
#' @return invisibly returns the filepath saved
#' @details
#' Saves as "data/dat4Gotten{YYYYMMDD}.RDS"
save_data_checkpoint <- function(dat, dateStr = NULL) {
  if (is.null(dateStr)) {
    dateStr <- gsub("-", "", as.character(Sys.Date()))
  }
  
  filePath <- paste0("data/dat4Gotten", dateStr, ".RDS")
  saveRDS(dat, file = filePath)
  cat("Saved checkpoint:", filePath, "\n")
  return(invisible(filePath))
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
  setkey(oldRows, eval(keyCol))
  mergedSubset <- dat_merged[get(keyCol) %in% oldRows[[keyCol]]]
  setkey(mergedSubset, eval(keyCol))
  
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
