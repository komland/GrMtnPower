# GrMtnPower data-retrieval module
# Main pipeline for fetching and merging meter data
# Kristian Omland

source("R/get-gmp-data.r")
source("R/utilities.r")
source("R/data-processing.r")

#' Retrieve and merge new hourly data into existing dataset
#'
#' @param latestDate IDate of the last date in existing data
#' @param currentDate IDate up to which to retrieve (default: Sys.time() - 1 day)
#' @param maxChunkSize maximum days to request in one API call (default: 29)
#' @return data.table with new hourly data, transformed and validated
retrieve_new_data <- function(latestDate, currentDate = NULL, maxChunkSize = 29) {
  if (is.null(currentDate)) {
    currentDate <- as.IDate(Sys.time()) - 1
  }
  
  # Check if already up-to-date
  if (latestDate >= currentDate) {
    cat("Data already up-to-date through", as.character(latestDate), "\n")
    return(NULL)
  }
  
  # Handle large date ranges by chunking
  if (difftime(currentDate, latestDate, units = "days") > maxChunkSize) {
    currentDate <- latestDate + maxChunkSize
    cat("Large date range requested; limiting to:", as.character(currentDate), "\n")
  }
  
  # Fetch from API
  cat("Fetching data from", as.character(latestDate), "to", as.character(currentDate), "\n")
  dat_new <- buildAndGet(
    intrvl = "hourly",
    strDt = latestDate,
    endDt = currentDate
  )
  
  # Remove any duplicate columns (e.g., totalEnergyUsed added summer 2024)
  if ("totalEnergyUsed" %in% names(dat_new)) {
    dat_new[, totalEnergyUsed := NULL]
  }
  
  # Transform to standard format
  dat_new <- add_datetime_columns(dat_new)
  
  # Verify consumed == consumedTotal and rename
  verify_consumed_consistency(dat_new)
  setnames(dat_new, "consumed", "consumedFromGrid")
  
  # Add derived columns
  dat_new <- add_consumption_derived(dat_new)
  
  # Handle unpopulated rows (incomplete today)
  dat_new <- check_unpopulated_rows(dat_new, removeIfToday = TRUE)
  
  return(dat_new)
}

#' Merge new data into existing dataset with validation
#'
#' @param dat_old data.table with existing hourly data
#' @param dat_new data.table with newly retrieved hourly data
#' @return data.table with merged data (newer rows from dat_new take precedence)
#' @details
#' Keeps rows from dat_old that aren't in dat_new (fresher updates for overlaps).
#' Verifies no data loss during merge.
merge_and_validate <- function(dat_old, dat_new) {
  # Check column compatibility
  validate_column_agreement(dat_old, dat_new, allowedMissing = "temperature")
  
  # Merge: keep old rows not in new, add all new rows
  dat_merged <- rbind(
    dat_old[!date %chin% dat_new[, date]],  # old rows not in new
    dat_new,
    fill = TRUE
  )
  
  # Verify no data loss
  verify_no_data_loss(dat_old, dat_new, dat_merged, keyCol = "date")
  
  return(dat_merged)
}

#' Main retrieval and update pipeline with validation safeguards
#'
#' @param outputCheckpoint logical: save checkpoint file after successful merge
#' @param validateBeforeSave logical: run validation checks before saving (default: TRUE)
#' @return data.table with merged and validated hourly data
#' @details
#' Safe update workflow:
#' 1. Load latest existing data
#' 2. Retrieve new hourly data from API (since last date)
#' 3. Merge and validate
#' 4. Run validation checks (if enabled)
#' 5. Save canonical + backup (if validation passes)
#' 6. Return merged data
update_power_data <- function(outputCheckpoint = TRUE, validateBeforeSave = TRUE) {
  cat("Loading existing data...\n")
  dat_old <- load_latest_data()
  
  cat("Retrieving new hourly data from API...\n")
  latestDate <- as.IDate(dat_old[.N, dateTime])
  dat_new <- retrieve_new_data(latestDate)
  
  # If no new data, return existing data
  if (is.null(dat_new)) {
    cat("No new data to retrieve. Dataset is current.\n")
    return(dat_old)
  }
  
  cat("Merging and validating...\n")
  dat_merged <- merge_and_validate(dat_old, dat_new)
  
  # Run comprehensive validation
  if (validateBeforeSave) {
    cat("\nRunning validation checks...\n")
    validation <- validate_updated_dataset(dat_merged, dat_old)
    
    # Print validation results
    for (msg in validation$messages) {
      cat("  ", msg, "\n")
    }
    
    if (!validation$passed) {
      stop("\nValidation FAILED. Data not saved. Review errors above.")
    }
    
    cat("\nValidation PASSED ✓\n")
  }
  
  if (outputCheckpoint) {
    save_data_checkpoint(dat_merged, createBackup = TRUE)
  }
  
  cat("Update complete. Total rows:", nrow(dat_merged), "\n")
  return(dat_merged)
}
