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
#' @param maxChunkSize maximum days to request in one API call (default: 30)
#' @return data.table with new hourly data, transformed and validated
retrieve_new_data <- function(latestDate, currentDate = NULL, maxChunkSize = 30) {
  if (is.null(currentDate)) {
    currentDate <- as.IDate(Sys.time()) - 1
  }
  
  # Handle large date ranges by chunking
  if (difftime(currentDate, latestDate, units = "days") > maxChunkSize) {
    currentDate <- latestDate + maxChunkSize
    cat("Large date range requested; limiting to:", as.character(currentDate), "\n")
  }
  
  # Fetch from API
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

#' Main retrieval and update pipeline
#'
#' @param outputCheckpoint logical: save checkpoint file after successful merge
#' @return data.table with merged and validated hourly data
#' @details
#' 1. Load latest existing data
#' 2. Retrieve new hourly data from API (since last date)
#' 3. Merge and validate
#' 4. Save checkpoint (optional)
#' 5. Return merged data
update_power_data <- function(outputCheckpoint = TRUE) {
  cat("Loading existing data...\n")
  dat_old <- load_latest_data()
  
  cat("Retrieving new hourly data from API...\n")
  latestDate <- as.IDate(dat_old[.N, dateTime])
  dat_new <- retrieve_new_data(latestDate)
  
  cat("Merging and validating...\n")
  dat_merged <- merge_and_validate(dat_old, dat_new)
  
  if (outputCheckpoint) {
    save_data_checkpoint(dat_merged)
  }
  
  cat("Update complete. Total rows:", nrow(dat_merged), "\n")
  return(dat_merged)
}
