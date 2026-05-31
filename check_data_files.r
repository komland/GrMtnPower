library(data.table)

files <- list.files('data', pattern='^power_data.*\\.RDS$', full.names=TRUE)

results <- lapply(files, function(f) {
  dat <- readRDS(f)
  data.table(
    file = basename(f),
    rows = nrow(dat),
    min_date = as.POSIXct(min(dat$date), origin='1970-01-01', tz='UTC'),
    max_date = as.POSIXct(max(dat$date), origin='1970-01-01', tz='UTC')
  )
})

comparison <- rbindlist(results)
setorder(comparison, file)
print(comparison, row.names=FALSE)

cat("\n=== Analysis ===\n")
most_recent <- comparison[file == "power_data.RDS"]
cat(sprintf("Most recent file (power_data.RDS): %d rows\n", most_recent$rows))
cat(sprintf("Date range: %s to %s\n", most_recent$min_date, most_recent$max_date))

cat("\nChecking if any older file has data outside this range...\n")
older_files <- comparison[file != "power_data.RDS"]
issues <- older_files[min_date < most_recent$min_date | max_date > most_recent$max_date]

if (nrow(issues) > 0) {
  cat("\nWARNING: These files have data outside the range of power_data.RDS:\n")
  print(issues)
} else {
  cat("\nAll older files are subsets of power_data.RDS - safe to remove backups.\n")
}
