# GrMtnPower main update script
# Periodically retrieve and verify meter readings from GMP API
# Kristian Omland

# Load all modules
source("R/utilities.r")
source("R/data-processing.r")
source("R/data-retrieval.r")

# Main execution: fetch latest data and save
dat_updated <- update_power_data(outputCheckpoint = TRUE)

# Optional: apply all transformations for analysis
dat_with_transforms <- apply_standard_transformations(dat_updated, addSolarPos = FALSE)

cat("\nUpdate complete!\n")
cat("Data range:", 
    min(dat_updated[, dateTime]), "to", max(dat_updated[, dateTime]), "\n")
cat("Total hourly records:", nrow(dat_updated), "\n")
