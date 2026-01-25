# GrMtnPower Weekly Performance Report
# Generates a summary report for the most recent complete week
# Kristian Omland

source("R/utilities.r")
source("R/data-processing.r")
source("R/modeling.r")

# Check if dat_model already exists in workspace (from analyze-loss.r)
if (exists("dat_model") && "Y0" %in% names(dat_model)) {
  weekly_performance_report(dat_model)
} else {
  # Load and prepare data
  dat <- load_latest_data()
  dat <- apply_standard_transformations(dat, addSolarPos = TRUE)
  
  # Need to load the full model - source analyze-loss.r
  # Suppress output for cleaner report generation
  sink(file = "/dev/null")
  source("analyze-loss.r")
  sink()
  
  # Now generate report
  weekly_performance_report(dat_model)
}
