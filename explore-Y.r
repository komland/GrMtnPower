# Explore Y patterns
# Kristian Omland

library(data.table)
library(lubridate)
library(lattice)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Plot 1: Peak Y by day of year
cat("Creating Plot 1: Peak Y by day throughout the year...\n")

# Get theoretical maximum Y for each day of year
daily_Y <- dat_model[, .(
  peak_Y = max(Y, na.rm = TRUE)
), by = .(doy_original = yday(dateForm))]

# Get maximum observed generation by day of year (across all years)
daily_actual <- dat_model[, .(
  peak_actual = max(generation, na.rm = TRUE)
), by = .(doy_original = yday(dateForm))]

# Merge
daily_peak <- merge(daily_Y, daily_actual, by = "doy_original")

# Shift to winter solstice year
daily_peak[, doy_solstice := fifelse(doy_original >= 355, 
                                      doy_original - 354,
                                      doy_original + 11)]

# Sort
setorder(daily_peak, doy_solstice)

# Reshape for plotting
daily_peak_long <- melt(daily_peak, 
                        id.vars = "doy_solstice",
                        measure.vars = c("peak_Y", "peak_actual"),
                        variable.name = "series",
                        value.name = "peak")

daily_peak_long[, series := fifelse(series == "peak_Y", 
                                     "Theoretical Upper Limit (Y)",
                                     "Maximum Observed Generation")]

# Plot
png("plots/Y_seasonal_envelope.png", width = 1000, height = 600)
print(xyplot(peak ~ doy_solstice, groups = series, daily_peak_long,
             type = "l",
             lwd = 2,
             auto.key = list(space = "right", lines = TRUE, points = FALSE),
             xlab = "Days Since Winter Solstice",
             ylab = "Peak Generation (kWh)",
             main = "Theoretical vs Observed Peak Generation (Winter Solstice to Winter Solstice)",
             scales = list(x = list(at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366),
                                   labels = c("Dec 21", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec 21"))),
             panel = function(x, y, ...) {
               panel.grid(h = -1, v = -1, col = "gray90")
               panel.xyplot(x, y, ...)
             }))
dev.off()

# Plot 2: Y by hour on solstices and equinoxes
cat("\nCreating Plot 2: Y by hour on solstices and equinoxes...\n")

# Define representative dates (using 2023 as example year)
key_dates <- data.table(
  date = as.IDate(c("2023-03-20", "2023-06-21", "2023-09-23", "2023-12-21")),
  label = c("March", "June", "September", "December")
)

# Extract data for these dates
hourly_patterns <- dat_model[dateForm %in% key_dates$date, 
                              .(dateForm, hour = hour(dateTime), Y)]

# Merge with labels
hourly_patterns <- merge(hourly_patterns, key_dates, by.x = "dateForm", by.y = "date")

# Plot
png("plots/Y_diurnal_patterns.png", width = 1000, height = 600)
print(xyplot(Y ~ hour, groups = label, hourly_patterns,
             type = "l",
             lwd = 2,
             auto.key = list(space = "right", lines = TRUE, points = FALSE, title = "Month"),
             xlab = "Hour of Day",
             ylab = "Potential Generation Y (kWh)",
             main = "Diurnal Pattern of Y on Key Solar Dates",
             scales = list(x = list(at = seq(0, 23, 3))),
             panel = function(x, y, ...) {
               panel.grid(h = -1, v = -1, col = "gray90")
               panel.xyplot(x, y, ...)
             }))
dev.off()

cat("\nPlots complete!\n")
