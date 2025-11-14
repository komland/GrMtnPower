# GrMtnPower

Retrieve, process, visualize, and model residential solar generation and grid consumption data from Green Mountain Power's API.

## Overview

This project retrieves hourly meter data from a residential solar + storage system in Vermont, validates it, and provides tools for visualization and analysis.

**Data:** Hourly meter readings (generation, consumption, grid returns) since October 2015.

## Setup

### Prerequisites

- R 4.5.2 or later
- `renv` for package management
- GMP API credentials (account number, API key ID, and secret)

### Installation

1. Clone the repository:
   ```bash
   git clone <repo-url>
   cd GrMtnPower
   ```

2. Restore the R environment:
   ```r
   renv::restore()
   ```

3. Create a `credentials.txt` file in the project root with three lines:
   ```
   GMPaccount: <your-account-number>
   apiKeyId: <your-api-key-id>
   apiKeySecret: <your-api-secret>
   ```
   (This file is git-ignored for security.)

4. Ensure hourly data is available in `data/` directory. You can retrieve an initial dataset by running `dataRetrievalShell.r` for historical backfill.

## Project Structure

### Core Modules (`R/`)

**`utilities.r`**
- `load_latest_data()` — Load the most recent RDS checkpoint
- `save_data_checkpoint()` — Save data with timestamp
- `validate_column_agreement()` — Check data structure compatibility
- `verify_consumed_consistency()` — Validate data integrity
- `parse_gmp_datetime()` — Parse ISO 8601 datetime strings
- Other validation helpers

**`data-processing.r`**
- `add_datetime_columns()` — Parse API datetime strings
- `add_solar_year()` — Add solar calendar (Nov–Oct year)
- `add_bev_flag()` — Mark battery storage system period
- `add_consumption_derived()` — Compute consumedGeneration, totalConsumed
- `add_solar_position()` — Compute azimuth/zenith using solarPos package
- `aggregate_daily()` — Sum hourly data to daily totals
- `aggregate_weekly()` — Sum hourly data to weekly totals
- `get_last_n_days()` — Slice recent data
- `apply_standard_transformations()` — Run all transforms in sequence

**`data-retrieval.r`**
- `retrieve_new_data()` — Fetch new hourly data from GMP API
- `merge_and_validate()` — Merge new data with existing, verify integrity
- `update_power_data()` — Main pipeline: load → fetch → merge → save

**`visualizations.r`**
- `setup_gmp_colors()` — Initialize colorblind-safe lattice palette
- `plot_daily_summary()` — Time series of daily generation/consumption
- `plot_last_n_days()` — Recent month with gridlines
- `plot_past_year()` — 12-month overview
- `plot_monthly_by_year()` — Single month across years
- `plot_weekly_by_year()` — ISO weeks across years
- `plot_annual_consumption_ratio()` — Rolling 365-day consumption %
- `plot_solar_position_generation()` — 3D scatter: generation vs azimuth/zenith
- `plot_generation_by_hour()` — Hourly pattern by season

**`modeling.r`**
- `fit_solar_generation_model()` — GAM: generation ~ s(azimuth, zenith)
- `fit_temporal_generation_model()` — GAM: generation ~ s(hour, day)
- `solar_position_statistics()` — Bin generation by azimuth/zenith
- `generation_by_hour_statistics()` — Hourly generation profile
- `generation_by_season_statistics()` — Seasonal generation profile
- `predict_solar_generation_grid()` — Prediction surface for plotting
- `model_summary_report()` — Formatted model diagnostics

### Execution Scripts (Root)

**`update-main.r`** — Retrieve and update data
- Fetches new hourly data from API
- Merges with existing data
- Validates integrity
- Saves checkpoint
- Run: `source("update-main.r")`

**`visualize-main.r`** — Standard visualizations
- Plots full history (daily aggregation)
- Plots past year (daily aggregation)
- Plots recent month (daily detail)
- Run: `source("visualize-main.r")`

**`analyze-solar.r`** — Solar modeling and analysis
- Fits GAM model to solar position
- Computes statistics by hour/season
- Generates solar analysis report
- Run: `source("analyze-solar.r")`

### Utility Scripts

**`getGMPdata.r`** — Low-level API client
- `buildAndGet()` — Construct API request and fetch hourly data
- Used by `data-retrieval.r`; rarely called directly

**`dataRetrievalShell.r`** — Legacy bulk backfill
- Retrieve historical data for date ranges
- Use for initial setup only

### Data

**`data/dat4Gotten*.RDS`**
- Checkpoints of processed hourly data
- Format: data.table with columns:
  - `date`: ISO 8601 string (original API field)
  - `dateTime`: POSIXct (local America/New_York)
  - `dateForm`: IDate
  - `consumedFromGrid`, `generation`, `returnedGeneration` (kWh)
  - `consumedGeneration`, `totalConsumed` (derived)
  - `solarYear`: Solar calendar year (Nov–Oct)
  - `bev`: Flag for battery system period (2016-06 to 2019-05)
  - `temperature`: Temperature (sparse, from API)

---

## Workflows

### 1. Routine Update (Weekly/Monthly)

```r
# Retrieve latest data from API and save checkpoint
source("update-main.r")
```

### 2. Quick Visualization

```r
# View daily/weekly aggregations
source("visualize-main.r")
```

### 3. Detailed Solar Analysis

```r
# Fit models, compute statistics, analyze generation patterns
source("analyze-solar.r")

# Models and data now available in environment:
# fm_solar, solar_stats, hourly_stats, seasonal_stats, pred_grid
```

### 4. Custom Analysis

```r
# Load and transform data
source("R/utilities.r")
source("R/data-processing.r")

dat <- load_latest_data()
dat <- apply_standard_transformations(dat, addSolarPos = TRUE)

# Work with dat...
```

---

## Data Notes

### DST and Hourly Records

- Local time (America/New_York) with DST applied
- Spring forward: 2:00 AM → 3:00 AM (missing hour, visible as 23 records on that day)
- Fall back: 2:00 AM occurs twice (not currently captured; GMP API may not populate the duplicate)

### Generation Data

- PV system active from 2015-10-27 onward
- Battery storage (BEV) system active 2016-06 to 2019-05
- Some hours have NA generation before 2015-10-27

### Validation

- `updateData.r` / `update_power_data()` performs extensive validation:
  - Checks `consumed == consumedTotal` consistency
  - Removes incomplete today's data
  - Verifies no information loss during merge
  - Safe to re-run on overlapping date ranges

---

## Troubleshooting

### Missing or out-of-sync packages

```r
renv::status()
renv::restore()
```

### API call fails

- Check `credentials.txt` is present and formatted correctly
- Verify API key still valid (GMP may require renewal)

### Solar position calculations

- Requires `solarPos` package and `lubridate`
- Computation happens only for rows with non-NA generation
- Results in new columns: `JD`, `zenith`, `azimuth`

---

## Acknowledgments

Solar position calculations use the `solarPos` R package.

Data retrieval built on Green Mountain Power's publicly documented API.

---

## License & Attribution

Kristian Omland, 2019–2025

See `getGMPdata.r` header for copyright notice and usage information.
