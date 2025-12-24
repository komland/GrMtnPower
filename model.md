# Model PV Generation Loss  
  
I want to model loss of potential electricity generation from the solar panels. As I envision this, for each hourly date-time stamp, `dat[,date]`, there will be a deterministic potential generation, $Y$. Observed `dat[,generation]` will be $(100\% - q) \times Y$.  
  
This model has two components: (1) a smooth function estimating potential generation $Y$ based on solar position, and (2) a loss parameter $q$ capturing all reduction mechanisms (clouds, snow, dust, equipment degradation). Both will be estimated from observed generation data.

## Potential Generation $Y$

Potential generation, $Y$, should be a realistic smooth continuous function of solar position (azimuth and zenith angles). This function will be evaluated at each hourly timestamp in the dataset.

When the sun is well below the horizon (high zenith angle), $Y$ will be zero. As the sun rises, $Y$ should gradually increase following a smooth curve. The system may have electronic regulation that prevents generation until solar irradiance exceeds a threshold, so $Y$ may remain at 0 even after sunrise or return to 0 before sunset. This threshold behavior may be asymmetrical: the system might wait until brightness exceeds some threshold in the morning but continue generating until brightness falls below a possibly different threshold in the evening.

The function should exhibit consistency across years and seasons: $Y$ should be approximately the same for identical solar positions (same azimuth and zenith) regardless of whether they occur in spring, fall, or different years. This consistency leverages the fact that solar position alone determines the geometric component of potential generation, independent of weather.

**Note:** Roof aspect and slope parameters may be needed to refine the model if solar panel orientation significantly affects the azimuth-zenith relationship to potential output.

## Annual Capacity Estimation

One key output of this model is the annual integral of $Y$, which represents a refined capacity estimate for our PV system. 

The naive theoretical capacity calculation is:
- 21 panels × 321 Watts × 2,300 hours of sunlight per year ≈ 15,500 kWh per year (15.5 MWh)

Our modeled integral of $Y$ should be less than this theoretical maximum, providing a more realistic capacity that accounts for:
- Actual solar geometry at our location
- Electronic threshold behavior
- Panel orientation (roof aspect and slope)

This refined capacity will serve as a better denominator when computing the capacity factor of our system. Additionally, the model will estimate actual "sunlight hours per year" as the number of hours when $Y > 0$, which may differ from the generic Vermont estimate of 2,300 hours.

**Analysis period:** Solar year (November 1 through October 31) to align with solar generation patterns and avoid splitting winter months across calendar years.

## Loss Parameter $q$

Loss, $q$, will be modeled using a beta distribution, taking values from 0 (100% potential generation achieved) to 1 (no generation, 0 kWh). 

$q$ serves as a catch-all for all mechanisms that reduce actual generation below potential:
- Cloud cover (primary driver of day-to-day variation)
- Snow accumulation on panels
- Dust and soiling
- Equipment degradation over time (if any trend emerges, this would be concerning and warrant investigation)

We will start with an ultra-simple model where $q$ is just a constant (mean loss across all hours). Gradually, we can add sophistication with covariates that govern $q$:
- Season (winter vs. summer patterns)
- Temperature (proxy for snow likelihood and equipment efficiency)
- Year (to detect potential equipment degradation trends)

**Note:** We do not have detailed weather condition codes in the current dataset. Future refinements could incorporate external weather data to isolate cloudiness effects, but snow cover on our specific panels cannot be directly observed.

## Data Filtering

The model will use only observations where `dat[,generation]` is available (post-solar installation, typically starting 2016). Periods where the grid was down or the PV system was offline (indicated by missing or zero generation during daylight hours) will be excluded from model fitting.

The BEV period (battery electric vehicle usage, 2016-06-01 to 2019-05-31) does not affect this analysis since we are modeling generation only, not consumption patterns.  
