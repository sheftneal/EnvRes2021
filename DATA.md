# Data availability and expected inputs

## Availability

The birth records used in this study came from California Vital Records and are
subject to a data-use agreement. They cannot be redistributed through this
repository. Derived files containing or linked to individual birth records are
also excluded.

Everything else the analysis depends on — smoke plumes, PM2.5 concentrations,
income, and geographic crosswalks — is built from public sources. Two small,
non-restricted inputs in `data/`. The rest are large enough, or specific
enough to the analysis period, that this repository documents how to obtain or
rebuild them instead of hosting them.

## Public inputs committed to this repository

| Path | Contents | Source |
| --- | --- | --- |
| `data/ca_county_boundaries.rds` | California county boundary polygons (`sp::SpatialPolygonsDataFrame`) used for Figure 1's basemap. | US Census Bureau TIGER/Line county boundaries. |
| `data/fig1_smokefire/hms_smoke20080617.*` | NOAA HMS smoke-plume polygons for June 17, 2008 (Figure 1, panel A example day). | NOAA Hazard Mapping System (HMS) Fire and Smoke Product (Schroeder et al., 2008), archived daily shapefiles: https://www.ospo.noaa.gov/Products/land/hms.html |
| `data/fig1_smokefire/hms_fire20080617.*` | NOAA HMS active-fire points for the same day. | Same HMS product as above. |

`script/fig01_exposure_maps.R` reads these two shapefiles directly with `sf`
and converts them to `sp` objects for the base-graphics plotting calls used
throughout the script.

## Restricted and to-be-rebuilt inputs

[`script/config.R`](script/config.R) expects `ENVRES2021_DATA_DIR` to contain
the following recovered filenames. The "Source" column documents how each file
was originally built or where the underlying public data comes from, so an
authorized user can reconstruct them.

| Configuration entry | Expected filename | Used for | Source |
| --- | --- | --- | --- |
| `analysis_data_final` | `analysis_data_final.rds` |  Main and supplementary regression analyses | Built by merging California Vital Records birth certificates (restricted) with zip-code-level smoke and PM2.5 exposure summaries and ACS income (see below), then applying the sample restrictions in the paper's Methods (gestational age 23–41 weeks, conception-date fixed-cohort-bias trim, P.O.-box exclusions). See the Methods section of the paper for the exact variable definitions. |
| `monthly_pm25` | `epa_zip_level_pm25_monthly_data.csv` | PM2.5 exposure summaries (Figure S1, S2) | Zip-code-month aggregates of Di et al.'s 1km daily PM2.5 grid (see `pm25_grid` below). |
| `monthly_smoke` | `smoke_month_exposure_density_count.rds` | Smoke exposure summaries (Figure 4, S1, S2) | Zip-code-month aggregates of HMS smoke-day counts (see `smoke_plumes` below). |
| `zip_county_crosswalk` | `crosswalk_county_mzip.csv` | Geographic fixed effects | HUD-USPS ZIP Code Crosswalk Files (ZIP-to-county), or equivalent Census ZCTA-to-county relationship file: https://www.huduser.gov/portal/datasets/usps_crosswalk.html |
| `smoke_plumes` | `smoke_plumes.rds` | Smoke-day construction across the full study period | NOAA HMS Fire and Smoke Product (Schroeder et al., 2008), same source as the committed Figure 1 example day, extended across 2006–2012: https://www.ospo.noaa.gov/Products/land/hms.html |
| `pm25_grid` | `pm25_grid.nc` | Daily 1km PM2.5 surface concentrations used for smoke-intensity anomalies | Di et al. (2019, 2021) ensemble-based daily PM2.5 estimates, NASA SEDAC, https://doi.org/10.7927/0rvr-4538 |
| `epa_station_pm25` | `epa_station_level_pm25_data.rds` | Figure 1, panel C monitor locations | EPA Air Quality System (AQS) monitor metadata: https://www.epa.gov/aqs |
| `fig1_panel_b` | `fig1_panel_b.rds` | Figure 1, panel B (average annual smoke-days map) | Grid-cell average of `smoke_plumes` across the study period. |
| `fig1_panel_c` | `fig1_panel_c.RData` | Figure 1, panel C (average PM2.5 map) | Van Donkelaar et al. (2016) global PM2.5 surface, https://sites.wustl.edu/acag/datasets/surface-pm2-5/, plus EPA AQS station coordinates (`statloc`) for the overlaid monitor locations. |

Community-level median household income (used for income quintiles/deciles in
Figures 2, 4, and S2) comes from the American Community Survey 5-year estimates
(2007–2011) at the census-tract level, area-weighted to zip codes; it is joined
into the analysis files above rather than stored as a separate configured path.

An authorized user with access to the restricted vital records, combined with
the public sources documented above, should have what is needed to rebuild the analysis sample as described in the paper's Methods section and reproduce the regression results in `script/`. For questions please email Sam Heft-Neal (sheftneal@stanford.edu).
