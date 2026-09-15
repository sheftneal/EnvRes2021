# Path configuration ----------------------------------------------------------
#
# This repository does not distribute the restricted analysis data. 

project_root <- normalizePath(
  Sys.getenv("ENVRES2021_ROOT", unset = "."),
  mustWork = FALSE
)

restricted_data_dir <- Sys.getenv(
  "ENVRES2021_DATA_DIR",
  unset = file.path(project_root, "data", "restricted")
)

raw_figure_dir <- file.path(project_root, "figures", "raw")

# Public, non-restricted exposure inputs are committed directly to the repo
# under data/ (see DATA.md for provenance) rather than in the
# restricted data directory.
public_data_dir <- file.path(project_root, "data")

paths <- list(
  analysis_data_final = file.path(restricted_data_dir, "analysis_data_final.rds"),
  monthly_pm25 = file.path(
    restricted_data_dir,
    "epa_zip_level_pm25_monthly_data.csv"
  ),
  monthly_smoke = file.path(
    restricted_data_dir,
    "smoke_month_exposure_density_count.rds"
  ),
  zip_county_crosswalk = file.path(
    restricted_data_dir,
    "crosswalk_county_mzip.csv"
  ),
  ca_counties = file.path(public_data_dir, "ca_county_boundaries.rds"),
  smoke_plumes = file.path(restricted_data_dir, "smoke_plumes.rds"),
  pm25_grid = file.path(restricted_data_dir, "pm25_grid.nc"),
  epa_station_pm25 = file.path(
    restricted_data_dir,
    "epa_station_level_pm25_data.rds"
  ),
  # Fig 1 panel A example day (June 17, 2008): NOAA HMS smoke-plume polygons
  # and active-fire points, committed as public shapefiles (see DATA.md).
  fig1_panel_a_smoke = file.path(
    public_data_dir, "fig1_smokefire", "hms_smoke20080617.shp"
  ),
  fig1_panel_a_fire = file.path(
    public_data_dir, "fig1_smokefire", "hms_fire20080617.shp"
  ),
  fig1_panel_b = file.path(restricted_data_dir, "fig1_panel_b.rds"),
  fig1_panel_c = file.path(restricted_data_dir, "fig1_panel_c.RData")
)

raw_figure_path <- function(filename) {
  file.path(raw_figure_dir, filename)
}
