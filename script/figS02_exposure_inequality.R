# Figure S2: smoke and total PM2.5 exposure by income and race/ethnicity
#
# Produces the two 10-by-4 matrices used for the published heatmaps. Values are
# means within maternal-income decile and race/ethnicity group.

source(file.path("script", "config.R"))
source(file.path("script", "plotting_helpers.R"))

library(tidyverse)

births <- read_rds(paths$analysis_data_final)
pm25_monthly <- read_csv(paths$monthly_pm25)
smoke_monthly <- read_rds(paths$monthly_smoke)

baseline_exposure <- left_join(pm25_monthly, smoke_monthly) %>%
  filter(!is.na(pm25), !is.na(smoke_day)) %>%
  group_by(mzip, year) %>%
  summarise(
    smoke_day = sum(smoke_day),
    pm25 = mean(pm25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(mzip) %>%
  summarise(
    baseline_smoke = mean(smoke_day),
    baseline_pm25 = mean(pm25, na.rm = TRUE),
    .groups = "drop"
  )

plot_data <- left_join(births, baseline_exposure, by = "mzip") %>%
  mutate(income_decile = statar::xtile(med_inc, n = 10)) %>%
  filter(race != "5 Other", !is.na(race), !is.na(income_decile)) %>%
  group_by(income_decile, race) %>%
  summarise(
    smoke = mean(baseline_smoke, na.rm = TRUE),
    pm25 = mean(baseline_pm25, na.rm = TRUE),
    .groups = "drop"
  )

race_order <- c("1 White", "2 Hisp", "3 Black", "4 Asian")

make_exposure_matrix <- function(data, value_column) {
  matrix_data <- data %>%
    select(income_decile, race, value = all_of(value_column)) %>%
    mutate(race = factor(race, levels = race_order)) %>%
    arrange(income_decile, race) %>%
    tidyr::pivot_wider(names_from = race, values_from = value) %>%
    arrange(income_decile)

  result <- as.matrix(matrix_data[, -1])
  rownames(result) <- paste0("income_", matrix_data$income_decile)
  colnames(result) <- c("White", "Hispanic", "Black", "Asian")
  result
}

smoke_matrix <- make_exposure_matrix(plot_data, "smoke")
pm25_matrix <- make_exposure_matrix(plot_data, "pm25")

# Match the color limits used in the recovered plotting code.
smoke_matrix <- pmin(pmax(smoke_matrix, 8), 20)
pm25_matrix <- pmin(pmax(pm25_matrix, 9), 15)

palette_function <- colorRampPalette(
  add.alpha(c("#404096", "#63AD99", "#BEBC48", "#E66B33", "#D92120"), 0.5)
)

png(raw_figure_path("FigS2_raw.png"), width = 1194, height = 692, res = 150)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

plot(raster::raster(smoke_matrix), col = palette_function(256), axes = FALSE)
segments(y0 = 0, y1 = 1, x0 = seq(0.25, 1, 0.25))
axis(2, at = seq(0.1, 1, 0.1) - 0.05, labels = 10:1, las = 2, tick = FALSE)
text(x = seq(0.125, 0.875, 0.25), y = 1.05, labels = colnames(smoke_matrix))
title("Smoke")

plot(raster::raster(pm25_matrix), col = palette_function(256), axes = FALSE)
segments(y0 = 0, y1 = 1, x0 = seq(0.25, 1, 0.25))
axis(2, at = seq(0.1, 1, 0.1) - 0.05, labels = 10:1, las = 2, tick = FALSE)
text(x = seq(0.125, 0.875, 0.25), y = 1.05, labels = colnames(pm25_matrix))
title(expression("Total " * PM[2.5]))

dev.off()
