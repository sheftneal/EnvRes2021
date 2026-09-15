# Figure 2: distribution of pregnancy smoke exposure
#
# Summarizes smoke exposure for the full sample and by income, maternal
# race/ethnicity, and baseline PM2.5. The final publication figure was formatte in Illustrator
# after the raw R export. Requires the restricted birth-level analysis file.

source(file.path("script", "config.R"))

library(tidyverse)

data <- read_rds(paths$analysis_data_final)

# Define income and baseline-PM2.5 quintiles. Baseline PM2.5 is the average of
# the pregnancy-level exposure measure within maternal ZIP code.
data <- data %>%
  mutate(income_quintile = statar::xtile(med_inc, n = 5)) %>%
  left_join(
    data %>%
      group_by(mzip) %>%
      summarise(baseline_pm25 = mean(exp_preg_vdpm25, na.rm = TRUE), .groups = "drop"),
    by = "mzip"
  ) %>%
  mutate(pm25_quintile = statar::xtile(baseline_pm25, n = 5))

summarize_exposure <- function(grouped_data) {
  grouped_data %>%
    summarise(
      smoke_mean = mean(exp_preg_smk37, na.rm = TRUE),
      smoke_25 = quantile(exp_preg_smk37, 0.25, na.rm = TRUE),
      smoke_50 = quantile(exp_preg_smk37, 0.50, na.rm = TRUE),
      smoke_75 = quantile(exp_preg_smk37, 0.75, na.rm = TRUE),
      smoke_90 = quantile(exp_preg_smk37, 0.90, na.rm = TRUE),
      smoke_max = max(exp_preg_smk37, na.rm = TRUE),
      ptb_rate = mean(prem37, na.rm = TRUE),
      .groups = "drop"
    )
}

full_sample <- summarize_exposure(data) %>%
  mutate(category = "Full sample", group = "-")

by_income <- data %>%
  filter(!is.na(income_quintile)) %>%
  group_by(group = income_quintile) %>%
  summarize_exposure() %>%
  mutate(category = "Income quintile")

by_race <- data %>%
  filter(race != "5 Other", !is.na(race)) %>%
  group_by(group = race) %>%
  summarize_exposure() %>%
  mutate(category = "Mother's race")

by_pm25 <- data %>%
  filter(!is.na(pm25_quintile)) %>%
  group_by(group = pm25_quintile) %>%
  summarize_exposure() %>%
  mutate(category = "Baseline PM2.5 quintile")

results <- bind_rows(full_sample, by_income, by_race, by_pm25) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

y_positions <- rev(seq_len(nrow(results)))

pdf(raw_figure_path("Fig2_raw.pdf"), width = 10, height = 6, useDingbats = FALSE)
    par(mar = c(3, 10, 1, 2))

    plot(
      NA,
      xlim = c(0, 25),
      ylim = c(0.5, nrow(results) + 0.5),
      axes = FALSE,
      xlab = "Number of smoke days in pregnancy weeks 1-32",
      ylab = ""
    )
    segments(results$smoke_25, y_positions, results$smoke_75, y_positions, lwd = 6)
    segments(results$smoke_90, y_positions - 0.12, results$smoke_90, y_positions + 0.12)
    segments(0, y_positions, results$smoke_25, y_positions, lty = 2)
    segments(results$smoke_75, y_positions, results$smoke_90, y_positions, lty = 2)
    points(results$smoke_50, y_positions, pch = 21, bg = "white")
    axis(1, at = seq(0, 25, 5))
    axis(
      2,
      at = y_positions,
      labels = paste(results$category, results$group, sep = ": "),
      las = 1,
      tick = FALSE
    )

dev.off()
