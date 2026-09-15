# Figure 4: heterogeneous associations with preterm-birth risk
#
# Estimates effects by maternal income, race/ethnicity, and baseline smoke
# exposure. The final publication figure added table labels during post-processing; 
# this script produces the underlying estimates and a compact raw forest plot.

source(file.path("script", "config.R"))

library(tidyverse)
library(lfe)
library(multcomp)

births <- read_rds(paths$analysis_data_final)
monthly_smoke <- read_rds(paths$monthly_smoke)

baseline_smoke <- monthly_smoke %>%
  group_by(mzip, year) %>%
  summarise(smoke_days = sum(smoke_day, na.rm = TRUE), .groups = "drop") %>%
  group_by(mzip) %>%
  summarise(baseline_smoke = mean(smoke_days, na.rm = TRUE), .groups = "drop")

data <- left_join(births, baseline_smoke, by = "mzip")
# Full-pregnancy exposure incorporates the third-trimester (last four weeks)
# component, so the sample is restricted to births at or after week 31, as in
# the trimester models (see Table 1 note).
analysis_sample <- filter(data, Final_gestwk >= 31)

# Main model used for the full-sample result. Covariates match Eq. (1)/(2):
# weather is deliberately excluded from the main specification (see Fig S4
# for the robustness check that adds it).
model_full <- felm(
  prem37 ~ exp_preg_smk37 + dumRacehisp + dumRaceblack + dumRaceasian +
    mothage + mothage2 + dumFemale + dumEduc1 + dumEduc2 + parity +
    dumForeignBorn |
    mzip_month + county_yr | 0 | mzip,
  data = analysis_sample
)

full_base_rate <- mean(analysis_sample$prem37, na.rm = TRUE)
full_beta <- summary(model_full)$coefficients["exp_preg_smk37", "Estimate"]
full_se <- summary(model_full)$coefficients["exp_preg_smk37", "Cluster s.e."]

full_result <- tibble(
  category = "Full sample",
  group = "All births",
  base_rate = full_base_rate,
  beta = full_beta,
  standard_error = full_se,
  sample_size = summary(model_full)$N
)

# Return the standard error for the sum of the base exposure coefficient and a
# group interaction. multcomp uses the clustered covariance matrix from felm.
combined_standard_error <- function(model, interaction_term) {
  contrast <- paste("exp_preg_smk37 +", interaction_term, "= 0")
  as.numeric(summary(glht(model, linfct = contrast))$test$sigma)
}

# Estimate quintile-specific effects for an arbitrary grouping variable. The
# first quintile is the reference group; interaction terms recover quintiles
# two through five.
estimate_quintile_effects <- function(data, grouping_variable, category_label) {
  grouped_data <- data %>%
    mutate(group_quintile = statar::xtile(.data[[grouping_variable]], n = 5)) %>%
    mutate(
      group2 = as.numeric(group_quintile == 2),
      group3 = as.numeric(group_quintile == 3),
      group4 = as.numeric(group_quintile == 4),
      group5 = as.numeric(group_quintile == 5)
    )

  model <- felm(
    prem37 ~ exp_preg_smk37 + exp_preg_smk37:group2 +
      exp_preg_smk37:group3 + exp_preg_smk37:group4 +
      exp_preg_smk37:group5 + mothage + mothage2 + dumFemale +
      dumEduc1 + dumEduc2 + dumRacehisp + dumRaceblack + dumRaceasian +
      parity + dumForeignBorn |
      mzip_month + county_yr | 0 | mzip,
    data = grouped_data
  )

  base_beta <- summary(model)$coefficients["exp_preg_smk37", "Estimate"]
  interaction_terms <- paste0("exp_preg_smk37:group", 2:5)
  group_betas <- c(
    base_beta,
    base_beta + summary(model)$coefficients[interaction_terms, "Estimate"]
  )
  group_standard_errors <- c(
    summary(model)$coefficients["exp_preg_smk37", "Cluster s.e."],
    vapply(interaction_terms, function(term) combined_standard_error(model, term), numeric(1))
  )

  base_rates <- grouped_data %>%
    group_by(group_quintile) %>%
    summarise(base_rate = mean(prem37, na.rm = TRUE), .groups = "drop") %>%
    arrange(group_quintile)

  tibble(
    category = category_label,
    group = as.character(1:5),
    base_rate = base_rates$base_rate,
    beta = group_betas,
    standard_error = group_standard_errors,
    sample_size = summary(model)$N
  )
}

income_results <- estimate_quintile_effects(
  analysis_sample,
  "med_inc",
  "Income quintile"
)
smoke_results <- estimate_quintile_effects(
  analysis_sample,
  "baseline_smoke",
  "Baseline smoke quintile"
)

# Race/ethnicity-specific effects. White mothers are the reference group in the
# interaction model. Births in the recovered "Other" category are excluded, as
# in the paper.
race_sample <- analysis_sample %>%
  filter(race != "5 Other", !is.na(race))

model_race <- felm(
  prem37 ~ exp_preg_smk37 * (dumRacehisp + dumRaceblack + dumRaceasian) +
    mothage + mothage2 + dumFemale + dumEduc1 + dumEduc2 + parity +
    dumForeignBorn |
    mzip_month + county_yr | 0 | mzip,
  data = race_sample
)

race_groups <- tribble(
  ~group, ~indicator, ~interaction,
  "Hispanic", "dumRacehisp", "exp_preg_smk37:dumRacehisp",
  "Black", "dumRaceblack", "exp_preg_smk37:dumRaceblack",
  "Asian", "dumRaceasian", "exp_preg_smk37:dumRaceasian",
  "White", "dumRacewhite", NA_character_
)

race_base_beta <- summary(model_race)$coefficients["exp_preg_smk37", "Estimate"]
race_results <- race_groups %>%
  rowwise() %>%
  mutate(
    category = "Mother's race",
    base_rate = mean(race_sample$prem37[race_sample[[indicator]] == 1], na.rm = TRUE),
    beta = if (is.na(interaction)) {
      race_base_beta
    } else {
      race_base_beta + summary(model_race)$coefficients[interaction, "Estimate"]
    },
    standard_error = if (is.na(interaction)) {
      summary(model_race)$coefficients["exp_preg_smk37", "Cluster s.e."]
    } else {
      combined_standard_error(model_race, interaction)
    },
    sample_size = summary(model_race)$N
  ) %>%
  ungroup() %>%
  select(category, group, base_rate, beta, standard_error, sample_size)

results <- bind_rows(full_result, income_results, race_results, smoke_results) %>%
  mutate(
    ci_low = beta + qnorm(0.025) * standard_error,
    ci_high = beta + qnorm(0.975) * standard_error,
    relative_effect = beta / base_rate,
    relative_ci_low = ci_low / base_rate,
    relative_ci_high = ci_high / base_rate,
    absolute_per_1000 = 1000 * beta,
    absolute_ci_low = 1000 * ci_low,
    absolute_ci_high = 1000 * ci_high
  )

y_positions <- rev(seq_len(nrow(results)))

      pdf(raw_figure_path("Fig4_raw.pdf"), width = 9, height = 8, useDingbats = FALSE)
            par(mar = c(4, 11, 1, 2))

            plot(
              results$relative_effect,
              y_positions,
              xlim = c(-0.002, 0.014),
              ylim = c(0.5, nrow(results) + 0.5),
              axes = FALSE,
              xlab = "Percent change in preterm-birth risk per additional smoke day",
              ylab = "",
              pch = 21,
              bg = c("black", rep("white", nrow(results) - 1))
            )
            segments(
              results$relative_ci_low,
              y_positions,
              results$relative_ci_high,
              y_positions
            )
            abline(v = 0, lwd = 0.5)
            axis(1, at = seq(-0.002, 0.014, 0.002), labels = 100 * seq(-0.002, 0.014, 0.002))
            axis(
              2,
              at = y_positions,
              labels = paste(results$category, results$group, sep = ": "),
              las = 1,
              tick = FALSE
            )

      dev.off()
