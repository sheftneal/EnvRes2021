# Figure 3: associations by pregnancy period
#
# All-smoke timing models used for panel (a)

source(file.path("script", "config.R"))

library(tidyverse)
library(lfe)
library(multcomp)

data <- read_rds(paths$analysis_data_final)
# Third-trimester exposure (last four weeks before birth) requires the birth
# to occur at week 31 or later (4 weeks into trimester 3).
analysis_sample <- filter(data, Final_gestwk >= 31)

# Entire-pregnancy exposure model. ZIP-by-month and county-by-year fixed effects
# match the main specification; standard errors are clustered by maternal ZIP.
model_pregnancy <- felm(
  prem37 ~ exp_preg_smk37 + mothage + mothage2 + dumFemale + dumEduc1 +
    dumEduc2 + dumRacehisp + dumRaceblack + dumRaceasian + parity +
    dumForeignBorn | mzip_month + county_yr | 0 | mzip,
  data = analysis_sample
)

# Trimester-specific exposures enter jointly. The third-trimester measure is
# limited to the final month to avoid conditioning on post-birth exposure.
model_trimester <- felm(
  prem37 ~ exp_tri1_smk + exp_tri2_smk + exp_lastmonth_smk + mothage +
    mothage2 + dumFemale + dumEduc1 + dumEduc2 + dumRacehisp + dumRaceblack +
    dumRaceasian + parity + dumForeignBorn |
    mzip_month + county_yr | 0 | mzip,
  data = analysis_sample
)

outcome_mean <- mean(analysis_sample$prem37, na.rm = TRUE)
periods <- c("Entire pregnancy", "First trimester", "Second trimester", "Third trimester")
coefficient_names <- c(
  "exp_preg_smk37",
  "exp_tri1_smk",
  "exp_tri2_smk",
  "exp_lastmonth_smk"
)
models <- c(list(model_pregnancy), rep(list(model_trimester), 3))

results <- tibble(
  period = periods,
  estimate = purrr::map2_dbl(
    models,
    coefficient_names,
    ~ summary(.x)$coefficients[.y, "Estimate"] / outcome_mean
  ),
  standard_error = purrr::map2_dbl(
    models,
    coefficient_names,
    ~ summary(.x)$coefficients[.y, "Cluster s.e."] / outcome_mean
  )
) %>%
  mutate(
    ci_low = estimate + qnorm(0.025) * standard_error,
    ci_high = estimate + qnorm(0.975) * standard_error
  )

# Pairwise tests among trimester coefficients
trimester_comparisons <- c(
  first_vs_second = as.numeric(summary(glht(model_trimester, "exp_tri1_smk - exp_tri2_smk = 0"))$test$tstat),
  first_vs_third = as.numeric(summary(glht(model_trimester, "exp_tri1_smk - exp_lastmonth_smk = 0"))$test$tstat),
  second_vs_third = as.numeric(summary(glht(model_trimester, "exp_tri2_smk - exp_lastmonth_smk = 0"))$test$tstat)
)

pdf(raw_figure_path("Fig3_panel_a_raw.pdf"), width = 6, height = 5, useDingbats = FALSE)
        par(mar = c(5, 4, 2, 2))

        plot(
          seq_len(nrow(results)),
          results$estimate,
          ylim = c(-0.01, 0.01),
          axes = FALSE,
          xlab = "",
          ylab = "Percent change in PTB risk per smoke day",
          pch = 21,
          bg = c("black", rep("white", 3)),
          cex = 1.5
        )
        segments(seq_len(nrow(results)), results$ci_low, seq_len(nrow(results)), results$ci_high)
        abline(h = 0, lwd = 0.5)
        axis(1, at = seq_len(nrow(results)), labels = results$period, las = 2)
        axis(2, at = seq(-0.01, 0.01, 0.0025), labels = 100 * seq(-0.01, 0.01, 0.0025), las = 2)

dev.off()
