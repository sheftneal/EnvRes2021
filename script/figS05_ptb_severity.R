# Figure S5: associations by preterm-birth severity and exposure period
#
# Estimates separate linear probability models for births before 37, 32, and
# 28 weeks. Exposure windows are restricted to periods observable for each
# outcome, reducing fixed-cohort bias from exposure after an early birth.

source(file.path("script", "config.R"))

library(tidyverse)
library(lfe)

data <- read_rds(paths$analysis_data_final) %>%
  filter(!is.na(exp_preg_smk32))

controls <- paste(
  "mothage + mothage2 + dumFemale + dumEduc1 + dumEduc2 +",
  "dumRacehisp + dumRaceblack + dumRaceasian + parity + dumForeignBorn"
)

# Entire-pregnancy exposure models. The <37-week outcome's exposure measure
# includes the third-trimester (last four weeks) component, so it requires
# births at week 31 or later to be fully observed (see Table 1 note).
model_37_pregnancy <- felm(
  as.formula(paste(
    "prem37 ~ exp_preg_smk37 +", controls,
    "| mzip_month + county_yr | 0 | mzip"
  )),
  data = filter(data, Final_gestwk >= 31)
)
model_32_pregnancy <- felm(
  as.formula(paste(
    "prem32 ~ exp_preg_smk32 +", controls,
    "| mzip_month + county_yr | 0 | mzip"
  )),
  data = filter(data, Final_gestwk >= 28)
)
model_28_pregnancy <- felm(
  as.formula(paste(
    "prem28 ~ exp_preg_smk28 +", controls,
    "| mzip_month + county_yr | 0 | mzip"
  )),
  data = filter(data, Final_gestwk >= 24)
)

# Joint exposure-period models.
model_37_periods <- felm(
  as.formula(paste(
    "prem37 ~ exp_tri1_smk + exp_tri2_smk + exp_lastmonth_smk +", controls,
    "| mzip_month + county_yr | 0 | mzip"
  )),
  data = filter(data, Final_gestwk >= 31)
)
model_32_periods <- felm(
  as.formula(paste(
    "prem32 ~ exp_tri1_smk + exp_tri2_smk +", controls,
    "| mzip_month + county_yr | 0 | mzip"
  )),
  data = filter(data, Final_gestwk >= 28)
)
model_28_periods <- felm(
  as.formula(paste(
    "prem28 ~ exp_tri1_smk + exp_tri2_smk28 +", controls,
    "| mzip_month + county_yr | 0 | mzip"
  )),
  data = filter(data, Final_gestwk >= 24)
)

outcome_means <- c(
  `PTB <37 weeks` = mean(data$prem37[data$Final_gestwk >= 31], na.rm = TRUE),
  `PTB <32 weeks` = mean(data$prem32[data$Final_gestwk >= 28], na.rm = TRUE),
  `PTB <28 weeks` = mean(data$prem28[data$Final_gestwk >= 24], na.rm = TRUE)
)

model_rows <- tribble(
  ~outcome, ~period, ~weeks, ~model, ~term,
  "PTB <37 weeks", "Pregnancy", "weeks 1-32", list(model_37_pregnancy), "exp_preg_smk37",
  "PTB <37 weeks", "First trimester", "weeks 1-12", list(model_37_periods), "exp_tri1_smk",
  "PTB <37 weeks", "Second trimester", "weeks 13-27", list(model_37_periods), "exp_tri2_smk",
  "PTB <37 weeks", "Third trimester", "weeks 28-32", list(model_37_periods), "exp_lastmonth_smk",
  "PTB <32 weeks", "Pregnancy", "weeks 1-28", list(model_32_pregnancy), "exp_preg_smk32",
  "PTB <32 weeks", "First trimester", "weeks 1-12", list(model_32_periods), "exp_tri1_smk",
  "PTB <32 weeks", "Second trimester", "weeks 13-28", list(model_32_periods), "exp_tri2_smk",
  "PTB <28 weeks", "Pregnancy", "weeks 1-24", list(model_28_pregnancy), "exp_preg_smk28",
  "PTB <28 weeks", "First trimester", "weeks 1-12", list(model_28_periods), "exp_tri1_smk",
  "PTB <28 weeks", "Second trimester", "weeks 13-24", list(model_28_periods), "exp_tri2_smk28"
)

results <- model_rows %>%
  mutate(
    absolute_estimate = map2_dbl(
      model,
      term,
      ~ summary(.x)$coefficients[.y, "Estimate"]
    ),
    absolute_se = map2_dbl(
      model,
      term,
      ~ summary(.x)$coefficients[.y, "Cluster s.e."]
    ),
    sample_size = map_dbl(model, ~ summary(.x)$N),
    base_rate = outcome_means[outcome],
    relative_estimate = absolute_estimate / base_rate,
    relative_se = absolute_se / base_rate,
    ci_low = relative_estimate + qnorm(0.025) * relative_se,
    ci_high = relative_estimate + qnorm(0.975) * relative_se
  )

y_positions <- rev(seq_len(nrow(results)))

      pdf(raw_figure_path("FigS5_raw.pdf"), width = 9, height = 8, useDingbats = FALSE)
            par(mar = c(4, 12, 1, 2))

            plot(
              results$relative_estimate,
              y_positions,
              xlim = c(-0.02, 0.04),
              ylim = c(0.5, nrow(results) + 0.5),
              axes = FALSE,
              xlab = "Percentage change in preterm-birth risk per additional smoke day",
              ylab = "",
              pch = 21,
              bg = ifelse(results$period == "Pregnancy", "black", "white")
            )
            segments(results$ci_low, y_positions, results$ci_high, y_positions)
            abline(v = 0, lwd = 0.5)
            axis(1, at = seq(-0.02, 0.04, 0.01), labels = 100 * seq(-0.02, 0.04, 0.01))
            axis(
              2,
              at = y_positions,
              labels = paste(results$outcome, results$period, results$weeks, sep = " — "),
              las = 1,
              tick = FALSE
            )

      dev.off()
