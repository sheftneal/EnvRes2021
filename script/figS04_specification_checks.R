# Figure S4: alternative model specifications
#
# Compares the main linear probability model with changes to covariates and
# analytic samples. Requires the restricted version-4 analysis file.

source(file.path("script", "config.R"))

library(tidyverse)
library(lfe)

data <- read_rds(paths$analysis_data_final)

# exp_preg_smk37 aggregates the three trimester exposures, including the
# third-trimester (last four weeks) measure, so births must occur at week 31
# or later for exposure to be fully observed (see Table 1 note).

# Main specification.
model_main <- felm(
  prem37 ~ exp_preg_smk37 + mothage + mothage2 + dumFemale + dumEduc1 +
    dumEduc2 + dumRacehisp + dumRaceblack + dumRaceasian + parity +
    dumForeignBorn | mzip_month + county_yr | 0 | mzip,
  data = filter(data, Final_gestwk >= 31)
)

# Remove individual-level controls while retaining the fixed effects.
model_unadjusted <- felm(
  prem37 ~ exp_preg_smk37 | mzip_month + county_yr | 0 | mzip,
  data = filter(data, Final_gestwk >= 31)
)

# Restrict to first births (parity equal to one).
model_parity <- felm(
  prem37 ~ exp_preg_smk37 + mothage + mothage2 + dumFemale + dumEduc1 +
    dumEduc2 + dumRacehisp + dumRaceblack + dumRaceasian + dumForeignBorn |
    mzip_month + county_yr | 0 | mzip,
  data = filter(data, Final_gestwk >= 31, parity == 1)
)

# Restrict to Hispanic mothers.
model_hispanic <- felm(
  prem37 ~ exp_preg_smk37 + mothage + mothage2 + dumFemale + dumEduc1 +
    dumEduc2 + parity | mzip_month + county_yr | 0 | mzip,
  data = filter(data, Final_gestwk >= 31, dumRacehisp == 1)
)

# Add a control for self-reported smoking during pregnancy.
model_smoking <- felm(
  prem37 ~ exp_preg_smk37 + mothage + mothage2 + dumFemale + dumEduc1 +
    dumEduc2 + dumRacehisp + dumRaceblack + dumRaceasian + dumForeignBorn +
    parity + cig | mzip_month + county_yr | 0 | mzip,
  data = filter(data, Final_gestwk >= 31)
)

# Add pregnancy temperature and precipitation controls.
model_weather <- felm(
  prem37 ~ exp_preg_smk37 + exp_tmp_preg + exp_precip_preg + mothage +
    mothage2 + dumFemale + dumEduc1 + dumEduc2 + dumRacehisp + dumRaceblack +
    dumRaceasian + dumForeignBorn + parity |
    mzip_month + county_yr | 0 | mzip,
  data = filter(data, Final_gestwk >= 31)
)

models <- list(
  Main = model_main,
  Unadjusted = model_unadjusted,
  `Limited to parity` = model_parity,
  `Limited to Hispanic mothers` = model_hispanic,
  `Includes self-reported smoking` = model_smoking,
  `Includes temperature` = model_weather
)

samples <- list(
  filter(data, Final_gestwk >= 31),
  filter(data, Final_gestwk >= 31),
  filter(data, Final_gestwk >= 31, parity == 1),
  filter(data, Final_gestwk >= 31, dumRacehisp == 1),
  filter(data, Final_gestwk >= 31),
  filter(data, Final_gestwk >= 31)
)

base_rates <- vapply(samples, function(sample) mean(sample$prem37, na.rm = TRUE), numeric(1))
estimates <- vapply(
  models,
  function(model) summary(model)$coefficients["exp_preg_smk37", "Estimate"],
  numeric(1)
)
standard_errors <- vapply(
  models,
  function(model) summary(model)$coefficients["exp_preg_smk37", "Cluster s.e."],
  numeric(1)
)

results <- data.frame(
  specification = names(models),
  base_rate = base_rates,
  sample_size = vapply(models, function(model) summary(model)$N, numeric(1)),
  estimate = estimates / base_rates,
  standard_error = standard_errors / base_rates
)
results$ci_low <- results$estimate + qnorm(0.025) * results$standard_error
results$ci_high <- results$estimate + qnorm(0.975) * results$standard_error

y_positions <- rev(seq_len(nrow(results)))

      pdf(raw_figure_path("FigS4_raw.pdf"), width = 8, height = 5, useDingbats = FALSE)
            par(mar = c(3, 11, 1, 2))

            plot(
              results$estimate,
              y_positions,
              xlim = c(-0.01, 0.01),
              ylim = c(0.5, nrow(results) + 0.5),
              axes = FALSE,
              xlab = "Percent change in preterm-birth risk per additional smoke day",
              ylab = "",
              pch = 21,
              bg = "black"
            )
            segments(results$ci_low, y_positions, results$ci_high, y_positions)
            abline(v = 0, lwd = 0.5)
            axis(1, at = seq(-0.01, 0.01, 0.005), labels = 100 * seq(-0.01, 0.01, 0.005))
            axis(2, at = y_positions, labels = results$specification, las = 1, tick = FALSE)

      dev.off()
