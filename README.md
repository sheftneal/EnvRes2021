# Wildfire smoke exposure and preterm birth in California

This repository contains analysis and figure code for:

> Heft-Neal S, Driscoll A, Yang W, Shaw G, and Burke M. (2022).
> [Associations between wildfire smoke exposure during pregnancy and risk of
> preterm birth in California](https://doi.org/10.1016/j.envres.2021.111872).
> *Environmental Research*, 203, 111872.

## Scope and reproducibility

The individual-level California Vital Records data used in the study are
restricted by a data-use agreement and cannot be redistributed. Consequently,
this repository documents the analyses but is not a self-contained replication
package. 

The files in [`figures/clean`](figures/clean) are the final publication figures.
Some were assembled or edited for aesthetics after export from R using Adobe Illustrator. The scripts write "raw" versions and the "clean" figures are left untouched.

## Repository contents

- [`script/`](script) contains one script per paper
  figure, shared plotting helpers, and path configuration.
- [`figures/clean/`](figures/clean) contains the final published figures.
- [`figures/raw/`](figures/raw) is the intended destination for unedited R
  output.
- `data/` is git-ignored except for two small, public, non-restricted files
  used by Figure 1 (California county boundaries and the NOAA HMS smoke/fire
  example day).


## Code organization

| Paper output | Script | Description |
| --- | --- | --- |
| Figure 1 | `script/fig01_exposure_maps.R` | Smoke-plume example and spatial exposure maps |
| Figure 2 | `script/fig02_exposure_summary.R` | Exposure distributions by income, race/ethnicity, and baseline PM2.5 |
| Figure 3 | `script/fig03_timing_and_intensity.R` | Associations by pregnancy period and smoke intensity |
| Figure 4 | `script/fig04_subgroup_effects.R` | Heterogeneity by income, race/ethnicity, and baseline smoke exposure |
| Figure 5 | — | To be added |
| Figure S1 | `script/figS01_smoke_pm25_association.R` | Cross-sectional and panel smoke–PM2.5 relationships |
| Figure S2 | `script/figS02_exposure_inequality.R` | Exposure by income decile and race/ethnicity |
| Figure S3 | - | To be added |
| Figure S4 | `script/figS04_specification_checks.R` | Alternative model specifications |
| Figure S5 | `script/figS05_ptb_severity.R` | Results by preterm-birth severity |


## Software

The analysis was originally written in R (3.5.3) and used packages including
`tidyverse`, `fixest`, `lfe`, `statar`, `multcomp`, `sf`, `sp`, and the historical R spatial stack. 

All fixed-effects models were initially estimated with
`lfe::felm()` then later re-estimated with the `fixest` package to confirm consistency. `lfe` is no longer maintained on CRAN  so `fixest` (0.4.1) can be substituted and produce the same results. 

## License

Code is provided under the terms in [`LICENSE`](LICENSE). The license does not
apply to the restricted source data, which are not distributed here.
