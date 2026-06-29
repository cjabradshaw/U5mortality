# Copilot instructions for this repository

## Build, test, and lint

- No build system, automated test suite, or lint configuration is checked in. There is no `Makefile`, `package.json`, `renv.lock`, `DESCRIPTION`, `testthat`, `lintr`, or similar project automation.
- The repository currently centres on a single R analysis script: `scripts/U5MR Projections.R`.
- The checked-in script is not directly runnable with `Rscript` in its current form because it contains literal knitr chunk fences such as ```` ```{r} ```` inside a `.R` file.
- The script also reads CSVs by bare filename (`read.csv("Master Sheet U5MR Final.csv")`, etc.), so execution assumes the working directory contains the data files from `data/`.

## High-level architecture

- This is a single-analysis R project rather than a package or application. All logic currently lives in `scripts/U5MR Projections.R`, and all inputs live in `data/`.
- The analysis combines three source families:
  - **IF** projections from `Master Sheet U5MR Final.csv` plus births from `IF_births.csv`
  - **UNPD** baseline births and deaths from `Master Sheet UNPD.csv` and `Master Sheet UNPD World.csv`
  - **IASA** births and deaths from `births.csv` and `deaths.csv`
- The script follows a fixed pipeline:
  1. Load the six CSV inputs and required tidyverse-style packages.
  2. Normalise the IASA inputs, aggregate births and deaths by scenario/area/period, and derive `u5mr`.
  3. Define hard-coded country membership vectors for `SSA`, `SAS`, and `EAP`, then use them to aggregate both UNPD and IASA regional series into `Region_group` summaries.
  4. Load the UNPD world baseline separately from `Master Sheet UNPD World.csv`.
  5. Pivot IF births from wide year columns to long format, join them onto IF U5MR rows, and derive IF deaths.
  6. Produce eight comparison plots with `ggplot2` and `ggsave`: four IF vs UNPD plots and four IASA vs UNPD plots for World, SSA, SAS, and EAP.
  7. Compute excess-death and relative-risk summary tables for selected years and cumulative 2030-2100 comparisons.

## Key conventions

- Treat the script as the canonical workflow. There are no shared helpers, packages, or secondary modules; most changes will be edits inside one file.
- Region aggregation is hard-coded. `SSA`, `SAS`, and `EAP` membership comes from explicit country-name vectors in the script, including alias handling such as both `Swaziland` and `Eswatini`, or both `China, Hong Kong SAR` and `Hong Kong Special Administrative Region of China`. If regional logic changes, update those vectors first.
- The code uses two different regional keys:
  - `Region` for IF data and world-level IASA data
  - `Region_group` for derived UNPD and IASA regional aggregates
  Keep joins aligned with the correct key names; several joins explicitly map `Region` to `Region_group`.
- Unit conversions are part of the business logic, not cleanup:
  - UNPD births and deaths are read as spaced strings and converted to numerics.
  - IASA deaths are multiplied by `1000`, while IASA births are divided by `1000` before `u5mr` is calculated.
  - IF deaths are derived from `U5MR * 1000 * Births`.
  - Later cumulative comparisons multiply UNPD totals by `1000` again and IF births by `1000000`.
  Preserve these conversions unless you have traced the upstream units across all three source families.
- Plot generation is intentionally repetitive and manual: each region/source combination has its own filtered data frame, `ggplot` call, and `ggsave` filename such as `IF U5MR World.png` or `IASA U5MR SSA.png`.
- Output paths are implicit. `ggsave()` writes to the current working directory, not to a dedicated outputs folder.
