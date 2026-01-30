# R analysis scripts

This folder contains the R scripts used to process raw data, fit statistical models, and generate figures and tables for the manuscript.

## Data availability

The data required to run the analysis scripts are included in this repository for reproducibility. All raw and processed data have been submitted for publication in the Dryad Digital Repository (DOI: https://doi.org/10.5061/dryad.crjdfn3hh), which serves as the permanent data archive.

## Recommended run order (end-to-end)

Run the `01_process_*` scripts first to generate processed data. All remaining scripts can be run in any order.

1.  `01_process_litter_data.R`
2.  `01_process_tfall_data.R`
3.  `stats_lmers.R`
4.  `tables_tfall_litter.R`
5.  `tables_foliar.R`
6.  `figs_tfall_litter.R`
7.  `figs_tfall_with_LAI.R`
8.  `figs_supp_dry_dep.R`

> The `fns_*.R` files are sourced by the scripts above and are not typically run directly.

------------------------------------------------------------------------

## Scripts

### Data processing

-   **`01_process_litter_data.R`**\
    Reads raw litterfall mass (trap-level, monthly) and raw litter nutrient concentrations, then writes processed datasets used downstream.\
    **Outputs (CSV):**

    -   `data/litter/processed_litter_mass_by_year.csv` (annual litter mass by plot × year; Nov–Oct “litter year”)\
    -   `data/litter/processed_litter_plot_means.csv` (plot-level mean litter mass and litter nutrient fluxes; plus concentrations in parts per thousand (`*_ppt`))

-   **`01_process_tfall_data.R`**\
    Reads event-level throughfall chemistry and computes plot-level means of throughfall nutrient concentrations and deposition fluxes by:\
    1. Averaging replicate collectors within each plot × rain event, then\
    2. Averaging across rain events (equal weight per event).\
        **Output (CSV):**\

    -   `data/throughfall/processed_throughfall_plot_means.csv` (plot-level means used for LMERs, tables, and figures)

### Statistics / models

-   **`stats_lmers.R`**\
    Fits linear mixed-effects models (LMER) for fertilization effects on LAI, litter mass, litter nutrients, and throughfall nutrients. Produces Type-II ANOVA summaries (Kenward–Roger df) and runs model diagnostics (residual plots, Q–Q plots, Shapiro–Wilk tests).\
    **Primary inputs:** Processed litter + throughfall CSVs and raw throughfall data (for LAI).\
    **Primary outputs:** Word tables of Type-II ANOVA summaries from LMERs (for supplemental text), plus on-screen diagnostic plots.

### Tables

-   **`tables_tfall_litter.R`**\
    Builds summary tables for rainfall/throughfall/litter nutrient budgets, including:
    -   Nutrient concentrations (mg L⁻¹ for throughfall/rain; mg g⁻¹ for litter)\
    -   Throughfall and rainfall deposition per rain event (mg m⁻² event⁻¹)\
    -   Annual nutrient deposition (kg ha⁻¹ y⁻¹)\
    -   Throughfall canopy-to-soil transfers (throughfall deposition − rain deposition), calculated per rain event and scaled to annual values\
        **Outputs:** Word tables of nutrient budgets by focal nutrient addition (for main text) and by factorial fertilization treatment (for supplemental text).
-   **`tables_foliar.R`**\
    Fits linear mixed-effects models (LMER) for fertilization effects on foliar nutrient concentrations (N, P, K) in understory and canopy leaves, and builds summary tables.\
    **Outputs:** Word tables of foliar N, P, K concentrations by focal nutrient addition (for main text) and by factorial treatment (for supplemental text), plus Type-II ANOVA tables from foliar LMERs (for supplemental text).

### Figures

-   **`figs_tfall_litter.R`**\
    Generates primary figures for fertilization effects on throughfall and litterfall nutrients.\
    **Outputs:** Figure files (see script for filenames/paths).

-   **`figs_tfall_with_LAI.R`**\
    Generates multi-paneled scatterplots of throughfall nutrient concentrations (for main text) and fluxes (for supplemental text) as a function of leaf area index (LAI).\
    **Outputs:** Figure files (see script for filenames/paths).

-   **`figs_supp_dry_dep.R`**\
    Generates supplemental figures (phosphate vs. total P concentration in throughfall; leaf area index as a function of fertilization treatment) and performs supplemental text calculations to estimate dry deposition using the sodium ion tracer method.\
    **Outputs:** Supplemental figure files.

------------------------------------------------------------------------

## Shared function files (sourced)

-   **`fns_data_prep.R`**\
    Data-cleaning helpers and factor standardization (e.g., `factorize_tfall_columns()`, `factorize_litter_columns()`).

-   **`fns_tables.R`**\
    Table construction and formatting utilities (e.g., section headers, mean ± SE formatting, Word export helpers).

-   **`fns_stats.R`**\
    Statistical helpers used across scripts (e.g., model wrappers like `make_lmer()`, diagnostics like `diagnostic_plot()`, model extractors like `get_model()`).

-   **`fns_plotting.R`**\
    Plotting helpers (themes, axis formatting, shared geoms/labels, etc.).

------------------------------------------------------------------------

## Notes / conventions

-   Scripts assume the project root is set via `{here}`; run from the project root so relative paths resolve correctly.
-   Units:
    -   Concentrations: mg L⁻¹ (throughfall/rain), mg g⁻¹ (foliar/litter)
    -   Event deposition: mg m⁻² event⁻¹
    -   Annual fluxes: kg ha⁻¹ y⁻¹ (via scaling using annual rainfall where applicable)

------------------------------------------------------------------------

## Reproducibility

For a clean run, start a fresh R session and execute the scripts in the recommended order above. Many scripts begin by clearing the workspace when run interactively to avoid reliance on cached objects. 

This project uses {renv}. Run renv::restore() to recreate the package environment.

------------------------------------------------------------------------

## Acknowledgements

Portions of the R code and code comments were edited with assistance from ChatGPT (OpenAI) to improve clarity and organization; all analyses, interpretations, and final code were reviewed and approved by the authors.

