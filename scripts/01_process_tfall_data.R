
## Plot-level throughfall nutrient summaries
##
## Computes plot-level means of throughfall nutrient concentrations
## (mg L⁻¹) and deposition fluxes (mg m⁻² per storm) from event-level data.
##
## Processing steps:
## 1) Within each plot × rain event (Collection), replicate throughfall samples
##    are averaged to obtain a single value per plot per event.
## 2) Plot × event means are then averaged across rain events, giving each
##    sampled event equal weight in the final plot-level means.
##
## This approach avoids over-weighting rain events with more samples and ensures
## that inter-event variability is represented consistently across plots.
##
## The resulting dataset is used for:
## - fertilization effect-size summaries
## - plot-level LMER analyses of throughfall nutrients

## Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

## Load functions
source(here("scripts/fns_data_prep.R"))

## Read in data
dat_tfall <- read_csv(here("data/throughfall/raw_throughfall.csv"))

## Names of nutrients
nuts <- c("NO3", "NH4", "N", "PO4", "K", "Ca", "Mg", "Na")
ppm_cols <- paste0(nuts, "_ppm")
dep_cols <- paste0(nuts, ".dep")

## Plot means of nutrient concentration & deposition
## First average by plot and event, then average across events
tfall_byPlot <- dat_tfall %>%
  # Stage 1: within each Plot × Collection, average replicates
  group_by(Plot, Collection) %>%
  summarize(across(c(Treatment, Replicate, Block, N, P, K), first),
            across(all_of(ppm_cols), ~ mean(.x, na.rm = TRUE)),
            across(all_of(dep_cols), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  # Stage 2: average across Collections (equal weight per rain event)
  group_by(Plot) %>%
  summarize(across(c(Treatment, Replicate, Block, N, P, K), first),
            across(all_of(ppm_cols), ~ mean(.x, na.rm = TRUE)),
            across(all_of(dep_cols), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  ## No Ca/Mg/Na data from plot 23; convert NaN to NA
  mutate(across(where(is.numeric), ~ replace(.x, is.nan(.x), NA_real_)))

## Create CSV of plot-level throughfall nutrient means
write.csv(tfall_byPlot, here("data/throughfall/processed_throughfall_plot_means.csv"), row.names = FALSE)

