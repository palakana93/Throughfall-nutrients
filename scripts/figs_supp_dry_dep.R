
## Supplemental figures and calculations using throughfall data:
## - Fig S1: Total P vs phosphate (PO4) in throughfall
## - Fig S2: Leaf area index (LAI) by treatment
## - Dry deposition estimate using Na as a tracer (for supplemental text)

## Clean workspace to ensure figures are generated from a fresh session
## (safe because this script is intended to be run, not sourced)
if (sys.nframe() == 0) {
  rm(list = ls())
}

## Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

## Load shared helper functions
source(here("scripts/fns_data_prep.R")) # Factor recoding and data-prep helpers
source(here("scripts/fns_plotting.R")) # Shared plotting helpers (e.g., treatmentEffectPlot, add_theme)

## Read in data and standardize factor levels
## - raw_throughfall.csv: event-level rain and throughfall chemistry
dat_tfall <- read_csv(here("data/throughfall/raw_throughfall.csv")) %>% 
  factorize_tfall_columns()

## Subset for LAI figure: omit missing LAI values and exclude rain samples
dat_LAI <- dat_tfall %>% filter(!is.na(LAI), Treatment != "Rain")

## Set style parameters (P treatment only; used for Fig S2)
scm.P <- scale_color_manual(labels = c(" - P", "+ P"), values = c("#FF6E3A", "#9F0162"))

########################################################## 
## FIGURE S1. Throughfall concentrations of total P versus phosphate (PO4)
##########################################################  
dat_tfall %>% filter(!is.na(P_total_ppm)) %>%
  ggplot(aes(x = P_total_ppm, y = PO4_ppm)) + 
  geom_point(pch = 1) + 
  geom_abline(slope = 1, lty = "dashed") +
  labs(x = expression(Total~P~(mg~L^-1)), y = expression(PO[4]*phantom()^{'3-'}~(mg~L^-1))) +
  theme_bw()
## ggsave(here("figures/Figure S1.png"), width = 4.5, height = 3) # Uncomment to save figure


########################################################## 
## FIGURE S2. Effect of fertilization treatment on leaf area index
##########################################################  
dat_LAI %>%
  rename(P_LAI = LAI) %>%
  treatmentEffectPlot("P_LAI", point.size = 0.8) %>%
  add_theme(scm = scm.P, legend.pos = "inside", legend.coord = c(0.08, 0.88)) +
  labs(y = expression(Leaf~area~index~(m^2~m^-2)))
# ggsave(here("figures/Figure S2.png"), width = 6, height = 4) # Uncomment to save figure


########################################################## 
## Dry deposition calculations for supplemental text
########################################################## 
## Estimate dry deposition using Na as a tracer ion
## Assumptions:
## - All Na in throughfall originates from atmospheric deposition (no canopy leaching source)
## - Other elements share the same wet:dry deposition ratio as Na

## Summarize Na concentrations and deposition by water type (rain vs throughfall)
na_summary <- dat_tfall %>% group_by(Type) %>%
  filter(!is.na(Na_ppm)) %>%
  summarize(Na_conc = mean(Na_ppm), Na_dep = mean(Na.dep), n = n(), .groups = "drop")

## Estimate dry deposition (DD) as a fraction of wet deposition, using Na as a tracer
## DD is calculated as the proportional increase in throughfall relative to rain
##
## DD is reported in the supplemental text using the deposition-based estimate
## The concentration-based estimate is also calculated here for completeness and comparison
dd_by_dep <- na_summary %>%
  summarize(dd = (Na_dep[Type == "Tfall"] - Na_dep[Type == "Rain"]) / Na_dep[Type == "Rain"]) %>%
  pull(dd)

dd_by_conc <- na_summary %>%
  summarize(dd = (Na_conc[Type == "Tfall"] - Na_conc[Type == "Rain"]) / Na_conc[Type == "Rain"]) %>%
  pull(dd)

dd_by_dep   # ~13% of wet deposition
dd_by_conc  # ~21% of wet deposition

