
## Process litterfall data
##
## Reads raw litterfall mass (trap-level, monthly) and raw litter nutrient concentrations,
## then produces:
## - processed_litter_mass_by_year.csv: annual litter mass by plot × year (kg ha⁻¹ y⁻¹; Nov–Oct year)
## - processed_litter_plot_means.csv: plot-level mean litter mass and nutrient fluxes (kg ha⁻¹ y⁻¹)
##
## Annualization: monthly trap data are summed within trap, averaged across traps within plot,
## and summed across months to define a "litter year" (Nov–Oct)

## Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

## Read in litter nutrient concentrations
litter.nutrients <- read.table(header = T, here("data/litter/raw_litter_nutrient_conc.txt"))
litter.nutrients <- litter.nutrients %>% 
  mutate(Plot = as.factor(Plot)) %>%
  as_tibble()

## Read in total litter mass
litter.mass <- read.csv(here("data/litter/raw_litter_mass_2020-06-02.csv")) %>%
  as_tibble()

## Aggregate trap-level monthly litter mass into annual plot totals (Nov–Oct “litter year”)
## (Original aggregation logic from Joe)
litter.mass.annual <- litter.mass %>%
  mutate(Date = as_date(Date, format="%Y-%m-%d")) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = ifelse(Month >= 11, year(Date) + 1, year(Date))) %>%
  filter(Year > 1998 & Year < 2019) %>%
  mutate(Total = Total*10/0.57) %>%      # unit conversion to kg ha⁻¹ (based on trap area; 0.57 m² collector)
  select(Year,Month,Plot,Trap_Coords_x.y,Total) %>%
  group_by(Year, Month, Plot, Trap_Coords_x.y) %>%
  summarise(Total = sum(Total)) %>%       # sum within trap & month
  filter(!is.na(Total)) %>%
  group_by(Year, Month, Plot) %>%
  summarise(Total = mean(Total)) %>%      # average within plot & month
  group_by(Year, Plot) %>%
  summarise(Total = sum(Total)) %>%       # sum within plot & year
  ungroup() %>%
  mutate(Plot = as.factor(Plot))

## Add plot-level treatment metadata (factorial N/P/K fertilization indicators and Treatment label)
meta_dat <- read.csv(here("data/plot_metadata.csv"))
meta_dat <- meta_dat %>% as_tibble() %>% 
  mutate(across(everything(), as.factor)) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK")))

## Join litter mass with plot data (NPK fert)
litter.mass.annual <- litter.mass.annual %>% left_join(meta_dat, by = "Plot")

## Omit litterfall mass data from the first 5 years of the experiment (Years ≤ 2002)
## to align with the analysis window used for treatment effects
litter.mass.annual <- litter.mass.annual %>%
  filter(Year > 2002) %>%
  filter(!is.na(Treatment)) %>%  ## Omit micronutrient plots
  relocate(Total, .after = K)


## Compute mean litter nutrient concentrations by plot
## Converts concentrations to:
## - ppt (mg g⁻¹) for reporting
## - mass_fraction (g g⁻¹) for flux calculations
## Note: raw N is reported in %; convert to mg g⁻¹ by multiplying by 10
litter.nutrient.plot.averages <- litter.nutrients %>% 
  mutate(Nutrient = sub(".*_", "", Parameter)) %>% 
  mutate(Value_ppt = ifelse(Nutrient == "N", Value*10, Value),
         Value_mass_fraction = Value_ppt/1000) %>%
  select(-c(Species, Parameter, Value)) %>%
  filter(Nutrient %in% c("P", "N", "K", "Mg", "Ca", "Na")) %>%
  group_by(Plot, Nutrient) %>%
  summarize(ppt = mean(Value_ppt), mass_fraction = mean(Value_mass_fraction)) %>%
  pivot_wider(names_from = Nutrient, values_from = c(ppt, mass_fraction), names_glue = "{Nutrient}_{.value}")

## Compute plot-level mean annual nutrient fluxes:
## Flux (kg ha⁻¹ y⁻¹) = mean litter mass (kg ha⁻¹ y⁻¹) × concentration (g g⁻¹)
litter.byPlot <- litter.mass.annual %>% 
  group_by(Plot) %>%
  summarize(across(c(Replicate, Block, Treatment, N, P, K), first),
            Mass = mean(Total)) %>% 
  left_join(litter.nutrient.plot.averages, by = "Plot") %>%
  mutate(across(.cols = c("P_mass_fraction", "N_mass_fraction", "K_mass_fraction", "Mg_mass_fraction", "Ca_mass_fraction", "Na_mass_fraction"), 
                ~ .x * Mass,
                .names = "{sub('_mass_fraction$', '', .col)}_flux")) %>%
  select(-contains("mass_fraction")) %>%
  ungroup()
  

## Write processed outputs used by downstream LMER scripts and tables
## - mass_by_year: Litter mass for all years in kg ha⁻¹ y⁻¹
## - plot_means: Mean annual mass and nutrient fluxes in kg ha⁻¹ y⁻¹; concentrations in mg g⁻¹ (stored as *_ppt)
write_csv(litter.mass.annual, here("data/litter/processed_litter_mass_by_year.csv"))
write_csv(litter.byPlot, here("data/litter/processed_litter_plot_means.csv"))

