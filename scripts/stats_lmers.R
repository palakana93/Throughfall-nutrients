
## LMER models for fertilization effects on:
##   (a) leaf area index (LAI)
##   (b) litter mass
##   (c) litter nutrient fluxes and concentrations
##   (d) throughfall nutrient fluxes and concentrations
## Includes summary calculations of fertilization effect sizes

## Clean workspace to ensure results are generated from a fresh session
## (safe because this script is intended to be run, not sourced)
if (sys.nframe() == 0) {
  rm(list = ls())
}

## Packages
## - lmerTest extends lme4 and provides p-values via Satterthwaite/Kenward–Roger methods
## - officer / flextable are used only for exporting ANOVA tables to Word
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lmerTest)
  library(officer) # export tables to Word
  library(flextable) # export tables to Word
})

## Load shared helper functions
source(here("scripts/fns_data_prep.R")) # Factor recoding and data-prep helpers
source(here("scripts/fns_tables.R"))    # Table construction and formatting helpers
source(here("scripts/fns_stats.R"))     # Statistical helpers (e.g., diagnostic_plot, make_lmer)

## Read in data and standardize factor levels
## - raw_throughfall.csv: event-level rain and throughfall chemistry
## - processed_throughfall_plot_means.csv: plot-level throughfall chemistry (averaged across events)
## - processed_litter_plot_means.csv: plot-level litter nutrient fluxes (annual means)
## - processed_litter_mass_by_year.csv: plot-level litter mass (data from all years; used for random Year effect)
dat_tfall <- read_csv(here("data/throughfall/raw_throughfall.csv")) %>% 
  factorize_tfall_columns()
tfall_byPlot <- read_csv(here("data/throughfall/processed_throughfall_plot_means.csv")) %>%
  factorize_tfall_columns() %>%
  filter(Treatment != "Rain") %>%
  arrange(as.numeric(as.character(Plot)))
dat_litter <- read_csv(here("data/litter/processed_litter_plot_means.csv")) %>%
  factorize_litter_columns()
dat_litter_mass_annual <- read_csv(here("data/litter/processed_litter_mass_by_year.csv")) %>%
  factorize_litter_columns() %>% mutate(Year = factor(Year))

## Subset for fertilization effects on LAI: omit missing LAI values and exclude rain samples
dat_LAI <- dat_tfall %>% filter(!is.na(LAI), Treatment != "Rain")


## Fertilization Effects: LAI, litterfall, throughfall -------------------
## LAI
## Compute mean LAI per plot for treatment-level comparisons
LAI_byPlot <- dat_LAI %>%
  group_by(Plot, Replicate, Block, Treatment, N, P, K) %>%
  summarize(mean.LAI = mean(LAI), .groups = "drop")
## Quantify percent difference in LAI between -P and +P plots
LAI_byPlot %>% group_by(P) %>% summarize(LAI = mean(mean.LAI))
(9.05/8.28-1)*100 ## LAI is 9.3% greater in +P vs -P plots


## Litterfall: percent changes with nutrient addition
## (reported in Results text; values shown here for transparency)

## P fertilization effects on litter P
## 18% increase in litter mass; 77% increase in P concentration
dat_litter %>% group_by(P) %>% summarize(mass = mean(Mass)) %>% mutate(mass = 100*mass/first(mass))
dat_litter %>% group_by(P) %>% summarize(P.conc = mean(P_ppt)) %>% mutate(P.conc = 100*P.conc/first(P.conc))
dat_litter %>% group_by(P) %>% summarize(P = mean(P_flux)) ## Litter P (kg/ha/yr)

## K fertilization effects on litter K
## 9% decrease in litter mass; 23% increase in K concentration
dat_litter %>% group_by(K) %>% summarize(mass = mean(Mass)) %>% mutate(mass = 100*mass/first(mass))
dat_litter %>% group_by(K) %>% summarize(K.conc = mean(K_ppt)) %>% mutate(K.conc = 100*K.conc/first(K.conc))

## Other fertilization effects
## +P: 14% reduction in K concentration
dat_litter %>% group_by(P) %>% summarize(K.conc = mean(K_ppt)) %>% mutate(K.conc = 100*K.conc/first(K.conc))
## +N: 17% reduction in P concentration
dat_litter %>% group_by(N) %>% summarize(P.conc = mean(P_ppt)) %>% mutate(P.conc = 100*P.conc/first(P.conc))
## +NP Interaction: P concentration increases under +P and +N alone, but is reduced under combined +NP
dat_litter %>% group_by(N, P) %>% summarize(P.conc = mean(P_ppt)) %>%
  ungroup() %>% mutate(P.conc = 100*P.conc/first(P.conc))


## Throughfall: percent changes with nutrient addition
## (reported in Results text; values shown here for transparency)

## Percent changes with fertilization
## +P: 310% increase in P concentration
tfall_byPlot %>% filter(P != "Rain") %>% group_by(P) %>% summarize(P.conc = mean(PO4_ppm)) %>% mutate(P.conc = 100*P.conc/first(P.conc))
## +P: 360% increase in P fluxes
tfall_byPlot %>% filter(P != "Rain") %>% group_by(P) %>% summarize(P.dep = mean(PO4.dep)) %>% mutate(P.dep = 100*P.dep/first(P.dep))

## +N: 16% reduction in NH4 fluxes
tfall_byPlot %>% filter(N != "Rain") %>% group_by(N) %>% summarize(NH4.dep = mean(NH4.dep)) %>% mutate(NH4.dep = 100*NH4.dep/first(NH4.dep))

## +N: 34% reduction in P fluxes
tfall_byPlot %>% filter(N != "Rain") %>% group_by(N) %>% summarize(P.dep = mean(PO4.dep)) %>% mutate(P.dep = 100*P.dep/first(P.dep))

## +N: 27% reduction in K concentrations
tfall_byPlot %>% filter(N != "Rain") %>% group_by(N) %>% summarize(K.conc = mean(K_ppm)) %>% mutate(K.conc = 100*K.conc/first(K.conc))
## +N: 25% reduction in K fluxes
tfall_byPlot %>% filter(N != "Rain") %>% group_by(N) %>% summarize(K.dep = mean(K.dep)) %>% mutate(K.dep = 100*K.dep/first(K.dep))

## +N: 29% reduction in Ca concentration
tfall_byPlot %>% filter(N != "Rain") %>% group_by(N) %>% summarize(Ca.conc = mean(Ca_ppm, na.rm = TRUE)) %>% mutate(Ca.conc = 100*Ca.conc/first(Ca.conc))
## +N: 28% reduction in Ca fluxes
tfall_byPlot %>% filter(N != "Rain") %>% group_by(N) %>% summarize(Ca.dep = mean(Ca.dep, na.rm = TRUE)) %>% mutate(Ca.dep = 100*Ca.dep/first(Ca.dep))
## --------------------------------------


## Tables of Type-II ANOVA output (Kenward–Roger df) ---------------

## Convert RHS templates to strings for make_lmer()
lmer_mod <- ~ N*P + P*K + N*K + (1|Replicate/Block)
rhs_txt  <- as.character(lmer_mod)[2]

## For Table S6
## Fertilization effects on throughfall nutrients (plot-level means)
## Note: responses are log-transformed to improve residual behavior and comparability across nutrients
specs_tfall <- tibble::tribble(
  ~section,                               ~resp_var,
  "Throughfall NO3 Flux",                 "NO3.dep",  
  "Throughfall NH4 Flux",                 "NH4.dep",
  "Throughfall NO3 + NH4 Flux",           "N.dep",
  "Throughfall PO4 Flux",                 "PO4.dep",
  "Throughfall K Flux",                   "K.dep",
  "Throughfall Ca Flux",                  "Ca.dep",
  "Throughfall Mg Flux",                  "Mg.dep",
  "Throughfall Na Flux",                  "Na.dep",
  "Throughfall NO3 Concentration",        "NO3_ppm",
  "Throughfall NH4 Concentration",        "NH4_ppm",
  "Throughfall NO3 + NH4 Concentration",  "N_ppm",
  "Throughfall PO4 Concentration",        "PO4_ppm",
  "Throughfall K Concentration",          "K_ppm",
  "Throughfall Ca Concentration",         "Ca_ppm",
  "Throughfall Mg Concentration",         "Mg_ppm",
  "Throughfall Na Concentration",         "Na_ppm")

anova_tables_tfall <- specs_tfall %>%
  mutate(model = map(resp_var, make_lmer, dat = tfall_byPlot, rhs = rhs_txt, log_TF = TRUE),
         anova_tbl = map(model, ~ anova(.x, type = 2, ddf = "Kenward-Roger") %>% make_ANOVA_table())) %>%
  transmute(section, resp_var, model,
            table = map2(anova_tbl, section, ~ bind_rows(section_row(names(.x), .y), .x)))

tableS6 <- bind_rows(anova_tables_tfall$table)

## Column names
tableS6_header <- setNames(c("Effect", "Numerator DF", "Denominator DF", "F-value", "P-value"),
                           names(tableS6))
## Column widths
tableS6_widths <- c(0.8, 1, 1.2, 0.8, 0.8)

## Save table to document (landscape orientation)
tableS6_doc <- read_docx() %>% 
  add_table(tableS6, tableS6_header, tableS6_widths, 
                         title = "Table S6. Linear mixed model output for models of throughfall nutrients")
# print(tableS6_doc, target = here("tables/Table S6_tfall LMERs.docx"))


## For Table S7
## Fertilization effects on LAI, litter mass, and litter nutrients

## Prep model specifications
lmer_mod_mass <- ~ N*P + P*K + N*K + (1|Replicate/Block/Plot) + (1|Year)
rhs_txt_mass <- as.character(lmer_mod_mass)[2]

specs_litter <- tibble::tribble(
  ~section,                 ~dat,                    ~resp_var,    ~log_TF,  ~rhs,
  "Leaf Area Index",        LAI_byPlot,              "mean.LAI",   FALSE,    rhs_txt,
  "Annual Litter Mass",     dat_litter_mass_annual,  "Total",      TRUE,     rhs_txt_mass,
  "Litter N Flux",          dat_litter,              "N_flux",     FALSE,    rhs_txt,
  "Litter P Flux",          dat_litter,              "P_flux",     FALSE,    rhs_txt,
  "Litter K Flux",          dat_litter,              "K_flux",     FALSE,    rhs_txt,
  "Litter N Concentration", dat_litter,              "N_ppt",      FALSE,    rhs_txt,
  "Litter P Concentration", dat_litter,              "P_ppt",      FALSE,    rhs_txt,
  "Litter K Concentration", dat_litter,              "K_ppt",      FALSE,    rhs_txt)

anova_tables_litter <- specs_litter %>%
  mutate(model = pmap(list(dat, resp_var, rhs, log_TF), make_lmer),
         anova_tbl = map(model, ~ anova(.x, type = 2, ddf = "Kenward-Roger") %>% make_ANOVA_table())) %>%
  transmute(section, model, resp_var,
            table = map2(anova_tbl, section, ~ bind_rows(section_row(names(.x), .y), .x)))

tableS7 <- bind_rows(anova_tables_litter$table)

## Column names
tableS7_header <- setNames(c("Effect", "Numerator DF", "Denominator DF", "F-value", "P-value"),
                           names(tableS7))
## Column widths
tableS7_widths <- c(0.8, 1, 1.2, 0.8, 0.8)

## Save table to document (landscape orientation)
tableS7_doc <- read_docx() %>%
  add_table(tableS7, tableS7_header, tableS7_widths, 
                         title = "Table S7. Linear mixed model output for models of LAI, litter mass, and litter nutrients")
# print(tableS7_doc, target = here("tables/Table S7_litter LMERs.docx"))
## --------------------------------------


## Model diagnostics -------------------
## For each fitted model: residual diagnostics and normality checks

#### Examine diagnostic plots
## LAI: Looks good
get_model(anova_tables_litter, "mean.LAI") %>% diagnostic_plot(mean.LAI, P, LAI_byPlot)

## Litter mass: Violates Shapiro-Wilk normality test (large data set), but Q-Q plot looks good
get_model(anova_tables_litter, "Total") %>% diagnostic_plot(Total, P, dat_litter_mass_annual)

## Litter nutrients: These look good
get_model(anova_tables_litter, "P_flux") %>% diagnostic_plot(P_flux, P)
get_model(anova_tables_litter, "N_flux") %>% diagnostic_plot(N_flux, N)
get_model(anova_tables_litter, "K_flux") %>% diagnostic_plot(K_flux, K)

get_model(anova_tables_litter, "P_ppt") %>% diagnostic_plot(P_ppt, P)
get_model(anova_tables_litter, "N_ppt") %>% diagnostic_plot(N_ppt, N)
get_model(anova_tables_litter, "K_ppt") %>% diagnostic_plot(K_ppt, K)

## Throughfall nutrients: Generally acceptable; K.dep is the weakest case on log scale
get_model(anova_tables_tfall, "PO4.dep") %>% diagnostic_plot(log(PO4.dep), P, filter(tfall_byPlot, P != "Rain"))
get_model(anova_tables_tfall, "N.dep") %>% diagnostic_plot(log(N.dep), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "K.dep") %>% diagnostic_plot(log(K.dep), K, filter(tfall_byPlot, K != "Rain"))
get_model(anova_tables_tfall, "NH4.dep") %>% diagnostic_plot(log(NH4.dep), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "NO3.dep") %>% diagnostic_plot(log(NO3.dep), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "Ca.dep") %>% diagnostic_plot(log(Ca.dep), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "Mg.dep") %>% diagnostic_plot(log(Mg.dep), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "Na.dep") %>% diagnostic_plot(log(Na.dep), N, filter(tfall_byPlot, N != "Rain"))

get_model(anova_tables_tfall, "PO4_ppm") %>% diagnostic_plot(log(PO4_ppm), P, filter(tfall_byPlot, P != "Rain"))
get_model(anova_tables_tfall, "N_ppm") %>% diagnostic_plot(log(N_ppm), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "K_ppm") %>% diagnostic_plot(log(K_ppm), K, filter(tfall_byPlot, K != "Rain"))
get_model(anova_tables_tfall, "NH4_ppm") %>% diagnostic_plot(log(NH4_ppm), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "NO3_ppm") %>% diagnostic_plot(log(NO3_ppm), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "Ca_ppm") %>% diagnostic_plot(log(Ca_ppm), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "Mg_ppm") %>% diagnostic_plot(log(Mg_ppm), N, filter(tfall_byPlot, N != "Rain"))
get_model(anova_tables_tfall, "Na_ppm") %>% diagnostic_plot(log(Na_ppm), N, filter(tfall_byPlot, N != "Rain"))

## Throughfall K deposition shows slightly improved diagnostics without log-transformation
## However, most nutrients perform better on the log scale, so logged values are used consistently
get_model(anova_tables_tfall, "K.dep") %>% diagnostic_plot(log(K.dep), K, filter(tfall_byPlot, K != "Rain"))
tfall.K.stat.2 <- lmer(data = tfall_byPlot, K.dep ~ N*P + P*K + N*K + (1|Replicate/Block))
diagnostic_plot(tfall.K.stat.2, K.dep, K, filter(tfall_byPlot, K != "Rain"))
## --------------------------------------

