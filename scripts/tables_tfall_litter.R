
## Annual budgets of throughfall and litterfall nutrient deposition
##
## Builds summary tables of rain, throughfall, and litter nutrient budgets, including:
## - Nutrient concentrations (mg L⁻¹ for throughfall and rainfall; mg g⁻¹ for litterfall)
## - Throughfall and rainfall nutrient deposition per rain event (mg m⁻² event⁻¹)
## - Annual nutrient deposition (kg ha⁻¹ y⁻¹)
## - Throughfall canopy-to-soil nutrient transfers (throughfall deposition − rain deposition),
##   reported per rain event and on an annual basis
##
## Tables are produced in two grouping schemes:
## 1) By focal nutrient addition (± nutrient; e.g., ±P for PO4, ±N for NO3/NH4/N)
## 2) By factorial treatment (Control, +N, +P, +K, +NP, +NK, +PK, +NPK)
##
## Outputs:
## - Table 2: Throughfall (focal nutrient addition; N/P/K subset)
## - Table 3: Litterfall (focal nutrient addition)
## - Table S1: Throughfall (by plot vs by rain event; includes sample sizes)
## - Table S4: Throughfall (by treatment; all nutrients)
## - Table S5: Litterfall (by treatment; all nutrients)


## Clean workspace to ensure results are generated from a fresh session
## (safe because this script is intended to be run, not sourced)
if (sys.nframe() == 0) {
  rm(list = ls())
}

## Packages
## - officer / flextable are used only for exporting ANOVA tables to Word
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(officer) # export tables to Word
  library(flextable) # export tables to Word
})

## Load shared helper functions
source(here("scripts/fns_data_prep.R")) # Factor recoding and data-prep helpers
source(here("scripts/fns_tables.R"))    # Table construction and formatting helpers

## Read in data and standardize factor levels
## - raw_throughfall.csv: event-level rain and throughfall chemistry
## - processed_throughfall_plot_means.csv: plot-level throughfall chemistry (averaged across events)
## - processed_litter_plot_means.csv: plot-level litter nutrient fluxes (annual means)
dat_tfall <- read_csv(here("data/throughfall/raw_throughfall.csv")) %>% 
  factorize_tfall_columns()
tfall_byPlot <- read_csv(here("data/throughfall/processed_throughfall_plot_means.csv")) %>%
  factorize_tfall_columns() %>%
  # Rename N/P/K columns in plot-level throughfall data to avoid collisions with nutrient variables later in the script (e.g., N as a nutrient vs N as a treatment indicator).
  rename(N.fert = N, P.fert = P, K.fert = K)
dat_litter <- read_csv(here("data/litter/processed_litter_plot_means.csv")) %>%
  factorize_litter_columns()

## Names of nutrients
nuts <- c("NO3", "NH4", "N", "PO4", "K", "Ca", "Mg", "Na")

##########################################################
## Means ± SE of fertilization fx on rain and throughfall nutrients
##########################################################
#### 5 metrics: nutrient concentration, deposition (per rain event and annual), and transfers (per rain event and annual)
#### Each metric has one table grouped by treatment ("treat"; n = 8) 
####   and one table grouped by fertilizer addition of focal nutrient ("fertAdd"; n = 2)
####   For instance; "fertAdd" groups by +P/-P for PO4 and +N/-N for NH4

## Multiplication factors to scale from measured rainfall to annual rainfall 
##
## For each nutrient, compute mean rainfall per sampled event (mm/event)
##   (N/P data are from 4 rain events, K from 3 events, Ca/Mg/Na from 1 event)
## Later will scale event-level deposition to annual deposition by:
##   annual_dep = event_dep * (mult_factor = annual_rainfall / mean_event_rainfall)
annual_rainfall <- 2623  # in mm/year
ppm_cols <- paste0(nuts, "_ppm")
rainfall_by_nutrient <- dat_tfall %>% select(Collection, ppm_cols, Rain_cm) %>%
  pivot_longer(all_of(ppm_cols), names_to = "Nutrient", values_to = "Conc") %>%
  filter(!is.na(Conc)) %>%
  select(-Conc) %>%
  distinct() %>%
  group_by(Nutrient) %>%
  summarize(mean_rainfall_per_event = mean(Rain_cm)*10, num_events = n()) %>%
  mutate(Nutrient = gsub("_ppm", "", Nutrient),
         mult_factor = annual_rainfall/mean_rainfall_per_event)

mult_factor_NP <- rainfall_by_nutrient %>% filter(Nutrient == "N") %>% select(mult_factor) %>% pull()
mult_factor_K <- rainfall_by_nutrient %>% filter(Nutrient == "K") %>% select(mult_factor) %>% pull()
mult_factor_CaMgNa <- rainfall_by_nutrient %>% filter(Nutrient == "Ca") %>% select(mult_factor) %>% pull()


## BY FOCAL NUTRIENT ADDITION (Table 2) ----------------
## Summarize concentration and deposition data
## Reshape to long format and classify each observation by whether the plot
##   received the focal nutrient for the given response (e.g., PO4 vs ±P plots).
fertAdd.event <- tfall_byPlot %>%
  pivot_longer(c(NO3_ppm:K_ppm, NO3.dep:K.dep), names_to = c("Nutrient", ".value"), names_pattern = "(.*)[._](ppm|dep)") %>%
  mutate(fert = case_when(Treatment == "Rain" ~ "Rain",
                          str_detect(as.character(Treatment), str_sub(Nutrient, 1, 1)) ~ "+",
                          TRUE ~ "—"),
         fert = factor(fert, levels = c("Rain", "—", "+"),
                       labels = c("Rain", "Tfall (– focal nutrient)", "Tfall (+ focal nutrient)"))) %>%
  select(Treatment, Nutrient, fert, ppm, dep) %>%
  group_by(Nutrient, fert)

## Nutrient concentration, formatted for table
conc.fertAdd.tbl <- fertAdd.event %>%
  summarize(stat = stat(ppm)) %>%
  arrange(desc(Nutrient)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  relocate(PO4, .after = N) %>%
  mutate(across(-fert, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))

## Nutrient deposition and transfer per rain event
## Transfer SE computed as sqrt(SE_tfall^2 + SE_rain^2)
dep.fertAdd.event.inProg <- fertAdd.event %>%
  summarize(stat = stat(dep)) %>%
  mutate(stat_tbl = map_chr(stat, \(ms) format_mean_se(ms["mean"], ms["se"])),
         stat_trans = stat_transfer(stat),
         stat_tbl_ss = paste0(stat_tbl, " (n = ", map_int(stat, ~ .x[["n"]]), ")")) %>%
  arrange(desc(Nutrient)) 

## Nutrient deposition formatted for table
dep.fertAdd.event <- dep.fertAdd.event.inProg %>%
  select(fert, stat_tbl) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_tbl)) %>%
  relocate(PO4, .after = N)

## Nutrient deposition: WITH SAMPLE SIZES (for Table S1)
dep.fertAdd.event.with.ss <- dep.fertAdd.event.inProg %>%
  select(fert, stat_tbl_ss) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_tbl_ss)) %>%
  relocate(PO4, .after = N)

## Nutrient transfers per rain event, formatted for table
transfer.fertAdd.event <- dep.fertAdd.event.inProg %>%
  select(fert, stat_trans) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_trans)) %>%
  relocate(PO4, .after = N) %>%
  filter(fert!="Rain")



## Annual nutrient deposition
##
## Convert units and scale to annual deposition:
## - dep is in mg m⁻² event⁻¹ in the plot-level dataset
## - divide by 100 to convert mg m⁻² → kg ha⁻¹ (because 1 mg m⁻² = 0.01 kg ha⁻¹)
## - multiply by mult_factor to scale from an average sampled event to annual totals
dep.fertAdd.inProg <- fertAdd.event %>%
  left_join(select(rainfall_by_nutrient, Nutrient, mult_factor), by = "Nutrient") %>%
  mutate(dep = dep/100*mult_factor) %>%
  summarize(stat = stat(dep)) %>%
  mutate(stat_tbl = map_chr(stat, \(ms) format_mean_se(ms["mean"], ms["se"])),
         stat_trans = stat_transfer(stat),
         stat_trans_ss = paste0(stat_trans, " (n = ", map_int(stat, ~ .x[["n"]]), ")")) %>%
  arrange(desc(Nutrient)) 

## Annual nutrient deposition formatted for table
dep.fertAdd <- dep.fertAdd.inProg %>%
  select(Nutrient, fert, stat_tbl) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_tbl)) %>%
  relocate(PO4, .after = N)

## Annual nutrient transfer, formatted for table
transfer.fertAdd <- dep.fertAdd.inProg %>%
  select(Nutrient, fert, stat_trans) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_trans)) %>%
  relocate(PO4, .after = N) %>%
  filter(fert != "Rain")

## Annual nutrient transfer: WITH SAMPLE SIZES (for Table S1)
transfer.fertAdd.with.ss <- dep.fertAdd.inProg %>%
  select(Nutrient, fert, stat_trans_ss) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_trans_ss)) %>%
  relocate(PO4, .after = N) %>%
  filter(fert != "Rain")
## -------------------------------------


## BY TREATMENT (factorial nutrient addition; Table S4) ----------------------
## Nutrient concentration, formatted for table
conc.treat <- tfall_byPlot %>% 
  group_by(Treatment) %>%
  summarize(across(contains("_ppm"), list(stat = stat), .names = "{sub('_.*$', '', .col)}")) %>%
  mutate(across(-Treatment, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))

## Annual nutrient deposition
dep.treat <- tfall_byPlot %>% 
  group_by(Treatment) %>%
  ## Adjust to annual flux based on sampled rainfall (differs by nutrient)
  ## Divide by 100 to go from mg/m2 to kg/ha
  mutate(across(all_of(paste0(nuts[1:4], ".dep")), ~ .x/100 * mult_factor_NP),
         across(all_of(paste0(nuts[5], ".dep")), ~ .x/100 * mult_factor_K),
         across(all_of(paste0(nuts[6:8], ".dep")), ~ .x/100 * mult_factor_CaMgNa)) %>%
  summarize(across(contains(".dep"), list(stat = stat), .names = "{sub('\\\\..*$', '', .col)}")) 
## Formatted for table
dep.treat.tbl <- dep.treat %>%
  mutate(across(-Treatment, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))

## Annual nutrient transfers, formatted for table (throughfall − rain)
## (Rain row removed because transfer is defined relative to rain)
transfer.treat <- dep.treat %>%
  mutate(across(-Treatment, stat_transfer)) %>% 
  filter(Treatment !="Rain")
## -------------------------------------


## VARIATION IN NUTRIENT FLUXES/TRANSFERS ACROSS RAIN EVENTS (Table S1) ----------------
## Summarize nutrient data by event
##   First summarize deposition within each Collection, then summarizing across Collections within each fert class
dep.fertAdd.byEvent <- dat_tfall %>%
  rename(N.fert = N, P.fert = P, K.fert = K,
         PO4 = PO4.dep, NO3 = NO3.dep, NH4 = NH4.dep, N = N.dep, K = K.dep) %>%
  pivot_longer(c(NO3, NH4, N, PO4, K), names_to = "Nutrient", values_to = "dep") %>%
  mutate(fert = case_when(Treatment == "Rain" ~ "Rain",
                          str_detect(as.character(Treatment), str_sub(Nutrient, 1, 1)) ~ "+",
                          TRUE ~ "—")) %>%
  group_by(Collection, Nutrient, fert) %>%
  summarize(across(dep, ~mean(., na.rm = T)), .groups = "drop")  %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.nan(.x), NA_real_))) %>% ## Replace NaNs with NAs
  mutate(fert = factor(fert, levels = c("Rain", "—", "+"),
                       labels = c("Rain", "Tfall (– focal nutrient)", "Tfall (+ focal nutrient)")))
  
## Start formatting deposition data per event for table
dep.fertAdd.byEvent.perEvent.inProg <- dep.fertAdd.byEvent %>%
  group_by(Nutrient, fert) %>%
  summarize(stat = stat(dep)) %>%
  mutate(stat2 = map_chr(stat, \(ms) format_mean_se(ms["mean"], ms["se"]))) %>%
  arrange(desc(Nutrient))
## Format for table, with sample sizes (n = ...)
dep.fertAdd.byEvent.perEvent <- dep.fertAdd.byEvent.perEvent.inProg %>%
  mutate(stat3 = paste0(stat2, " (n = ", map_int(stat, ~ .x[["n"]]), ")")) %>%
  select(-c(stat, stat2)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat3)) %>%
  relocate(PO4, .after = N)

## Start formatting annual transfer data for table
dep.fertAdd.byEvent.annual <- dep.fertAdd.byEvent %>%
  mutate(dep = ifelse(Nutrient %in% nuts[1:4], dep/100*mult_factor_NP, dep/100*mult_factor_K)) %>%
  group_by(Nutrient, fert) %>%
  summarize(stat = stat(dep)) %>%
  mutate(stat_trans = stat_transfer(stat),
         stat_trans_ss = paste0(stat_trans, " (n = ", map_int(stat, ~ .x[["n"]]), ")")) %>%
  arrange(desc(Nutrient)) 
## Format annual transfer data for table, with sample sizes (n = ...)
transfers.byEvent.annual <- dep.fertAdd.byEvent.annual %>%
  select(Nutrient, fert, stat_trans_ss) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat_trans_ss)) %>%
  relocate(PO4, .after = N) %>%
  filter(fert !="Rain")
## -------------------------------------


##########################################################
## Means ± SE of fertilization fx on litterfall nutrients
##########################################################
## BY FOCAL NUTRIENT ADDITION (Table 3) ----------------
## Summarize litter data
litter.fertAdd <- dat_litter %>%
  # Convert litter mass from kg ha⁻¹ y⁻¹ to Mg ha⁻¹ y⁻¹ for reporting (divide by 1000)
  mutate(N.mass = Mass/1000, P.mass = Mass/1000, K.mass = Mass/1000) %>%
  select(-Mass) %>%
  pivot_longer(c(N.mass, P.mass, K.mass, N_flux, P_flux, K_flux, N_ppt, P_ppt, K_ppt), 
               names_to = c("Nutrient", ".value"), 
               names_pattern = "(.*)[._](mass|flux|ppt)") %>%
  mutate(fert = case_when(str_detect(as.character(Treatment), str_sub(Nutrient, 1, 1)) ~ "+", TRUE ~ "—")) %>%
  mutate(fert = factor(fert, levels = c("—", "+"),
                       labels = c("– focal nutrient", "+ focal nutrient"))) %>%
  group_by(Nutrient, fert)

## Litter mass
mass.litter.fertAdd <- litter.fertAdd %>%
  summarize(stat = stat(mass)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  relocate(K, .after = P) %>%
  mutate(across(-fert, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))
  
## Litterfall nutrient transfers
transfer.litter.fertAdd <- litter.fertAdd %>%
  summarize(stat = stat(flux)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  relocate(K, .after = P) %>%
  mutate(across(-fert, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))

## Litter nutrient concentrations by fert addition of focal nutrient
conc.litter.fertAdd <- litter.fertAdd %>%
  summarize(stat = stat(ppt)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  relocate(K, .after = P) %>%
  mutate(across(-fert, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))
## --------------------------------


## BY TREATMENT (Table S5) ----------------------
## Litter mass
mass.litter.treat <- dat_litter %>% 
  group_by(Treatment) %>%
  mutate(Mass = Mass/1000) %>%
  summarize(across(c("Mass"), list(stat = stat), .names = "{.col}")) %>%
  mutate(across(-Treatment, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))

## Litterfall transfers
transfer.litter.treat <- dat_litter %>% 
  group_by(Treatment) %>%
  summarize(across(c("N_flux", "P_flux", "K_flux", "Ca_flux", "Mg_flux", "Na_flux"),
                   list(stat = stat), .names = "{sub('_.*$', '', .col)}")) %>%
  mutate(across(-Treatment, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))

## Litter nutrient concentrations
conc.litter.treat <- dat_litter %>% group_by(Treatment) %>%
  summarize(across(c("N_ppt", "P_ppt", "K_ppt", "Ca_ppt", "Mg_ppt", "Na_ppt"),
                   list(stat = stat), .names = "{sub('_.*$', '', .col)}")) %>%
  mutate(across(-Treatment, ~ map_chr(.x, \(ms) format_mean_se(ms["mean"], ms["se"]))))
## --------------------------------


## Format and output tables as Word docs ----------------------
##########################################################
## Column display labels
tfall_display_labels_npk <- c("Treatment", "NO₃⁻–N", "NH₄⁺–N", "(NO₃⁻+NH₄⁺)–N", "PO₄³⁻–P", "K")
tfall_display_labels_all <- c(tfall_display_labels_npk, "Ca", "Mg", "Na")
litter_display_labels_npk <- c("Treatment", "N", "P", "K")
litter_display_labels_all <- c("Treatment", "N", "P", "K", "Ca", "Mg", "Na")


## Set everything up
## Assemble all table sections into a single "specs" object, then write out each table
## by filtering on tbl_num and formatting via add_table()
specs <- tibble::tribble(
  ~tbl_num,    ~stat,                         ~section,
  "2",         conc.fertAdd.tbl,              "Rain and Throughfall Nutrient Concentration (mg L\u207b\u00b9)",                                             
  "2",         dep.fertAdd.event,             "Rain and Throughfall Nutrient Flux (mg N or P or K m\u207b\u00b2 rain event\u207b\u00b9)",                   
  "2",         dep.fertAdd,                   "Annual Rain and Throughfall Nutrient Flux (kg N or P or K ha\u207b\u00b9 y\u207b\u00b9)",                    
  "2",         transfer.fertAdd.event,        "Throughfall Nutrient Transfers from Canopy to Soil (mg N or P or K m\u207b\u00b2 rain event\u207b\u00b9)",
  "2",         transfer.fertAdd,              "Annual Throughfall Nutrient Transfers from Canopy to Soil (kg N or P or K ha\u207b\u00b9 y\u207b\u00b9)",
  "3",         mass.litter.fertAdd,           "Litterfall Mass (Mg ha\u207b\u00b9 y\u207b\u00b9)", 
  "3",         conc.litter.fertAdd,           "Litter Nutrient Concentrations (mg N or P or K g\u207b\u00b9)", 
  "3",         transfer.litter.fertAdd,       "Annual Litterfall Nutrient Flux (kg N or P or K ha\u207b\u00b9 y\u207b\u00b9)",
  "S1",        NULL,                          "Rain and Throughfall Nutrient Flux (mg N or P or K m\u207b\u00b2 rain event\u207b\u00b9)",
  "S1",        dep.fertAdd.event.with.ss,     "By Plot",
  "S1",        dep.fertAdd.byEvent.perEvent,  "By Rain Event",
  "S1",        NULL,                          "Annual Throughfall Nutrient Transfers from Canopy to Soil (kg N or P or K ha\u207b\u00b9 y\u207b\u00b9)",
  "S1",        transfer.fertAdd.with.ss,      "By Plot",
  "S1",        transfers.byEvent.annual,      "By Rain Event",
  "S4",        conc.treat,                    "Rain and Throughfall Nutrient Concentration (mg L\u207b\u00b9)", 
  "S4",        dep.treat.tbl,                 "Annual Rain and Throughfall Nutrient Flux (kg ha\u207b\u00b9 y\u207b\u00b9)", 
  "S4",        transfer.treat,                "Annual Throughfall Nutrient Transfer from Canopy to Soil (kg ha\u207b\u00b9 y\u207b\u00b9)",
  "S5",        transfer.litter.treat,         "Annual Litterfall Nutrient Flux  (kg ha\u207b\u00b9 y\u207b\u00b9)", 
  "S5",        conc.litter.treat,             "Litter Nutrient Concentrations (mg g\u207b\u00b9)")

## Format tables for later extraction
col_names <- specs %>% pull(stat) %>% keep(~ !is.null(.x)) %>% pluck(1) %>% names() ## For NULL columns in Table S1
specs <- specs %>%
  mutate(table = map2(stat, section, \(x, s) {
    if (is.null(x)) section_row(col_names, s)  ## For NULL columns in Table S1
    else dplyr::bind_rows(section_row(names(x), s), x)
    }))

## Create Table 2 (throughfall by focal nutrient addition)
table2 <- specs %>% filter(tbl_num == "2") %>% pull(table) %>% bind_rows()

table2_header <- setNames(tfall_display_labels_npk, names(table2)) ## Column names
table2_widths <- c(1.5, 0.9, 1, 1.1, 1, 0.8) ## Column widths

## Save table to document
table2_doc <- read_docx()
table2_doc <- add_table(table2_doc, table2, table2_header, table2_widths, 
                 title = "Table 2. Effects of fertilization on rain and throughfall nutrients")
#print(table2_doc, target = here("tables/Table 2_tfall.docx"))


## Create Table 3 (litterfall by focal nutrient addition)
table3 <- specs %>% filter(tbl_num == "3") %>% pull(table) %>% bind_rows()

table3_header <- setNames(litter_display_labels_npk, names(table3)) ## Column names
table3_widths <- c(1.2, rep(0.9, 3)) ## Column widths

## Save table to document
table3_doc <- read_docx()
table3_doc <- add_table(table3_doc, table3, table3_header, table3_widths, 
                        title = "Table 3. Effects of fertilization on litter nutrients and litter mass")
#print(table3_doc, target = here("tables/Table 3_litter.docx"))



## Create Table S1 (throughfall by plot vs. by event)
tableS1 <- specs %>% filter(tbl_num == "S1") %>% pull(table) %>% bind_rows()

tableS1_header <- setNames(tfall_display_labels_npk, names(tableS1)) ## Column names
tableS1_widths <- c(1.5, rep(1.4, 5)) ## Column widths

## Save table to document (landscape orientation)
tableS1_doc <- read_docx()
sec_landscape <- prop_section(page_size = page_size(orient = "landscape"))
tableS1_doc <- body_set_default_section(tableS1_doc, sec_landscape)
tableS1_doc <- add_table(tableS1_doc, tableS1, tableS1_header, tableS1_widths, title = "Table S1")
# print(tableS1_doc, target = here("tables/Table S1_tfall.docx"))



## Create Table S4 (throughfall by treatment)
tableS4 <- specs %>% filter(tbl_num == "S4") %>% pull(table) %>% bind_rows()

tableS4_header <- setNames(tfall_display_labels_all, names(tableS4)) ## Column names
tableS4_widths <- c(0.8, rep(1.1, 8)) ## Column widths

## Save table to document (landscape orientation)
tableS4_doc <- read_docx()
sec_landscape <- prop_section(page_size = page_size(orient = "landscape"))
tableS4_doc <- body_set_default_section(tableS4_doc, sec_landscape)
tableS4_doc <- add_table(tableS4_doc, tableS4, tableS4_header, tableS4_widths, title = "Table S4")
# print(tableS4_doc, target = here("tables/Table S4_tfall.docx"))


## Create Table S5 (litterfall by treatment)
tableS5 <- specs %>% filter(tbl_num == "S5") %>% pull(table) %>% bind_rows()

tableS5_header <- setNames(litter_display_labels_all, names(tableS5)) ## Column names
tableS5_widths <- c(0.8, rep(0.9, 6)) ## Column widths

## Save table to document (landscape orientation)
tableS5_doc <- read_docx() %>%
  add_table(tableS5, tableS5_header, tableS5_widths, title = "Table S5")
# print(tableS5_doc, target = here("tables/Table S5_litter.docx"))
## --------------------------------


