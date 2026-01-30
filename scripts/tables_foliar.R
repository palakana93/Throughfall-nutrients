
## Foliar nutrient concentrations (N, P, K) in understory and canopy leaves
## from common tree species, summarized by focal nutrient addition and by treatment
## Concentrations are reported in mg g⁻¹
##
## Outputs:
## - Table 1: understory and canopy means ± SE by focal nutrient addition (± nutrient)
## - Table S3: understory and canopy means ± SE by fertilization treatment (Control, +N, +P, …)
## - Table S6: Type-II ANOVA tables from LMERs (Kenward–Roger df)


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
source(here("scripts/fns_tables.R")) # Table construction and formatting helpers
source(here("scripts/fns_stats.R"))  # Statistical helpers (e.g., diagnostic_plot)


## Understory Foliar Nutrients -------------------
## Read in data
dat <- read.table(here("data/foliar/raw_understory_nutrients.txt"), header = TRUE)

## Reshape to long format (nutrient = N/P/K; conc = concentration) and standardize factor columns
clean.dat <- dat %>% filter(micro == 0) %>% 
  pivot_longer(cols = c(P.conc, K.conc, N.conc), names_to = "nut", values_to = "conc") %>%
  group_by(sp, nut) %>% 
  mutate(N = factor(N, levels = c(0,1)), 
         P = factor(P, levels = c(0,1)), 
         K = factor(K, levels = c(0,1)),
         plot = factor(plot), 
         repl = factor(repl), 
         block = factor(block),
         tmt = factor(tmt),
         sp = factor(sp)) %>%
  mutate(nut = gsub(".conc", "", nut)) %>%
  select(plot, repl, block, tmt, sp, N, P, K, nut, conc)

## Number of plots per species
clean.dat %>% filter(nut == "P") %>% summarize(n = n())
## Alseis and Heisteria have leaves from 31/32 plots; Protium has leaves from 32/32 plots

## Mean ± SE and relative change by focal fertilization (within each nutrient × species)
## Note: raw N is reported in %; convert to mg g⁻¹ by multiplying by 10
## P/N/K are factors with levels 0 (–) and 1 (+)
mean.SE.und <- clean.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  mutate(fert = case_when(nut == "P" ~ P,
                          nut == "N" ~ N,
                          TRUE       ~ K)) %>%
  group_by(sp, nut, fert) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc), num.cols = n()) %>%
  mutate(nut = factor(nut, levels = c("N", "P", "K"))) %>%
  arrange(nut, sp, fert) %>%
  mutate(pctChng = mean.conc/first(mean.conc)) %>%
  mutate(num.digits = nchar(gsub(".*\\.", "", formatC(mean.conc, digits = 3, flag = "#")))) %>%
  rowwise() %>%
  mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
                      formatC(sd.conc/sqrt(num.cols), digits = num.digits, format = "f"))) %>%
  select(sp, nut, fert, stat, num.cols, pctChng)

## Percent change: with focal nutrient relative to without focal nutrient
mean.SE.und %>% filter(fert == 1) %>%
  select(sp, nut, pctChng) %>%
  mutate(pctChng = formatC(100*(pctChng - 1), digits = 0, format = "f"))
## N: 1/6/8% PRO/ALS/HEI increase
## P: 35/71/91% PRO/HEI/ALS increase
## K: 10/17/26% HEI/PRO/ALS increase

## Average percent change (average of the three species)
mean.SE.und %>% filter(fert == 1) %>%
  select(sp, nut, pctChng) %>%
  group_by(nut) %>%
  summarize(pctChng = mean(pctChng)) %>%
  mutate(pctChng = formatC(100*(pctChng - 1), digits = 0, format = "f"))
## N: 5%, P: 66%, K: 18%

## Average percent changes irrespective of species
clean.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  mutate(fert = case_when(nut == "P" ~ P,
                          nut == "N" ~ N,
                          TRUE       ~ K)) %>%
  group_by(nut, fert) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc), num.cols = n()) %>%
  mutate(nut = factor(nut, levels = c("N", "P", "K"))) %>%
  arrange(nut, fert) %>%
  mutate(pctChng = mean.conc/first(mean.conc)) %>% 
  filter(fert == 1) %>%
  select(nut, pctChng) %>%
  mutate(pctChng = formatC(100*(pctChng - 1), digits = 0, format = "f"))
## N: 7%, P: 68%, K: 21%
## --------------------------------------


## Canopy Foliar Nutrients -------------------
## Read in data
canopy.dat <- read.table(here("data/foliar/raw_canopy_nutrients.csv"), header = TRUE, sep = ",")
canopy.dat <- canopy.dat %>% filter(Treatment != "Micronutrient") %>%
  rename(plot = Plot, tmt = Treatment, sp = Species) %>%
  mutate(plot = factor(plot))

## Join canopy nutrient data with plot-level fertilization metadata (N/P/K indicators, blocks/replicates)
clean.canopy.dat <- clean.dat %>% ungroup %>% 
  select(plot, repl, block, N, P, K) %>% unique() %>%
  full_join(canopy.dat, by = "plot")

## Reshape to long format (nutrient = N/P/K; conc = concentration) and standardize factor columns
clean.canopy.dat <- clean.canopy.dat %>%
  pivot_longer(cols = c(P.conc, K.conc, N.conc), names_to = "nut", values_to = "conc") %>%
  group_by(sp, nut) %>%
  mutate(tmt = factor(tmt),
         sp = factor(sp)) %>%
  mutate(nut = gsub(".conc", "", nut)) %>%
  select(plot, repl, block, tmt, sp, N, P, K, nut, conc)

## Number of plots per species
clean.canopy.dat %>% filter(nut == "P") %>% summarize(n = n())
## Canopy leaf data from 20, 26, and 28 out of 32 plots

## Mean ± SE and relative change by focal fertilization (within each nutrient × species)
## Note: raw N is reported in %; convert to mg g⁻¹ by multiplying by 10
## P/N/K are factors with levels 0 (–) and 1 (+)
mean.SE.canopy <- clean.canopy.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  mutate(fert = case_when(nut == "P" ~ P,
                          nut == "N" ~ N,
                          TRUE       ~ K)) %>%
  group_by(sp, nut, fert) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc), num.cols = n()) %>%
  mutate(nut = factor(nut, levels = c("N", "P", "K"))) %>%
  arrange(nut, sp, fert) %>%
  mutate(pctChng = mean.conc/first(mean.conc)) %>%
  mutate(num.digits = nchar(gsub(".*\\.", "", formatC(mean.conc, digits = 3, flag = "#")))) %>%
  rowwise() %>%
  mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
                      formatC(sd.conc/sqrt(num.cols), digits = num.digits, format = "f"))) %>%
  select(sp, nut, fert, stat, num.cols, pctChng)

## Percent change: with focal nutrient relative to without focal nutrient
mean.SE.canopy %>% filter(fert == 1) %>%
  select(sp, nut, pctChng) %>%
  mutate(pctChng = formatC(100*(pctChng - 1), digits = 0, format = "f"))
## P: 37/41/50% ALS/SIM/PRO increase
## N: -2/7/14% SIM/ALS/PRO increase
## K: 5/21/33% SIM/ALS/PRO increase

## Average percent change (average of the three species)
mean.SE.canopy %>% filter(fert == 1) %>%
  select(sp, nut, pctChng) %>%
  group_by(nut) %>%
  summarize(pctChng = mean(pctChng)) %>%
  mutate(pctChng = formatC(100*(pctChng - 1), digits = 0, format = "f"))
## N: 7%, P: 42%, K: 20%

## Average percent changes irrespective of species
clean.canopy.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  mutate(fert = case_when(nut == "P" ~ P,
                          nut == "N" ~ N,
                          TRUE       ~ K)) %>%
  group_by(nut, fert) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc), num.cols = n()) %>%
  mutate(nut = factor(nut, levels = c("N", "P", "K"))) %>%
  arrange(nut, fert) %>%
  mutate(pctChng = mean.conc/first(mean.conc)) %>% 
  filter(fert == 1) %>%
  select(nut, pctChng) %>%
  mutate(pctChng = formatC(100*(pctChng - 1), digits = 0, format = "f"))
## N: 4%, P: 44%, K: 17%
## --------------------------------------


## Make Tables -------------------
## Table 1: means ± SE by focal nutrient addition (± nutrient), for understory and canopy data
## Table S3: means ± SE by full factorial treatment (Control, +N, +P, +NP, …), for understory and canopy data

## Get understory table dataframe
foliar_understory <- mean.SE.und %>%
  mutate(stat = paste0(stat, " (n = ", num.cols, ")")) %>%
  select(-c(pctChng, num.cols)) %>%
  pivot_wider(names_from = "nut", values_from = "stat") %>%
  mutate(fert = factor(fert, levels = c(0, 1),
                       labels = c("– focal nutrient", "+ focal nutrient"))) %>%
  mutate(sp = factor(sp, levels = c("ALSEBL", "HEISCO", "TET2PA"),
                     labels = c("Alseis blackiana", "Heisteria concinna", "Protium stevensonii")))

## Get canopy table dataframe
foliar_canopy <- mean.SE.canopy %>% 
  mutate(stat = paste0(stat, " (n = ", num.cols, ")")) %>%
  select(-c(pctChng, num.cols)) %>%
  pivot_wider(names_from = "nut", values_from = "stat") %>%
  mutate(fert = factor(fert, levels = c(0, 1),
                       labels = c("– focal nutrient", "+ focal nutrient"))) %>%
  mutate(sp = factor(sp, levels = c("ALSEBL", "SIMAAM", "TET2PA"),
                     labels = c("Alseis blackiana", "Simarouba amara", "Protium stevensonii")))

## Make Table 1
table1_colnames <- names(foliar_understory)
foliar_display_labels <- c("Species", "Treatment", "N (mg g\u207b\u00b9)", "P (mg g\u207b\u00b9)", "K (mg g\u207b\u00b9)")

t1 <- bind_rows(section_row(table1_colnames, "Understory Leaves"),
                foliar_understory)
t2 <- bind_rows(section_row(table1_colnames, "Canopy Leaves"),
                foliar_canopy)
table1 <- bind_rows(t1, t2)

## Column names
table1_header <- setNames(foliar_display_labels, table1_colnames)

## Column widths
table1_widths <- c(1.3, 1.1, 1.3, 1.4, 1.3)

## Save table to document (landscape orientation)
table1_doc <- read_docx()
table1_doc <- add_table(table1_doc, table1, table1_header, table1_widths, 
                        title = "Table 1. Effects of fertilization on understory and canopy foliar nutrients")
## Print table
# print(table1_doc, target = here("tables/Table 1_foliar.docx"))


## For Table S3
## Understory nutrients: Mean +/- SE by treatment
mean.SE.und.tmt <- clean.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  group_by(sp, nut, tmt) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc), num.cols = n()) %>%
  mutate(nut = factor(nut, levels = c("N", "P", "K"))) %>%
  arrange(nut, sp, tmt) %>%
  mutate(num.digits = nchar(gsub(".*\\.", "", formatC(mean.conc, digits = 3, flag = "#")))) %>%
  rowwise() %>%
  mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
                      formatC(sd.conc/sqrt(num.cols), digits = num.digits, format = "f"))) %>%
  select(sp, nut, tmt, stat, num.cols)

mean.SE.und.tmt.table <- mean.SE.und.tmt %>%
  mutate(stat = paste0(stat, " (n = ", num.cols, ")")) %>%
  select(-num.cols) %>%
  pivot_wider(names_from = "nut", values_from = "stat") %>%
  mutate(tmt = factor(tmt, levels = c("CTL", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                      labels = c("Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK"))) %>%
  mutate(sp = factor(sp, levels = c("ALSEBL", "HEISCO", "TET2PA"),
                     labels = c("Alseis blackiana", "Heisteria concinna", "Protium stevensonii"))) %>%
  arrange(sp, tmt)

  
## Canopy nutrients: Mean +/- SE by treatment
mean.SE.canopy.tmt <- clean.canopy.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  group_by(sp, nut, tmt) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc), num.cols = n()) %>%
  mutate(nut = factor(nut, levels = c("N", "P", "K"))) %>%
  arrange(nut, sp, tmt) %>%
  mutate(num.digits = nchar(gsub(".*\\.", "", formatC(mean.conc, digits = 3, flag = "#")))) %>%
  rowwise() %>%
  mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
                      formatC(sd.conc/sqrt(num.cols), digits = num.digits, format = "f"))) %>%
  select(sp, nut, tmt, stat, num.cols)

mean.SE.canopy.tmt.table <- mean.SE.canopy.tmt %>%
  mutate(stat = paste0(stat, " (n = ", num.cols, ")")) %>%
  select(-num.cols) %>%
  pivot_wider(names_from = "nut", values_from = "stat") %>%
  mutate(tmt = factor(tmt, levels = c("Control", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                      labels = c("Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK"))) %>%
  mutate(sp = factor(sp, levels = c("ALSEBL", "SIMAAM", "TET2PA"),
                     labels = c("Alseis blackiana", "Simarouba amara", "Protium stevensonii"))) %>%
  arrange(sp, tmt)


## Make Table S3
tableS3_colnames <- names(mean.SE.und.tmt.table)
foliar_display_labels <- c("Species", "Treatment", "N (mg g\u207b\u00b9)", "P (mg g\u207b\u00b9)", "K (mg g\u207b\u00b9)")

t1 <- bind_rows(section_row(tableS3_colnames, "Understory Leaves"),
                mean.SE.und.tmt.table)
t2 <- bind_rows(section_row(tableS3_colnames, "Canopy Leaves"),
                mean.SE.canopy.tmt.table)
tableS3 <- bind_rows(t1, t2)

## Column names
tableS3_header <- setNames(foliar_display_labels, tableS3_colnames)

## Column widths
tableS3_widths <- c(1.4, 0.8, 1.3, 1.4, 1.3)

## Save table to document (landscape orientation)
tableS3_doc <- read_docx()
tableS3_doc <- add_table(tableS3_doc, tableS3, tableS3_header, tableS3_widths, 
                        title = "Table S3. Effects of fertilization on understory and canopy foliar nutrients")
## Print table
# print(tableS3_doc, target = here("tables/Table S3_foliar.docx"))
## --------------------------------------


## Tables of F values and P values from LMERs -------------------
## LMERs for log-transformed foliar concentrations:
## log(conc) ~ N*P + N*K + P*K + species + (1|replicate/block/plot)

## Understory and canopy LMERs
lmer_mod <- log(conc) ~ N*P + N*K + P*K + sp + (1|repl/block/plot)

specs <- tibble::tribble(
  ~section,                               ~dat,              ~nut,
  "Understory Foliar P Concentration",    clean.dat,         "P",
  "Understory Foliar N Concentration",    clean.dat,         "N",
  "Understory Foliar K Concentration",    clean.dat,         "K",
  "Canopy Foliar P Concentration",        clean.canopy.dat,  "P",
  "Canopy Foliar N Concentration",        clean.canopy.dat,  "N",
  "Canopy Foliar K Concentration",        clean.canopy.dat,  "K")

anova_tables <- specs %>%
  mutate(model = map2(dat, nut, ~ lmer(data = filter(.x, nut == .y), lmer_mod)),
         anova_tbl = map(model, ~ anova(.x, type = 2, ddf = "Kenward-Roger") %>% make_ANOVA_table())) %>%
  transmute(section, model, nut,
            table = map2(anova_tbl, section, ~ bind_rows(section_row(names(.x), .y), .x)))

tableS6 <- bind_rows(anova_tables$table)

## Column names
tableS6_header <- setNames(c("Effect", "Numerator DF", "Denominator DF", "F-value", "P-value"),
                           names(tableS6))
## Column widths
tableS6_widths <- c(0.8, 1, 1.2, 0.8, 0.8)

## Save table to document (landscape orientation)
tableS6_doc <- read_docx() %>%
  add_table(tableS6, tableS6_header, tableS6_widths, 
            title = "Table S6. Linear mixed model output for models of understory and canopy foliar nutrients")
## Print table
# print(tableS6_doc, target = here("tables/Table S6_foliar LMERs.docx"))
## --------------------------------------


## Diagnostic plots -------------------
#### Examine diagnostic plots

## Rename section → resp_var so get_model() can be used
anova_tables_2 <- anova_tables %>% rename(resp_var = section)

## Understory: acceptable residual behavior (a few outliers; Q–Q plots generally linear)
## Canopy: acceptable residual behavior (P has a few outliers; Q–Q plots generally linear)
get_model(anova_tables_2, "Understory Foliar P Concentration") %>%
  diagnostic_plot(log(conc), P, filter(clean.dat, nut == "P"))
get_model(anova_tables_2, "Understory Foliar N Concentration") %>%
  diagnostic_plot(log(conc), N, filter(clean.dat, nut == "N"))
get_model(anova_tables_2, "Understory Foliar K Concentration") %>%
  diagnostic_plot(log(conc), K, filter(clean.dat, nut == "K"))
get_model(anova_tables_2, "Canopy Foliar P Concentration") %>%
  diagnostic_plot(log(conc), P, filter(clean.canopy.dat, nut == "P"))
get_model(anova_tables_2, "Canopy Foliar N Concentration") %>%
  diagnostic_plot(log(conc), N, filter(clean.canopy.dat, nut == "N"))
get_model(anova_tables_2, "Canopy Foliar K Concentration") %>%
  diagnostic_plot(log(conc), K, filter(clean.canopy.dat, nut == "K"))
## --------------------------------------

