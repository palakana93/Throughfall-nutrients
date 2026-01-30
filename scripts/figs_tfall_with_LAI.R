
## Throughfall nutrients as a function of leaf area index (LAI)
## 1) Mixed-effects models (LMERs) for concentration and deposition of various nutrients
## 2) Table S2: Model output from LMERs
## 3) Figures 4 and S4: Throughfall nutrients vs. LAI with model-based fits


## Clean workspace to ensure figures are generated from a fresh session
## (safe because this script is intended to be run, not sourced)
if (sys.nframe() == 0) {
  rm(list = ls())
}

## Packages
## Notes:
## - lmerTest extends lme4 and provides p-values via Satterthwaite/Kenward–Roger methods
## - Some packages import MASS; if MASS is attached, MASS::select() can mask dplyr::select()
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lmerTest)
  library(scales) # axis/label helpers for figures
  library(officer) # export tables to Word
  library(flextable) # export tables to Word
  library(MuMIn) # extract R2 values from lmers
})

## Load shared helper functions
source(here("scripts/fns_data_prep.R")) # Factor recoding and data-prep helpers
source(here("scripts/fns_tables.R")) # Table prep helpers
source(here("scripts/fns_stats.R")) # Stat helpers
source(here("scripts/fns_plotting.R")) # Shared plotting helpers (e.g., log_adaptive_breaks)

## Read in data and standardize factor levels
## - raw_throughfall.csv: event-level rain and throughfall chemistry
dat_tfall <- read_csv(here("data/throughfall/raw_throughfall.csv")) %>% 
  factorize_tfall_columns()

## Analysis dataset for LMERs:
## - restrict to observations with LAI (covariate in all models)
## - exclude rain samples (models analyze treatment effects in throughfall)
dataForStats <- dat_tfall %>% filter(!is.na(LAI), Treatment != "Rain")

## Plotting dataset: includes both rain and throughfall samples where LAI is available
dataForPlots <- dat_tfall %>% filter(!is.na(LAI)) %>%
  filter(!is.na(PO4_ppm) | !is.na(NO3_ppm | !is.na(NH4_ppm)))


##########################################################
## LMERs for throughfall nutrients
##########################################################

## Model structure notes (applies to all models below):
## - Fixed effects: LAI and two-way fertilization interactions with LAI
## - Random effects: nested plot structure (Replicate / Block / Plot)
## - Collection (rain event) is included as a random intercept where supported by the response
## - log_TF (in specs tables) controls whether the response is log-transformed in make_lmer()

## -----------------------------
## BY CONCENTRATION (mg element / L)
## -----------------------------

## RHS formula template for most concentration models:
## Include log of throughfall sample volume as a covariate and Collection as a random intercept
lmer_mod_conc <- ~ LAI + log(Volume_mL) + (N*P):LAI + (N*K):LAI + (P*K):LAI + (1|Replicate/Block/Plot) + (1|Collection)

## Alternative RHS template for Ca/Mg/Na:
## Same fixed effects, but omit Collection random effect (data are from only one rain event)
lmer_mod_conc_CaMgNa <- ~ LAI + log(Volume_mL) + (N*P):LAI + (N*K):LAI + (P*K):LAI + (1|Replicate/Block/Plot)

## Convert RHS templates to strings for make_lmer()
rhs_txt  <- as.character(lmer_mod_conc)[2]
rhs_txt_CaMgNa  <- as.character(lmer_mod_conc_CaMgNa)[2]

## Specification table: one row per response variable/model run
specs_tfall_conc <- tibble::tribble(
  ~section,     ~dat,          ~resp_var,   ~log_TF,  ~rhs,
  "PO₄³⁻",      dataForStats,  "PO4_ppm",   TRUE,     rhs_txt,
  "NO₃⁻",       dataForStats,  "NO3_ppm",   TRUE,     rhs_txt,
  "NH₄⁺",       dataForStats,  "NH4_ppm",   TRUE,     rhs_txt,
  "NO₃⁻+NH₄⁺",  dataForStats,  "N_ppm",     TRUE,     rhs_txt,
  "K",          dataForStats,  "K_ppm",     TRUE,     rhs_txt,
  "Ca",         dataForStats,  "Ca_ppm",    TRUE,     rhs_txt_CaMgNa,
  "Mg",         dataForStats,  "Mg_ppm",    TRUE,     rhs_txt_CaMgNa,
  "Na",         dataForStats,  "Na_ppm",    TRUE,     rhs_txt_CaMgNa)

## Fit models and compute Type-II ANOVA tables (Kenward–Roger df)
anova_tables_tfall_conc <- specs_tfall_conc %>%
  mutate(model = pmap(list(dat, resp_var, rhs, log_TF), make_lmer),
         anova_tbl = map(model, ~ anova(.x, type = 2, ddf = "Kenward-Roger") %>% make_ANOVA_table())) %>%
  select(section, resp_var, model, anova_tbl)


## -----------------------------
## BY DEPOSITION (mg element / m2 / event)
## -----------------------------

## RHS template for deposition models:
## Deposition already incorporates water amount, so omit log(Volume_mL)
lmer_mod_dep <- ~ LAI + (N*P):LAI + (N*K):LAI + (P*K):LAI + (1|Replicate/Block/Plot) + (1|Collection)

## Alternative deposition RHS for Ca/Mg/Na: omit Collection random effect as above
lmer_mod_dep_CaMgNa <- ~ LAI + (N*P):LAI + (N*K):LAI + (P*K):LAI + (1|Replicate/Block/Plot)

## Convert RHS templates to strings for make_lmer()
rhs_txt_dep  <- as.character(lmer_mod_dep)[2]
rhs_txt_dep_CaMgNa  <- as.character(lmer_mod_dep_CaMgNa)[2]

## Specification table for deposition responses
specs_tfall_dep <- tibble::tribble(
  ~section,                ~dat,          ~resp_var,   ~log_TF,  ~rhs,
  "Throughfall PO4 Dep",   dataForStats,  "PO4.dep",   TRUE,     rhs_txt_dep,
  "Throughfall NO3 Dep",   dataForStats,  "NO3.dep",   TRUE,     rhs_txt_dep,
  "Throughfall NH4 Dep",   dataForStats,  "NH4.dep",   TRUE,     rhs_txt_dep,
  "Throughfall N Dep",     dataForStats,  "N.dep",     TRUE,     rhs_txt_dep,
  "Throughfall K Dep",     dataForStats,  "K.dep",     TRUE,     rhs_txt_dep,
  "Throughfall Ca Dep",    dataForStats,  "Ca.dep",    TRUE,     rhs_txt_dep_CaMgNa,
  "Throughfall Mg Dep",    dataForStats,  "Mg.dep",    TRUE,     rhs_txt_dep_CaMgNa,
  "Throughfall Na Dep",    dataForStats,  "Na.dep",    TRUE,     rhs_txt_dep_CaMgNa)

## Fit models and compute Type-II ANOVA tables (Kenward–Roger df)
anova_tables_tfall_dep <- specs_tfall_dep %>%
  mutate(model = pmap(list(dat, resp_var, rhs, log_TF), make_lmer),
         anova_tbl = map(model, ~ anova(.x, type = 2, ddf = "Kenward-Roger") %>% make_ANOVA_table())) %>%
  select(section, resp_var, model, anova_tbl)


##########################################################
## Table S2. LMER outputs for throughfall nutrients (concentration and deposition)
##########################################################

## ---- Build per-model coefficient+ANOVA tables (concentration models) ----
## For each fitted model:
## 1) extract coefficient estimates (printCoefs)
## 2) merge with ANOVA output (merge_coefs)
## 3) compute marginal and conditional R² for the concentration model and the corresponding deposition model
## 4) prepend a section header row for the nutrient that includes both R² values (section_row)
anova_tables_tfall_conc_2 <- anova_tables_tfall_conc %>%
  mutate(coefs = map(model, printCoefs)) %>%
  mutate(coefs = map2(coefs, anova_tbl, merge_coefs),
         R2_txt_conc = map_chr(resp_var, ~ r2_txt(anova_tables_tfall_conc, .x)),
         R2_txt_dep = map_chr(resp_var, ~ r2_txt(anova_tables_tfall_dep, sub("_ppm$", ".dep", .x)))) %>%
  mutate(table = pmap(list(coefs, section, R2_txt_conc, R2_txt_dep), 
                      ~ bind_rows(section_row(names(..1), paste0(..2, strrep("\t",2), strrep(" ", 8), ..3, strrep("\t", 6), ..4)), ..1)))

## Stack all nutrient sections into a single table
tableS2 <- bind_rows(anova_tables_tfall_conc_2$table)

## ---- Build per-model tables (deposition models; mg m^-2 event^-1) ----
anova_tables_tfall_dep_2 <- anova_tables_tfall_dep %>%
  mutate(coefs = map(model, printCoefs)) %>%
  mutate(coefs = map2(coefs, anova_tbl, merge_coefs)) %>%
  mutate(table = map2(coefs, section, ~ bind_rows(section_row(names(.x), .y), .x)))

tableS2_dep <- bind_rows(anova_tables_tfall_dep_2$table)

## ---- Join concentration + deposition tables side-by-side ----
## Join is performed by Effect, but Effect labels repeat within each section
## To preserve row alignment within each Effect, add a within-Effect row index (.idx) in both tables
tableS2_joined <- tableS2 %>%
  group_by(Effect) %>%
  mutate(.idx = row_number()) %>%
  ungroup() %>%
  left_join(tableS2_dep %>%
              group_by(Effect) %>%
              mutate(.idx = row_number()) %>%
              ungroup(),
            by = c("Effect", ".idx")) %>%
  select(-.idx)

## ---- Post-processing: df formatting, row ordering, missing-value display ----
tableS2_joined <- tableS2_joined %>%
  ## Combine numerator and denominator df into one display column
  mutate(dfs.x = ifelse(is.na(df_num.x), NA, paste0(df_num.x, ", ", round(as.numeric(df_den.x), 1))),
         dfs.y = ifelse(is.na(df_num.y), NA, paste0(df_num.y, ", ", round(as.numeric(df_den.y), 1))),
         ## Swap log(Volume_mL) row above LAI to match reporting convention (LAI shown first)
         ## (This is a presentation-only adjustment; it does not change model results)
         .row = row_number(),
         .row = case_when(Effect == "log(Volume_mL)" & .row > 1L ~ .row - 1L,
                          lead(Effect) == "log(Volume_mL)" ~ .row + 1L, 
                          TRUE ~ .row)) %>%
  arrange(.row, row_number()) %>%
  ## Drop intermediate df columns
  select(-c(.row, df_num.x, df_den.x, df_num.y, df_den.y)) %>%
  ## Place df columns next to SE for readability
  relocate(dfs.x, .after = SE.x) %>%
  relocate(dfs.y, .after = SE.y) %>%
  ## Replace blank/NA cells with display-friendly values:
  ## - P < 0.01 indicator: blank if not applicable
  ## - other character columns: use en dash for missing entries in rows with estimates
  mutate(across(c(`P<0.01.x`, `P<0.01.y`), ~ ifelse(is.na(.x), "", .x)),
         across(where(is.character), ~ ifelse(is.na(.x) & !is.na(Est.x), "–", .x)))


## ---- Table header text + column widths ----
tableS2_header <- setNames(c("Effect", "Estimate", "SE", "df (num, den)", "F value", "P value", "P < 0.01", 
                                       "Estimate", "SE", "df (num, den)", "F value", "P value", "P < 0.01"),
                           names(tableS2_joined))

## Column widths (Word) — concentration block then deposition block
tableS2_widths <- c(1.2, rep(c(0.7, 0.6, 0.9, 0.6, 0.6, 0.7), 2))

## ---- Preheader row: label the two model blocks (concentration vs. deposition) ----
## This row is a separate flextable so we can span/merge columns visually above the main table
table_preheader <- tibble(c1 = "", c2 = "Natural log of concentration (mg L\u207b\u00b9)", 
                          c3 = "Natural log of deposition (mg m\u207b\u00b2 event\u207b\u00b9)")

preheader_widths <- c(tableS2_widths[1], sum(tableS2_widths[2:7]), sum(tableS2_widths[8:13]))

ft_pre <- table_preheader %>% flextable() %>%
  delete_part(part = "header") %>%
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border_remove() %>%
  width(j = 1:3, width = preheader_widths) %>%
  set_table_properties(layout = "fixed", align = "left") %>% 
  hline_bottom(border = fp_border(width = 0.75), part = "body") %>%
  vline(j = c(1,2), border = fp_border(width = 0.5), part = "body")

## ---- Export to Word (landscape page orientation) ----
tableS2_doc <- read_docx()
sec_landscape <- prop_section(page_size = page_size(orient = "landscape"))
tableS2_doc <- body_set_default_section(tableS2_doc, sec_landscape)
tableS2_doc <- tableS2_doc %>% 
  body_add_fpar(fpar(ftext("Table S2. Linear mixed model output for models of throughfall nutrients with LAI as a fixed effect", 
                           fp_text(font.family = "Times New Roman", bold = TRUE, font.size = 12)))) %>%
  body_add_flextable(ft_pre) %>%
  add_table(tableS2_joined, tableS2_header, tableS2_widths,
            title = NULL,
            vline_j = c(1,7)) ## Vertical line after 1st and 7th columns

# print(tableS2_doc, target = here("tables/Table S2_joined_tfall LMERs.docx")) # Uncomment to save table

##########################################################


##########################################################
## Figure 4. Throughfall nutrient concentration vs LAI
## Faceted scatterplots with fitted lines from LMERs (volume held at nutrient-specific mean)
##########################################################

## ---- Prepare covariate values used in prediction lines ----
## For model-based fitted lines, set Volume_mL to the mean value for each nutrient group
## (Some nutrients have missing values in subsets; compute means on the subset where the nutrient is observed)
mean.vol.NP <- dataForPlots %>% summarize(meanVol = mean(Volume_mL, na.rm = T)) %>% pull()
mean.vol.K <- dataForPlots %>% filter(!is.na(K_ppm)) %>% summarize(meanVol = mean(Volume_mL, na.rm = T)) %>% pull()
mean.vol.CaMg <- dataForPlots %>% filter(!is.na(Mg_ppm)) %>% summarize(meanVol = mean(Volume_mL, na.rm = T)) %>% pull()

## ---- Extract fitted model parameters used to draw prediction lines ----
## parms.LAIxNut() returns the parameters needed for prediction in log space:
##   int      = intercept term in the fitted model (in log space)
##   Estimate = LAI slope term (or LAI×treatment slope, depending on nutrient/group)
##   Vol      = coefficient for log(Volume_mL) in the concentration models
##
## get_model(<resp>) retrieves the fitted LMER object from the concentration model table
PO4_coefs_F4 <- anova_tables_tfall_conc_2 %>% get_model("PO4_ppm") %>% parms.LAIxNut()
NH4_coefs_F4 <- anova_tables_tfall_conc_2 %>% get_model("NH4_ppm") %>% parms.LAIxNut()
K_coefs_F4 <- anova_tables_tfall_conc_2 %>% get_model("K_ppm") %>% parms.LAIxNut()
Ca_coefs_F4 <- anova_tables_tfall_conc_2 %>% get_model("Ca_ppm") %>% parms.LAIxNut()
Mg_coefs_F4 <- anova_tables_tfall_conc_2 %>% get_model("Mg_ppm") %>% parms.LAIxNut()
Na_coefs_F4 <- anova_tables_tfall_conc_2 %>% get_model("Na_ppm") %>% parms.LAIxNut()

## ---- Define treatment groupings used for plotting fitted slopes ----
## For PO4, slopes differ among P × N groups; average slopes within each group for display
## For other nutrients, a single LAI slope is used across treatments in this figure ("All")
##
## NOTE: the grouping below collapses the full factorial Treatment levels into 4 P x N groups:
##   -P/-N, -P/+N, +P/-N, +P/+N
PO4_coefs_sig_F4 <- PO4_coefs_F4 %>% 
  mutate(Group = case_when(
    Treatment %in% c("Control", "+K") ~ "-P, -N",
    Treatment %in% c("+N", "+NK") ~ "-P, +N",
    Treatment %in% c("+P", "+PK") ~ "+P, -N",
    Treatment %in% c("+NP", "+NPK") ~ "+P, +N")) %>%
  group_by(Group) %>%
  ## Average slope estimate within each P x N group (used for the fitted lines)
  mutate(Estimate = mean(Estimate, na.rm = TRUE),
         Nutrient = "PO4",
         meanVol = mean.vol.NP) %>%
  ungroup()

## For NH4/K/Ca/Mg/Na: use a single LAI slope (averaged across treatment levels) for plotting
## NOTE: Estimate[1:8] used because the first 8 rows correspond to the factorial Treatment levels
NH4_coefs_sig_F4 <- NH4_coefs_F4 %>% mutate(Estimate = mean(NH4_coefs_F4$Estimate[1:8]), Nutrient = "NH4", Group = "All", meanVol = mean.vol.NP)
K_coefs_sig_F4 <- K_coefs_F4 %>% mutate(Estimate = mean(K_coefs_F4$Estimate[1:8]), Nutrient = "K", Group = "All", meanVol = mean.vol.K)
Ca_coefs_sig_F4 <- Ca_coefs_F4 %>% mutate(Estimate = mean(Ca_coefs_F4$Estimate[1:8]), Nutrient = "Ca", Group = "All", meanVol = mean.vol.CaMg)
Mg_coefs_sig_F4 <- Mg_coefs_F4 %>% mutate(Estimate = mean(Mg_coefs_F4$Estimate[1:8]), Nutrient = "Mg", Group = "All", meanVol = mean.vol.CaMg)
Na_coefs_sig_F4 <- Na_coefs_F4 %>% mutate(Estimate = mean(Na_coefs_F4$Estimate[1:8]), Nutrient = "Na", Group = "All", meanVol = mean.vol.CaMg)

## Combine per-nutrient slope specs for joining onto the plotting dataframe
coefs.sig <- rbind(PO4_coefs_sig_F4, NH4_coefs_sig_F4, K_coefs_sig_F4, Ca_coefs_sig_F4, Mg_coefs_sig_F4, Na_coefs_sig_F4)

## ---- Percent change per leaf layer (reported in Results) ----
## Convert LAI slopes from log space to percent change per unit LAI: 100*(exp(beta)-1)
## PO4 is reported by P x N group; other nutrients use the single "All" slope
get_percent_change <- function(df) df %>% mutate(Pct.change = 100*(exp(Estimate)-1)) %>% select(Pct.change) %>% unique() %>% pull()

## PO4
PO4_coefs_sig_F4 %>% mutate(Pct.change = 100*(exp(Estimate)-1)) %>% select(Treatment, Pct.change)
## -P, -N: 5.8% increase per leaf layer
## -P, +N: 3.2% increase per leaf layer
## +P, -N: 20.9% increase per leaf layer
## +P, +N: 17.0% increase per leaf layer

## All other nutrients
NH4_coefs_sig_F4 %>% get_percent_change() ## 7.6% increase per leaf layer
K_coefs_sig_F4 %>% get_percent_change() ## 17.5% increase per leaf layer
Ca_coefs_sig_F4 %>% get_percent_change() ## 5.9% increase per leaf layer
Mg_coefs_sig_F4 %>% get_percent_change() ## 7.2% increase per leaf layer
Na_coefs_sig_F4 %>% get_percent_change() ## 2.8% increase per leaf layer


## ---- Panel labels for facets (a–h) ----
dat_text <- data.frame(label = c("a", "b", "c", "d", "e", "f", "g", "h"),
                       Nutrient = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na")) %>%
  mutate(Nutrient = factor(Nutrient, levels = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na"),
                           labels = c("PO[4]*phantom()^{'3-'}*-P", "NO[3]*phantom()^{'-'}*-N", "NH[4]*phantom()^{'+'}*-N", "(NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'})*-N", "K", "Ca", "Mg", "Na")))

## ---- Build long-format plotting dataframe and compute fitted values ----
## Observed points are plotted as concentration vs LAI
## Fitted lines are computed from LMER coefficients with Volume_mL fixed at the mean for that nutrient group:
##   log(Conc_est) = int + Estimate*LAI + Vol*log(meanVol) => Conc_est = exp(int) * exp(Estimate*LAI) * meanVol^Vol
dataForPlots %>%
  select(Treatment, P, N, LAI, PO4_ppm, NH4_ppm, NO3_ppm, K_ppm, Mg_ppm, Ca_ppm, N_ppm, Na_ppm) %>%
  pivot_longer(PO4_ppm:Na_ppm, names_to = "Nutrient", values_to = "Conc") %>%
  mutate(Nutrient = substr(Nutrient, 1, nchar(Nutrient) - 4)) %>%
  left_join(coefs.sig, by = c("Nutrient", "Treatment")) %>%
  ## Define plotting groups (Rain vs treatment groups; missing groups default to "All")
  mutate(Group = if_else(P == "Rain", "Rain", Group),
         Group = if_else(is.na(Group), "All", Group),
         Group = factor(Group, levels = c("Rain", "All", "-P, -N", "-P, +N", "+P, -N", "+P, +N"))) %>%
  ## Compute predicted concentration along LAI gradient
  mutate(Conc_est = exp(int)*exp(Estimate*LAI)*meanVol^Vol,
         Nutrient = factor(Nutrient, levels = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na"),
                           labels = c("PO[4]*phantom()^{'3-'}*-P", "NO[3]*phantom()^{'-'}*-N", "NH[4]*phantom()^{'+'}*-N", "(NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'})*-N", "K", "Ca", "Mg", "Na"))) %>%
  ggplot(aes(x = LAI, y = Conc, color = Group)) + 
  ## Observed points
  geom_point(aes(shape = Group), size = 0.8) + 
  ## Fitted lines from model coefficients
  geom_line(aes(y = Conc_est, linetype = Group), lwd = 0.5) +
  theme_bw() + 
  facet_wrap(~Nutrient, nrow = 4, scales = "free_y", labeller = label_parsed, as.table = T) +
  ## Aesthetic mappings for groups
  scale_shape_manual(values = c("Rain" = 1, "All" = 1, "+P, +N" = 0, "+P, -N" = 4, "-P, +N" = 0, "-P, -N" = 4)) +
  scale_linetype_manual(values = c("Rain" = "solid", "All" = "solid", "+P, +N" = "dashed", "+P, -N" = "solid", "-P, +N" = "dashed", "-P, -N" = "solid")) +
  scale_color_manual(values = c("Rain" = "#008DF9", "All" = "#FF5AAF", "-P, -N" = "#FF6E3A", "-P, +N" = "#FF6E3A", "+P, -N" = "#9F0162", "+P, +N" = "#9F0162")) +
  labs(x = expression(Leaf~Area~Index~(m^2~m^-2)), 
       y = expression(Nutrient~concentration~(mg~L^-1)),
       color = "Fertilization\nTreatment", 
       shape = "Fertilization\nTreatment",
       linetype = "Fertilization\nTreatment") +
  guides(color = guide_legend(override.aes = list(size = 2, stroke = 0.8))) +
  ## Panel labels (a–h) in top-left of each facet
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2) +
  theme(axis.text = element_text(color = "black", size = 9),
        strip.background = element_rect(fill = "white"),
        legend.key.width = unit(1.1, "cm")) +
  scale_y_log10(breaks = log_adaptive_breaks, labels = label_number())

## Save plot
# ggsave(here("figures/Figure 4.png"), width = 6, height = 6) # Uncomment to save plot
##########################################################


##########################################################
## Figure S4. Throughfall nutrient deposition vs LAI
## Faceted scatterplots with fitted lines from deposition LMERs
##########################################################

## ---- Extract fitted model parameters used to draw prediction lines ----
## parms.LAIxNut() returns the parameters needed for prediction in log space:
##   int      = intercept term in the fitted model (in log space)
##   Estimate = LAI slope term (or LAI×treatment slope, depending on nutrient/group)
##
## get_model(<resp>) retrieves the fitted LMER object from the deposition model table
PO4_coefs_FS4 <- anova_tables_tfall_dep_2 %>% get_model("PO4.dep") %>% parms.LAIxNut()
NO3_coefs_FS4 <- anova_tables_tfall_dep_2 %>% get_model("NO3.dep") %>% parms.LAIxNut()
K_coefs_FS4 <- anova_tables_tfall_dep_2 %>% get_model("K.dep") %>% parms.LAIxNut()

## ---- Define treatment groupings used for plotting fitted slopes ----
## For PO4, slopes differ among P × N groups; average slopes within each group for display
## For NO3 and K, a single LAI slope is used across treatments in this figure ("All")
PO4_coefs_sig_FS4 <- PO4_coefs_FS4 %>% 
  mutate(Group = case_when(
    Treatment %in% c("Control", "+K") ~ "-P, -N",
    Treatment %in% c("+N", "+NK") ~ "-P, +N",
    Treatment %in% c("+P", "+PK") ~ "+P, -N",
    Treatment %in% c("+NP", "+NPK") ~ "+P, +N")) %>%
  group_by(Group) %>%
  ## Average LAI slope within each P x N group (used for fitted lines)
  mutate(Estimate = mean(Estimate, na.rm = TRUE),
         Nutrient = "PO4") %>%
  ungroup()

## NOTE: Estimate[1:8] used because the first 8 rows correspond to the factorial Treatment levels
NO3_coefs_sig_FS4 <- NO3_coefs_FS4 %>% mutate(Estimate = mean(NO3_coefs_FS4$Estimate[1:8]), Nutrient = "NO3", Group = "All")
K_coefs_sig_FS4 <- K_coefs_FS4 %>% mutate(Estimate = mean(K_coefs_FS4$Estimate[1:8]), Nutrient = "K", Group = "All")

## Combine per-nutrient slope specs for joining onto the plotting dataframe
coefs.sig <- rbind(PO4_coefs_sig_FS4, NO3_coefs_sig_FS4, K_coefs_sig_FS4)

## ---- Percent change per leaf layer (reported in Results) ----
## Convert LAI slopes from log space to percent change per unit LAI: 100*(exp(beta)-1)

## PO4 is reported by P x N group; NO3 and K use the single "All" slope
PO4_coefs_sig_FS4 %>% mutate(Pct.change = 100*(exp(Estimate)-1)) %>% select(Treatment, Pct.change)
## -P, -N: 1.7% increase per leaf layer
## -P, +N: 0.5% decrease per leaf layer
## +P, -N: 17.6% increase per leaf layer
## +P, +N: 13.1% increase per leaf layer
## All other nutrients
NO3_coefs_sig_FS4 %>% get_percent_change() ## 6.0% decrease per leaf layer
K_coefs_sig_FS4 %>% get_percent_change() ## 15.3% increase per leaf layer


## ---- Throughfall nutrient deposition vs LAI (observations + model fits) ----
## Observed points: deposition vs LAI
## Fitted lines: back-transform from the log link:
##   log(Dep_est) = int + Estimate*LAI => Dep_est = exp(int) * exp(Estimate*LAI)
dataForPlots %>%
  select(Treatment, P, N, LAI, PO4.dep, NH4.dep, NO3.dep, N.dep, K.dep, Mg.dep, Ca.dep, Na.dep) %>%
  pivot_longer(PO4.dep:Na.dep, names_to = "Nutrient", values_to = "Dep") %>%
  mutate(Nutrient = substr(Nutrient, 1, nchar(Nutrient) - 4)) %>%
  left_join(coefs.sig, by = c("Nutrient", "Treatment")) %>%
  ## Define plotting groups (Rain vs treatment groups; missing groups default to "All")
  mutate(Group = if_else(P == "Rain", "Rain", Group),
         Group = if_else(is.na(Group), "All", Group),
         Group = factor(Group, levels = c("Rain", "All", "-P, -N", "-P, +N", "+P, -N", "+P, +N"))) %>%
  ## Predicted deposition along LAI gradient (model fit)
  mutate(Dep_est = exp(int)*exp(Estimate*LAI),
         Nutrient = factor(Nutrient, levels = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na"),
                           labels = c("PO[4]*phantom()^{'3-'}*-P", "NO[3]*phantom()^{'-'}*-N", "NH[4]*phantom()^{'+'}*-N", "(NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'})*-N", "K", "Ca", "Mg", "Na"))) %>%
  ggplot(aes(x = LAI, y = Dep, color = Group)) + 
  geom_point(aes(shape = Group), size = 0.8) + 
  geom_line(aes(y = Dep_est, linetype = Group), lwd = 0.5) +
  theme_bw() + 
  facet_wrap(~Nutrient, nrow = 4, scales = "free_y", labeller = label_parsed, as.table = T) +
  ## Aesthetic mappings for groups (consistent with Fig 2)
  scale_shape_manual(values = c("Rain" = 1, "All" = 1, "+P, +N" = 0, "+P, -N" = 4, "-P, +N" = 0, "-P, -N" = 4)) +
  scale_linetype_manual(values = c("Rain" = "solid", "All" = "solid", "+P, +N" = "dashed", "+P, -N" = "solid", "-P, +N" = "dashed", "-P, -N" = "solid")) +
  scale_color_manual(values = c("Rain" = "#008DF9", "All" = "#FF5AAF", "-P, -N" = "#FF6E3A", "-P, +N" = "#FF6E3A", "+P, -N" = "#9F0162", "+P, +N" = "#9F0162")) +
  labs(x = expression(Leaf~Area~Index~(m^2~m^-2)), 
       y = expression(Nutrient~deposition~(mg~m^-2~event^-1)),
       color = "Fertilization\nTreatment", 
       shape = "Fertilization\nTreatment",
       linetype = "Fertilization\nTreatment") +
  guides(color = guide_legend(override.aes = list(size = 2, stroke = 0.8)))  +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2) +
  theme(axis.text = element_text(color = "black", size = 9),
        strip.background = element_rect(fill = "white"),
        legend.key.width = unit(1.1, "cm")) +
  scale_y_log10(breaks = log_adaptive_breaks, labels = label_number())

## Save plot
# ggsave(here("figures/Figure S4.png"), width = 6, height = 6)
##########################################################

