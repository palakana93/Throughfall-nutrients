
## Combined throughfall + litterfall figures
## Fig 1 (P), Fig 2 (K), Fig 3 (N): three panels —
##   (a) throughfall concentration
##   (b) throughfall canopy-to-soil transfers (plot means)
##   (c) litterfall flux (plot means)
## Fig S3: throughfall NO3 and NH4 (concentration + transfers -- 4 panels)


## Clean workspace to ensure figures are generated from a fresh session
## (safe because this script is intended to be run, not sourced)
if (sys.nframe() == 0) {
  rm(list = ls())
}

## Packages:
## - ggbreak, cowplot: broken y-axis with legend (Fig 1A)
## - patchwork: multi-panel layouts 
##   https://patchwork.data-imaginist.com/articles/guides/layout.html
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(ggbreak)
  library(cowplot)
  library(patchwork)
})

## Load shared helper functions
source(here("scripts/fns_data_prep.R")) # Factor recoding and data-prep helpers
source(here("scripts/fns_plotting.R")) # Shared plotting helpers (e.g., treatmentEffectPlot, add_theme)

## Read in data and standardize factor levels
## - raw_throughfall.csv: event-level rain and throughfall chemistry
## - processed_throughfall_plot_means.csv: plot-level throughfall chemistry (averaged across events)
## - processed_litter_plot_means.csv: plot-level annual litter fluxes
dat_tfall <- read_csv(here("data/throughfall/raw_throughfall.csv")) %>% 
  factorize_tfall_columns()
tfall_byPlot <- read_csv(here("data/throughfall/processed_throughfall_plot_means.csv")) %>%
  factorize_tfall_columns()
dat_litter <- read_csv(here("data/litter/processed_litter_plot_means.csv")) %>%
  factorize_litter_columns()

## Compute throughfall nutrient transfers by plot
## Transfer is defined as throughfall deposition minus rainfall deposition,
## representing net nutrient flux from the canopy to the soil

## 1) Calculate mean rainfall deposition for each nutrient
##    (averaged across rain-only collectors; units: mg m⁻² per event)
rain_dep_means <- tfall_byPlot %>%
  filter(Treatment == "Rain") %>%
  summarize(across(contains(".dep"), ~ mean(.x, na.rm = TRUE))) %>%
  unlist()

## 2) Subtract rainfall deposition from throughfall deposition for each plot
##    to obtain plot-level nutrient transfers (mg m⁻² per event)
transfers_byPlot <- tfall_byPlot %>%
  filter(Treatment != "Rain") %>%
  mutate(across(contains(".dep"), ~ .x - rain_dep_means[cur_column()], 
                .names = "{sub('\\\\.dep$', '', .col)}_transfers"))


## Patchwork layouts
## For layout.P, panel A intentionally spans a different vertical extent to visually accommodate
## the broken y-axis; using a uniform grid causes the broken-axis panel to appear compressed
layout.P <- c(area(t = 1, l = 1, b = 51, r = 5),
              area(t = 2, l = 6, b = 50, r = 9),
              area(t = 2, l = 10, b = 50, r = 13))
layout.NK <- c(area(t = 1, l = 1, b = 50, r = 4),
               area(t = 1, l = 5, b = 50, r = 8),
               area(t = 1, l = 9, b = 50, r = 12))
layout.NO3.NH4 <- c(area(t = 1, l = 1, b = 50, r = 4),
                    area(t = 1, l = 5, b = 50, r = 8),
                    area(t = 51, l = 1, b = 100, r = 4),
                    area(t = 51, l = 5, b = 100, r = 8))

## Set style parameters
# PO4 / P
legend.labs.P <- c("Rain", " - P", "+ P")
plot.colors.P <- c("#008DF9", "#FF6E3A", "#9F0162")
scm.default.P <- scale_color_manual(name = "Color", labels = legend.labs.P, values = plot.colors.P)
scm.transfers.P <- scale_color_manual(name = "Color", labels = legend.labs.P[2:3], values = plot.colors.P[2:3])

# NO3 / NH4 / N
legend.labs.N <- c("Rain", " - N", "+ N")
plot.colors.N <- c("#008DF9", "#009F81", "#8400CD")
scm.default.N <- scale_color_manual(name = "Color", labels = legend.labs.N, values = plot.colors.N)
scm.transfers.N <- scale_color_manual(name = "Color", labels = legend.labs.N[2:3], values = plot.colors.N[2:3])

# K
legend.labs.K <- c("Rain", " - K", "+ K")
plot.colors.K <- c("#008DF9", "#E20134", "#BDA800")
scm.default.K <- scale_color_manual(name = "Color", labels = legend.labs.K, values = plot.colors.K)
scm.transfers.K <- scale_color_manual(name = "Color", labels = legend.labs.K[2:3], values = plot.colors.K[2:3])


##########################################################
## FIGURE 1. Effect of fertilization treatment on phosphorus transfers
## Panels:
##     (A) Throughfall PO4 concentration (mg P / L)
##     (B) Throughfall PO4 canopy-to-soil transfers (mg P / m2 / event; plot means)
##     (C) Litterfall P flux (kg P / ha / yr; plot means)
##########################################################
## Panel (A) -- Throughfall PO4 concentration (mg P / L)
# Use a broken y-axis to accommodate high PO4 values
fig1_A_with_break <- dat_tfall %>%
  treatmentEffectPlot("PO4_ppm", point.size = 0.8) %>%
  add_theme(scm = scm.default.P) +
  labs(y = expression(PO[4]*phantom()^{'3-'}~throughfall~concentration~(mg~P~L^-1))) +
  expand_limits(y = c(0, 5.5)) +
  ggtitle('a') +
  ggbreak::scale_y_break(c(2.2, 5), ticklabels = c(5, 5.5))

# Capture the fully rendered plot (including axis break marks) as a grob
# This is required because ggbreak draws break indicators at print time
fig1_A_grob <- grid::grid.grabExpr(print(fig1_A_with_break))

# Create a separate plot solely for extracting the legend
# This avoids layout issues that arise when combining ggbreak plots and legends
panel_for_legend <- dat_tfall %>%
  treatmentEffectPlot("PO4_ppm", point.size = 0.8) %>%
  add_theme(scm = scm.default.P, legend.pos = "right")

# Extract legend grob for manual placement within panel A
fig1_legend <- cowplot::get_legend(panel_for_legend)

# Combine the broken-axis plot and the extracted legend into a single panel
# Legend is inset to maintain consistent layout across multi-panel figures
fig1_A <- cowplot::ggdraw() +
  cowplot::draw_grob(fig1_A_grob, x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_grob(fig1_legend, x = 0.25, y = 0.48, width = 0.35, height = 0.25)

## Panel (B) -- Plot means of throughfall PO4 canopy-to-soil transfers (mg P / m2 / event)
fig1_B <- transfers_byPlot %>% 
  treatmentEffectPlot("PO4_transfers", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.P) +
  labs(y = expression(PO[4]*phantom()^{'3-'}~throughfall~transfers~(mg~P~m^-2~event^-1))) +
  expand_limits(y = c(0, 11.8)) +
  ggtitle('b')

## Panel (C) -- Plot means of litterfall phosphorus flux (kg P / ha / year)
fig1_C <- dat_litter %>% 
  treatmentEffectPlot("P_flux", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.P) +
  labs(y = expression(P~litterfall~flux~(kg~P~ha^-1~yr^-1))) +
  expand_limits(y = c(0, 15)) +
  ggtitle('c')

## Combine panels into a single figure
fig1_A + fig1_B + fig1_C + patchwork::plot_layout(design = layout.P)
# ggsave(here("figures/Figure 1.png"), width = 9, height = 4) # Uncomment to save figure


##########################################################
## FIGURE 2. Effect of fertilization treatment on potassium transfers
## Panels:
##     (A) Throughfall K concentration (mg K / L)
##     (B) Throughfall K canopy-to-soil transfers (mg K / m2 / event; plot means)
##     (C) Litterfall K flux (kg K / ha / yr; plot means)
##########################################################
## Panel (A) -- Throughfall K concentration (mg K / L)
fig2_A <- dat_tfall %>%
  treatmentEffectPlot("K_ppm", point.size = 0.8) %>%
  add_theme(scm = scm.default.K, legend.pos = "inside", legend.coord = c(0.45, 0.96)) +
  labs(y = expression(K~throughfall~concentration~(mg~K~L^-1))) +
  ggtitle('a')

## Panel (B) -- Plot means of throughfall K canopy-to-soil transfers (mg K / m2 / event)
fig2_B <- transfers_byPlot %>% 
  treatmentEffectPlot("K_transfers", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.K) +
  labs(y = expression(K~throughfall~transfers~(mg~K~m^-2~event^-1))) +
  expand_limits(y = c(0, 50)) +
  ggtitle('b')

## Panel (C) -- Plot means of litterfall K flux (kg K / ha / yr)
fig2_C <- dat_litter %>% 
  treatmentEffectPlot("K_flux", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.K) +
  labs(y = expression(K~litterfall~flux~(kg~K~ha^-1~yr^-1))) +
  expand_limits(y = c(0, 100)) +
  ggtitle('c')

## Combine panels into a single figure
fig2_A + fig2_B + fig2_C + plot_layout(design = layout.NK)
# ggsave(here("figures/Figure 2.png"), width = 9, height = 4) # Uncomment to save figure


##########################################################
## FIGURE 3. Effect of fertilization treatment on nitrogen transfers
## Panels:
##     (A) Throughfall inorganic N concentration (mg N / L)
##     (B) Throughfall inorganic N canopy-to-soil transfers (mg N / m2 / event; plot means)
##     (C) Litterfall N flux (kg N / ha / yr; plot means)
##########################################################
## Panel (A) -- Throughfall inorganic N concentration (mg N / L)
fig3_A <- dat_tfall %>%
  treatmentEffectPlot("N_ppm", point.size = 0.8) %>%
  add_theme(scm = scm.default.N, legend.pos = "inside", legend.coord = c(0.5, 0.8)) +
  labs(y = expression(Inorganic~N~throughfall~concentration~(mg~N~L^-1))) +
  ggtitle('a')

## Panel (B) -- Plot means of throughfall inorganic N canopy-to-soil transfers (mg N / m2 / event)
## Note: N.dep is provided in the throughfall dataset; N.rain is computed here as NO3.rain + NH4.rain
fig3_B <- transfers_byPlot %>% 
  treatmentEffectPlot("N_transfers", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.N) +
  labs(y = expression(Inorganic~N~throughfall~transfers~(mg~N~m^-2~event^-1))) +
  expand_limits(y = c(-2, 4)) +
  ggtitle('b')

## Panel (C) -- Plot means of litterfall N flux (kg N / ha / yr)
fig3_C <- dat_litter %>%
  treatmentEffectPlot("N_flux", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.N) +
  labs(y = expression(N~litterfall~flux~(kg~N~ha^-1~yr^-1))) +
  expand_limits(y = c(0, 250)) +
  ggtitle('c')

## Combine panels into a single figure
fig3_A + fig3_B + fig3_C + plot_layout(design = layout.NK)
# ggsave(here("figures/Figure 3.png"), width = 9, height = 4) # Uncomment to save figure


##########################################################
## Supplemental Figure: Fig S3
## Effect of fertilization treatment on nitrate and ammonium transfers
##     Panel (A) Throughfall NO3 concentration (mg NO3-N / L)
##     Panel (B) Throughfall NO3 canopy-to-soil transfers (mg NO3-N / m2 / event; plot means)
##     Panel (C) Throughfall NH4 concentration (mg NH4-N / L)
##     Panel (D) Throughfall NH4 canopy-to-soil transfers (mg NH4-N / m2 / event; plot means)
##########################################################
## Panel (A) -- Throughfall NO3 concentration (mg NO3-N / L)
figS3_A <- dat_tfall %>%
  treatmentEffectPlot("NO3_ppm", point.size = 0.8) %>%
  add_theme(scm = scm.default.N, legend.pos = "inside", legend.coord = c(0.6, 0.8)) +
  labs(y = expression(NO[3]*phantom()^{'-'}*~throughfall~concentration~(mg~N~L^-1))) +
  ggtitle('a')

## Panel (B) -- Plot means of throughfall NO3 canopy-to-soil transfers (mg NO3-N / m2 / event)
figS3_B <- transfers_byPlot %>% 
  treatmentEffectPlot("NO3_transfers", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.N) +
  labs(y = expression(NO[3]*phantom()^{'-'}*~throughfall~transfers~(mg~N~m^-2~event^-1))) +
  ggtitle('b')

## Panel (C) -- Throughfall NH4 concentration (mg NH4-N / L)
figS3_C <- dat_tfall %>%
  treatmentEffectPlot("NH4_ppm", point.size = 0.8)  %>%
  add_theme(scm = scm.default.N) +
  labs(y = expression(NH[4]*phantom()^{'+'}*~throughfall~concentration~(mg~N~L^-1))) +
  ggtitle('c')

## Panel (D) -- Plot means of throughfall NH4 canopy-to-soil transfers (mg NH4-N / m2 / event)
figS3_D <- transfers_byPlot %>% 
  treatmentEffectPlot("NH4_transfers", point.size = 1.5) %>%
  add_theme(scm = scm.transfers.N) +
  labs(y = expression(NH4*phantom()^{'+'}*~throughfall~transfers~(mg~N~m^-2~event^-1))) +
  expand_limits(y = c(0, 4)) +
  ggtitle('d')

## Combine panels into a single figure
figS3_A + figS3_B + figS3_C + figS3_D + plot_layout(design = layout.NO3.NH4)
# ggsave(here("figures/Figure S3.png"), width = 6, height = 8) # Uncomment to save figure

