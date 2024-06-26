
## Annual budgets of throughfall nutrient deposition

rm(list = ls())
library(tidyverse); library(gdata) ## For data processing and visualization
library(ggbreak) ## For broken-axis plots (Fig. 1)
library(patchwork) ## For multi-paneled plots (Fig. 1); see https://patchwork.data-imaginist.com/articles/guides/layout.html
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")


## Read in data
load("GFXdata.RData") ## Load throughfall data frames (see 'ReUp Merge.R' for descriptions) 
litter.filepath <- "/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data/Litter data/Litter merged_plot means.csv"
litter.byPlot <- read.csv(litter.filepath) %>% as.tibble()


##########################################################
## FIGURE 1
## Effect of fertilization treatment on phosphorus recycling
#     Panel (A) Throughfall PO4 concentration (ppm)
#     Panel (B) Throughfall PO4 recycling (mg PO4-P / m2 / event)
#     Panel (C) Litterfall P recycling (kg P / ha / yr)
##########################################################
## Set plot style parameters
legend.labs.P <- c("Rain", " - P", "+ P")
plot.colors <- c("#008DF9", "#FF6E3A", "#9F0162")
scm.default.P <- scale_color_manual(name = "Water Type", labels = legend.labs.P, values = plot.colors)
scm.P.rec <- scale_color_manual(name = "Water Type", labels = legend.labs.P[2:3], values = plot.colors[2:3])

## Function to create boxplots: independent variable vs. treatment
treatmentEffectPlot <- function(data, nutrient, point.size = 1) {
  data %>% ggplot(aes_string(x = "Treatment", y = nutrient, color = paste(substr(nutrient, 0, 1), "fert", sep = "."))) + 
    geom_boxplot(outlier.shape = NA) + theme_bw() + 
    geom_point(position = position_jitter(width = 0.2), pch = 1, show.legend = F, size = point.size) +
    labs(x = NULL, y = NULL, color = NULL) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          legend.position = "none")
}

## Panel (A) -- Throughfall phosphorus concentrations (mg PO4-P / L)
panel.A <- data.nutrients %>%
  mutate(PO4.conc = PO4.conc/1000) %>%
  filter(DI.control == FALSE) %>%
  treatmentEffectPlot("PO4.conc", point.size = 0.8) +
  labs(y = expression(PO[4]*phantom()^{'3-'}~throughfall~concentration~(mg~P~L^-1))) +
  scale_y_break(c(2.2, 5), ticklabels = c(5, 5.5)) +
  expand_limits(y = c(0, 5.5)) +
  scm.default.P +
  ggtitle('a')

## Panel (B) -- Throughfall phosphorus recycling (mg PO4-P / m2 / event)
panel.B <- data.nutrients %>% mutate(PO4.rec = PO4.dep - PO4.rain) %>%
  filter(Treatment != "Rain", DI.control == FALSE) %>%
  group_by(Plot, Treatment, P.fert) %>%
  summarize(PO4.rec = mean(PO4.rec)) %>%
  treatmentEffectPlot("PO4.rec", point.size = 1.5) +
  labs(y = expression(PO[4]*phantom()^{'3-'}~throughfall~recycling~(mg~P~m^-2~event^-1))) +
  expand_limits(y = c(0, 11.8)) +
  scm.P.rec +
  ggtitle('b')

## Panel (C) -- Litterfall phosphorus recycling (kg P / ha / year)
panel.C <- litter.byPlot %>% 
  mutate(Treatment = factor(Treatment, levels = c("Rain", "C", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                            labels = c("Rain", "Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK"))) %>%
  treatmentEffectPlot("P", point.size = 1.5) +
  labs(y = expression(P~litterfall~recycling~(kg~P~ha^-1~yr^-1))) +
  expand_limits(y = c(0, 15)) +
  scm.P.rec +
  ggtitle('c')

layout <- c(
  area(t = 1, l = 1, b = 50, r = 4),
  area(t = 2, l = 5, b = 49, r = 8),
  area(t = 3, l = 9, b = 48, r = 11)
)
panel.A + panel.B + panel.C +
  plot_layout(design = layout)
## ggsave("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Figures/*Figure 1.png", width = 9, height = 4)

## Legend for FIGURE 1 (crop legend from the below figure)
data.nutrients %>%
  mutate(PO4.conc = PO4.conc/1000) %>%
  treatmentEffectPlot("PO4.conc") +
  theme(legend.position = "right", legend.title = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10))


##########################################################
## Means ± SE of treatment fx on nutrients (for Table 1 and Table S2)
##########################################################
##### Set parameters and initialize functions ####
## Names of nutrients ---------------------------------
nuts <- c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na")

## Multiplication factors for annual budgets ----------
## mf = multiplication factor to scale from measured rainfall to annual rainfall 
##   (Average rainfall in 4, 3, or 1 rain events scaled to 2623 mm/y)
mf <- c(2623/(83.6/4), 2623/(31.5/3), 2623/10.9)
## The numbers above for mm rain come from the calculations below
data.nutrients %>% select(Collection, Plot, Treatment, Rain_cm) %>%
  filter(Treatment == "Rain") %>% mutate(Rain_mm = Rain_cm*10) %>%
  group_by(Collection) %>% summarize(mean = mean(Rain_mm), n = n())

## Function to get µ ± SE -------------------------------
## Given the plot means of nutrient concentration or deposition, calculate 
##    µ ± SE (of nutrient conc or dep) by treatment or by fertilizer addition of a focal nutrient
## Option to multiply µ, SE by factor (multFact; used for annual deposition (see above))
stat <- function(x, multFact = 1) {
  ## Shown to 2 significant digits (formatC)
  paste(formatC(multFact*mean(x, na.rm = T), digits = 2, flag = "#"), "±", 
        formatC(multFact*sd(x, na.rm = T)/sqrt(n()), digits = 2, flag = "#"))
}

## Function for nutrient recycling ----------------------
## Calculate nutrient recycling µ ± SE from deposition µ ± SE
## µ_recyc = µ_TND (throughfall nutrient deposition) - µ_RND (rainfall nutrient deposition)
## SE_recyc = sqrt(SE_TND^2 + SE_RND^2)
recyc <- function(x) {
  ## Shown to 2 significant digits (formatC)
  paste(formatC(as.numeric(sub(" .*", "", x)) - as.numeric(sub(" .*", "", first(x))), digits = 2, flag = "#"), "±", 
        formatC(sqrt(as.numeric(sub(".*± ", "", x))^2 + as.numeric(sub(".*± ", "", first(x)))^2), digits = 2, flag = "#"))
}

##### Get averages per plot ####
## Nutrient concentration ----------------
conc.byPlot <- data.nutrients %>% 
  mutate(across(c(PO4.conc, NO3.conc, NH4.conc, N.conc), ~./1000, .names = "{nuts[1:4]}_ppm")) %>%
  group_by(Plot, Treatment, P.fert, N.fert, K.fert) %>%
  summarize(across(paste(nuts, "_ppm", sep = ""), ~mean(., na.rm = T), .names = "{nuts}")) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>% ## No data from plot 23; convert NaN to NA
  ungroup()
## Nutrient deposition -------------------
dep.byPlot <- data.nutrients %>% group_by(Plot, Treatment, P.fert, N.fert, K.fert) %>%
  summarize(across(paste(nuts, ".dep", sep = ""), ~mean(./100, na.rm = T), .names = "{nuts}"),
            Rain_mm = mean(Rain_cm*10)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>% ## No data from plot 23; convert NaN to NA
  ungroup()

##### MAKE THROUGHFALL TABLES ###########
#### 3 metrics: nutrient concentration, annual deposition, annual recycling
#### Each metric has one table grouped by treatment ("treat"; n = 8) 
####   and one table grouped by fertilizer addition of focal nutrient ("fertAdd"; n = 2)
####   For instance; "fertAdd" groups by +P/-P for PO4 and +N/-N for NH4

## NUTRIENT CONCENTRATION ----------------------
## By treatment
conc.treat <- conc.byPlot %>% group_by(Treatment) %>%
  summarize(across(nuts, list(stat = stat)))
#write.csv(conc.treat, "../Figures/*Tables/conc_treat.csv", row.names = F)

## By fert addition
conc.fertAdd <- conc.byPlot %>% filter(Treatment != "Rain") %>%
  pivot_longer(c(PO4:K), names_to = "Nutrient", values_to = "dep") %>%
  rowwise() %>%
  mutate(fert = ifelse(grepl(substr(Nutrient, 0, 1), as.character(Treatment)), "+", "—")) %>%
  group_by(Nutrient, fert) %>%
  summarize(stat = stat(dep)) %>%
  arrange(desc(Nutrient)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  arrange(desc(fert)) %>%
  select(-fert)
#write.csv(conc.fertAdd, "../Figures/*Tables/conc_fertAdd.csv", row.names = F)


## ANNUAL NUTRIENT DEPOSITION ----------------
## By treatment
dep.treat <- dep.byPlot %>% group_by(Treatment) %>%
  summarize(across(nuts[1:4], list(stat = ~stat(., multFact = mf[1]))),
            across(nuts[5], list(stat = ~stat(., multFact = mf[2]))),
            across(nuts[6:8], list(stat = ~stat(., multFact = mf[3]))))
#write.csv(dep.treat, "../Figures/*Tables/dep_treat.csv", row.names = F)

## By fert addition
dep.fertAdd <- dep.byPlot %>% #filter(Treatment != "Rain") %>%
  pivot_longer(c(PO4:K), names_to = "Nutrient", values_to = "dep") %>%
  rowwise() %>%
  mutate(fert = ifelse(grepl(substr(Nutrient, 0, 1), as.character(Treatment)), "+", "—"),
         fert = ifelse(Treatment == "Rain", "Rain", fert)) %>%
  group_by(Nutrient, fert) %>%
  mutate(MF = ifelse(Nutrient != "K", mf[1], mf[2])) %>% #select(Plot, Treatment, Nutrient, dep, MF)
  summarize(stat = stat(dep, multFact = MF)) %>%
  distinct() %>%
  arrange(desc(Nutrient)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  mutate(fert = factor(fert, levels = c("Rain", "—", "+"))) %>%
  arrange(fert)
#write.csv(dep.fertAdd, "../Figures/*Tables/dep_fertAdd.csv", row.names = F)


## ANNUAL NUTRIENT RECYCLING ----------------
## By treatment
recyc.treat <- dep.treat %>%
  mutate(across(paste(nuts, "_stat", sep = ""), list(recyc = recyc))) %>% 
  select(Treatment, contains("recyc")) %>% slice(-1)
#write.csv(recyc.treat, "../Figures/*Tables/recyc_treat.csv", row.names = F)

## By fert addition
recyc.fertAdd <- dep.fertAdd %>% tibble() %>%
  mutate(across(nuts[1:5], list(recyc = recyc))) %>% 
  select(fert, contains("recyc")) %>% filter(fert!="Rain") %>%
  arrange(desc(fert))
#write.csv(recyc.fertAdd, "../Figures/*Tables/recyc_fertAdd.csv", row.names = F)



##### MAKE LITTERFALL TABLES ###########
## Litterfall recycling by treatment
recyc.litter.treat <- litter.byPlot %>% group_by(Treatment) %>%
  summarize(across(c("P", "N", "K", "Mg", "Ca", "Na"),
                   list(#mean = mean, SE = ~sd(.)/sqrt(n()), n = ~n(),
                     stat = stat)))
#write.csv(recyc.litter.treat, "../Figures/*Tables/recyc_litter_treat.csv", row.names = F)

## Litterfall recycling by fert addition of focal nutrient
recyc.litter.fertAdd <- litter.byPlot %>%
  pivot_longer(c(P, N, K), names_to = "Nutrient", values_to = "recyc") %>%
  rowwise() %>%
  mutate(fert = ifelse(grepl(substr(Nutrient, 0, 1), as.character(Treatment)), "+", "—")) %>%
  group_by(Nutrient, fert) %>%
  summarize(stat = stat(recyc)) %>%
  arrange(desc(Nutrient)) %>%
  pivot_wider(names_from = c(Nutrient), values_from = c(stat)) %>%
  arrange(desc(fert))
#write.csv(recyc.litter.fertAdd, "../Figures/*Tables/recyc_litter_fertAdd.csv", row.names = F)




##########################################################
## Some stats (LMERs) and calculations that are used in manuscript
##########################################################
library(lme4); library(lmerTest)
## Litterfall stats: P, N, K recycling
rep.block <- dataForStats %>% select(Plot, Replicate, Block) %>% distinct() %>% tibble()
litter.byPlot2 <- litter.byPlot %>% mutate(Plot = factor(Plot)) %>% left_join(rep.block, by = "Plot")
litter.P.stat <- lmer(data = litter.byPlot2, P ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
litter.N.stat <- lmer(data = litter.byPlot2, N ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
litter.K.stat <- lmer(data = litter.byPlot2, K ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
## Significance for P < 0.01 (< 0.05 or < 0.1 shown in parentheses)
summary(litter.P.stat) ## +P (-N*P at 0.1)
summary(litter.N.stat) ## N.S.
summary(litter.K.stat) ## N.S.


## Stats for litterfall: broken into "mass" and "concentration"
## Litterfall mass
mass.stat <- lmer(data = filter(litter.byPlot2), Mass ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
mass.stat2 <- lmer(data = filter(litter.byPlot2), Mass ~ P.fert + (1|Replicate/Block))
summary(mass.stat) ## N.S.
summary(mass.stat2) ## P = 0.02

## Litterfall concentration of P, N, K
P.stat <- lmer(data = litter.byPlot2, P/Mass ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
N.stat <- lmer(data = litter.byPlot2, N/Mass ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
K.stat <- lmer(data = litter.byPlot2, K/Mass ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
## Significance for P < 0.01 (< 0.05 or < 0.1 shown in parentheses)
summary(P.stat) ## +P, -NP (+K at 0.1)
summary(N.stat) ## N.S.
summary(K.stat) ## +K (-PK at 0.05)


## Percent changes for P litterfall with P fertilization
## 18% increase in litter mass; 77% increase in P concentration
litter.byPlot %>% group_by(P.fert) %>% summarize(mass = mean(Mass)) %>% mutate(mass = 100*mass/first(mass))
litter.byPlot %>% group_by(P.fert) %>% summarize(P.conc = mean(P/Mass)) %>% mutate(P.conc = 100*P.conc/first(P.conc))
litter.byPlot %>% group_by(P.fert) %>% summarize(P = mean(P)) ## Litter P (kg/ha/yr)


## Throughfall stats: Deposition of P, N, K
dep.byPlot2 <- dep.byPlot %>% left_join(rep.block, by = "Plot")
tfall.P.stat <- lmer(data = dep.byPlot2, PO4 ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
tfall.N.stat <- lmer(data = dep.byPlot2, N ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
tfall.K.stat <- lmer(data = dep.byPlot2, K ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
## Significance for P < 0.01 (< 0.05 or < 0.1 shown in parentheses)
summary(tfall.P.stat) ## +P (—N*P at 0.05; —P*K at 0.1)
summary(tfall.N.stat) ## N.S. (+N*P at 0.1)
summary(tfall.K.stat) ## N.S. (+N*P at 0.05)



##########################################################  
## Figure S5: Litterfall N and K recycling broken down by treatment
##########################################################  
legend.labs.N <- c("- N", "+ N")
legend.labs.K <- c("- K", "+ K")
N.colors <- c("#009F81", "#8400CD")
K.colors <- c("#E20134", "#BDA800")
scm.default.N <- scale_color_manual(name = "Water Type", labels = legend.labs.N, values = N.colors)
scm.default.K <- scale_color_manual(name = "Water Type", labels = legend.labs.K, values = K.colors)

panel.A <- litter.byPlot %>% 
  mutate(Treatment = factor(Treatment, levels = c("Rain", "C", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                            labels = c("Rain", "Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK"))) %>%
  treatmentEffectPlot("N", point.size = 1.5) +
  labs(y = expression(N~litterfall~recycling~(kg~ha^-1~yr^-1))) +
  expand_limits(y = c(0, 250)) +
  scm.default.N + ggtitle('a') +
  theme(legend.position = "right", legend.title = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10))

panel.B <- litter.byPlot %>% 
  mutate(Treatment = factor(Treatment, levels = c("Rain", "C", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                            labels = c("Rain", "Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK"))) %>%
  treatmentEffectPlot("K", point.size = 1.5) +
  labs(y = expression(K~litterfall~recycling~(kg~ha^-1~yr^-1))) +
  expand_limits(y = c(0, 110)) +
  scm.default.K + ggtitle('b') +
  theme(legend.position = "right", legend.title = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10))

layout <- c(
  area(t = 1, l = 1, b = 50, r = 4),
  area(t = 1, l = 5, b = 50, r = 8)
)

panel.A + panel.B + plot_layout(design = layout)
#ggsave("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Figures/Figure S5_NK litter rec.png", width = 9, height = 4)




##########################################################  
## Additional Figure: Annual Nutrient Recycling (not in manuscript)
##########################################################  
## Using mean and SE from throughfall, litterfall, rainfall recycling
## Throughfall recycling
tf <- recyc.fertAdd %>%
  select(fert, P = PO4_recyc, N = N_recyc, K = K_recyc) %>%
  pivot_longer(c(P, N, K), names_to = "Nutrient", values_to = "Recyc") %>%
  mutate(Type = "Throughfall")

## Litterfall recycling
lf <- recyc.litter.fertAdd %>%
  pivot_longer(c(P, N, K), names_to = "Nutrient", values_to = "Recyc") %>%
  mutate(Type = "Litterfall")

## Rainfall inputs
rf <- dep.fertAdd %>% filter(fert == "Rain") %>%
  select(P = PO4, N, K) %>%
  pivot_longer(c(P, N, K), names_to = "Nutrient", values_to = "Recyc") %>%
  mutate(Type = "Rainfall")
## Merge throughfall, litterfall, rainfall
recyc <- tf %>% rbind(lf) %>% rbind(rf) %>%
  mutate(fert = ifelse(fert != "Rain", paste(fert, Nutrient, sep = ""), "Rain"),
         fert = sub("—", "-", fert),
         mean.recyc = as.numeric(sub(" .*", "", Recyc)),
         SE.recyc = as.numeric(sub(".*± ", "", Recyc)))


## Get panel labels
dat_text <- data.frame(label = c("a", "b", "c"), Nutrient = c("P", "N", "K")) %>%
  mutate(Nutrient = factor(Nutrient, levels = c("P", "N", "K")))
## Make plot
recyc %>% mutate(Nutrient = factor(Nutrient, c("P", "N", "K")),
                 Type = factor(Type, levels = c("Throughfall", "Litterfall", "Rainfall"))) %>%
  ggplot(aes(x = fert, y = mean.recyc, fill = Type)) + 
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean.recyc - SE.recyc, ymax = mean.recyc + SE.recyc),
                position = position_dodge(0.9), width = c(rep(0.2, 12), rep(0.1, 3))) +
  facet_wrap(~Nutrient, nrow = 1, scales = "free") +
  theme_bw() + 
  geom_abline(slope = 0, lwd = 0.3) +
  scale_fill_manual(values = c("#FF6699", "#669933", "#008DF9")) + 
  labs(x = NULL, y = expression(Nutrient~recycling~(kg~ha^-1~y^-1))) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2) +
  theme(axis.text = element_text(color = "black", size = 10), legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#ggsave("../Figures/Litter v Tfall_3.png", width = 7, height = 3, dpi = 500)



