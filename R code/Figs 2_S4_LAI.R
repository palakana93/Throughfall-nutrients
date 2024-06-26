

## Throughfall nutrients: stats and figures that include leaf area index (LAI) as explanatory variable
## 1) Stats (LMERs) for throughfall nutrient concentration/deposition on LAI and fert treatment
## 2) Figure 2: Throughfall nutrients ~ LAI faceted figure (with statistical fits from LMER regression models)


## Load data and stats packages
## Note: lme4 requires 'MASS' and MASS::select() overrides dplyr::select()
## lmerTest is wrapper for lme4 that gives p-values
rm(list = ls())
library(tidyverse); library(gdata); library(lme4); library(lmerTest) 
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")
load("GFXdata.RData") ## Load data frames (see 'ReUp Merge.R' for descriptions) 

## Function to add columns for Block (north/south) and Replicate (1, 2, 3, 4)
getBlockRepl <- function(df) {
  df %>% mutate(Block = ifelse(Plot %in% c(1,2,4,5,9,12,13,15,19,20,23,24,28,29,30,32), 1, 2)) %>%
    mutate(Replicate = ifelse(Plot %in% c(1:7,10), 1, 0)) %>%
    mutate(Replicate = ifelse(Plot %in% c(9,11:15,17,18), 2, Replicate)) %>%
    mutate(Replicate = ifelse(Plot %in% c(19:24,26,27), 3, Replicate)) %>%
    mutate(Replicate = ifelse(Plot %in% c(28:30,32:36), 4, Replicate))
}

##########################################################
## LMER for LAI
dataLAIstats <- data.LAI %>% getBlockRepl() %>% filter(Treatment != "Rain") %>% rename(P = P.fert, N = N.fert, K = K.fert)
lme.LAI <- lmer(data = dataLAIstats, LAI.4.5 ~ N*P + P*K + N*K + (1|Replicate/Block/Plot))
summary(lme.LAI) ## N.S. (+P at 0.1)
dataLAIstats %>% group_by(P) %>% summarize(LAI = mean(LAI.4.5))
9.04/8.28 ## LAI is 9% greater in +P vs -P plots, but not significant (P = 0.09)


##########################################################
## LMERs for throughfall concentration
##########################################################
## Convert all nutrients to ppm
dataForStats <- dataForStats %>% mutate(PO4_ppm = PO4.conc/1000, NH4_ppm = NH4.conc/1000, NO3_ppm = NO3.conc/1000, N_ppm = N.conc/1000)
dataForPlots <- dataForPlots %>% mutate(PO4_ppm = PO4.conc/1000, NH4_ppm = NH4.conc/1000, NO3_ppm = NO3.conc/1000, N_ppm = N.conc/1000)
## LMERs: Two-way fertilization interacts with LAI, with random effects (replicate, plot, block and rain event)
## Model includes log of rainfall volume as covariate
conc.lme.PO4 <- lmer(data = dataForStats, log(PO4_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
conc.lme.NO3 <- lmer(data = dataForStats, log(NO3_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
conc.lme.NH4 <- lmer(data = dataForStats, log(NH4_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
conc.lme.N <- lmer(data = dataForStats, log(N_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
conc.lme.K <- lmer(data = dataForStats, log(K_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
conc.lme.Ca <- lmer(data = dataForStats, log(Ca_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
conc.lme.Mg <- lmer(data = dataForStats, log(Mg_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
conc.lme.Na <- lmer(data = dataForStats, log(Na_ppm) ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))

## Examine significant terms (listed below for p < 0.01)
summary(conc.lme.PO4) ## +LAI, -Volume, +P:LAI
summary(conc.lme.NO3) ## -Volume
summary(conc.lme.NH4) ## +LAI, -Volume, -N:LAI
summary(conc.lme.N) ## -Volume
summary(conc.lme.K) ## +LAI
summary(conc.lme.Ca) ## +LAI, -N:LAI
summary(conc.lme.Mg) ## +LAI
summary(conc.lme.Na) ## +LAI

## Compute effect sizes of LAI:fert
100*(exp(fixef(conc.lme.PO4)[2][[1]]) - 1) ## (-P) +6.2% PO4 per leaf layer
100*(exp(fixef(conc.lme.PO4)[2][[1]])*exp(fixef(conc.lme.PO4)[5][[1]]) - 1) ## (+P) +20.7% PO4 per leaf layer
100*(exp(fixef(conc.lme.NH4)[2][[1]]) - 1) ## (-N) +9.0% NH4 per leaf layer
100*(exp(fixef(conc.lme.NH4)[2][[1]])*exp(fixef(conc.lme.NH4)[4][[1]]) - 1) ## (+N) +3.7% NH4 per leaf layer
100*(exp(fixef(conc.lme.K)[2][[1]]) - 1) ## +17.7% K per leaf layer
100*(exp(fixef(conc.lme.Mg)[2][[1]]) - 1) ## +8.2% Mg per leaf layer
100*(exp(fixef(conc.lme.Ca)[2][[1]]) - 1) ## (-N) +7.8% Ca per leaf layer
100*(exp(fixef(conc.lme.Ca)[2][[1]])*exp(fixef(conc.lme.Ca)[4][[1]]) - 1) ## (+N) +2.2% Ca per leaf layer
100*(exp(fixef(conc.lme.Na)[2][[1]]) - 1) ## +3.3% Na per leaf layer

## Get coef tables of fixed effects (for Table S1)
printCoefs <- function(x) {
  cf <- coefficients(summary(x)) %>% data.frame()
  cf %>% mutate(Coef = rownames(cf)) %>%
    select(Coef, Est = Estimate, SE = Std..Error, P = `Pr...t..`) %>%
    mutate(Est = formatC(Est, digits = 2, flag = "#"),
           SE = formatC(SE, digits = 2, flag = "#"),
           P_0.01 = ifelse(P < 0.01, "*", ""),
           P = formatC(P, digits = 2, flag = "#"))
}
a <- printCoefs(conc.lme.PO4) %>% mutate(Nut = "PO4")
b <- printCoefs(conc.lme.NO3) %>% mutate(Nut = "NO3")
c <- printCoefs(conc.lme.NH4) %>% mutate(Nut = "NH4")
d <- printCoefs(conc.lme.N) %>% mutate(Nut = "N")
e <- printCoefs(conc.lme.K) %>% mutate(Nut = "K")
f <- printCoefs(conc.lme.Ca) %>% mutate(Nut = "Ca")
g <- printCoefs(conc.lme.Mg) %>% mutate(Nut = "Mg")
h <- printCoefs(conc.lme.Na) %>% mutate(Nut = "Na")
coefs <- rbind(a, b, c, d, e, f, g, h)
#write.csv(coefs, "/Users/palaniakana/Desktop/LMER Fixef Tables.csv", row.names = F)



##########################################################
## Figure 2
##########################################################
## Get mean Volume_mL per nutrient
mean.vol.NP <- dataForPlots %>% summarize(meanVol = mean(Volume_mL, na.rm = T))
mean.vol.K <- dataForPlots %>% filter(!is.na(K_ppm)) %>% summarize(meanVol = mean(Volume_mL, na.rm = T))
mean.vol.CaMg <- dataForPlots %>% filter(!is.na(Mg_ppm)) %>% summarize(meanVol = mean(Volume_mL, na.rm = T))

## Extract parameters from lmer model output; return as data frame
## For concentration (includes volume)
parms.LAIxNut3 <- function(model, interaction = 0) {
  int <- fixef(model)[1][[1]]
  LAI <- fixef(model)[2][[1]]
  if(interaction != 0) LAIxNut <- fixef(model)[interaction][[1]]
  else LAIxNut <- 0
  Vol <- fixef(model)[3][[1]]
  data.frame(int, LAI, LAIxNut, Vol)
}

## For each nutrient, extract LMER regression coefficients (intercept, volume, LAI effect, and LAI*fert effect as applicable)
coefs.conc <- cbind(nut_fert = c("PO4_ - P"), parms.LAIxNut3(conc.lme.PO4), mean.vol.NP) %>%
  rbind(cbind(nut_fert = c("PO4_+ P"), parms.LAIxNut3(conc.lme.PO4, 5), mean.vol.NP)) %>%
  rbind(cbind(nut_fert = c("NH4_ - N"), parms.LAIxNut3(conc.lme.NH4), mean.vol.NP)) %>%
  rbind(cbind(nut_fert = c("NH4_+ N"), parms.LAIxNut3(conc.lme.NH4, 4), mean.vol.NP)) %>%
  rbind(cbind(nut_fert = c("K_All"), parms.LAIxNut3(conc.lme.K), mean.vol.K)) %>%
  rbind(cbind(nut_fert = c("Mg_All"), parms.LAIxNut3(conc.lme.Mg), mean.vol.CaMg)) %>%
  rbind(cbind(nut_fert = c("Ca_ - N"), parms.LAIxNut3(conc.lme.Ca), mean.vol.CaMg)) %>%
  rbind(cbind(nut_fert = c("Ca_+ N"), parms.LAIxNut3(conc.lme.Ca, 4), mean.vol.CaMg)) %>%
  rbind(cbind(nut_fert = c("Na_All"), parms.LAIxNut3(conc.lme.Na), mean.vol.CaMg))

## Get panel labels
dat_text <- data.frame(label = c("a", "b", "c", "d", "e", "f", "g", "h"),
                       Nutrient = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na")) %>%
  mutate(Nutrient = factor(Nutrient, levels = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na"),
                           labels = c("PO[4]*phantom()^{'3-'}*-P", "NO[3]*phantom()^{'-'}*-N", "NH[4]*phantom()^{'+'}*-N", "(NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'})*-N", "K", "Ca", "Mg", "Na")))


## Make Figure 2
## Throughfall nutrient concentration vs. LAI for various nutrients
##   Lines show statistical fits (throughfall volume set to mean value per nutrient)
LAIfacets.conc <- dataForPlots %>% filter(PO4.dep < 90) %>%
  select(P.fert, N.fert, LAI.4.5, PO4_ppm, NH4_ppm, NO3_ppm, K_ppm, Mg_ppm, Ca_ppm, N_ppm, Na_ppm) %>%
  pivot_longer(PO4_ppm:Na_ppm, names_to = "Nutrient", values_to = "Conc") %>%
  mutate(Nutrient = substr(Nutrient, 1, nchar(Nutrient) - 4)) %>%
  mutate(fert_treat = ifelse(P.fert == "Rain", "Rain", "All"),
         fert_treat = ifelse(Nutrient == "PO4" & P.fert == TRUE, "+ P", fert_treat),
         fert_treat = ifelse(Nutrient == "PO4" & P.fert == FALSE, " - P", fert_treat),
         fert_treat = ifelse(Nutrient %in% c("NH4", "Ca") & N.fert == TRUE, "+ N", fert_treat),
         fert_treat = ifelse(Nutrient %in% c("NH4", "Ca") & N.fert == FALSE, " - N", fert_treat)) %>%
  mutate(fert_treat = factor(fert_treat, levels = c("Rain", "All", " - P", "+ P", " - N", "+ N"))) %>%
  mutate(nut_fert = paste(Nutrient, fert_treat, sep = "_")) %>%
  left_join(coefs.conc, by = "nut_fert") %>%
  mutate(Conc_est = exp(int)*exp(LAI*LAI.4.5)*exp(LAIxNut*LAI.4.5)*meanVol^Vol,
         Nutrient = factor(Nutrient, levels = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na"),
                           labels = c("PO[4]*phantom()^{'3-'}*-P", "NO[3]*phantom()^{'-'}*-N", "NH[4]*phantom()^{'+'}*-N", "(NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'})*-N", "K", "Ca", "Mg", "Na"))) %>%
  ggplot(aes(x = LAI.4.5, y = Conc, color = fert_treat)) + 
  geom_point(pch = 1, size = 0.8) + 
  geom_line(aes(y = Conc_est), lwd = 1, show.legend = F) +
  theme_bw() + 
  facet_wrap(~Nutrient, nrow = 4, scales = "free_y", labeller = label_parsed, as.table = T) +
  scale_color_manual(values = c("#008DF9", "#FF5AAF", "#FF6E3A", "#9F0162", "#009F81", "#8400CD")) +
  labs(x = expression(Leaf~Area~Index~(m^2~m^-2)), 
       y = expression(Nutrient~concentration~(mg~L^-1)),
       color = "Fertilization\nTreatment") +
  guides(color = guide_legend(override.aes = list(size = 2, stroke = 1)))  +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2) +
  theme(axis.text = element_text(color = "black", size = 9),
        strip.background = element_rect(fill = "white"))

## Save plot
LAIfacets.conc + scale_y_log10()
#ggsave("../Figures/LAI facets log_conc_all_NEW.png", width = 6, height = 6)


##########################################################
## LMERs for throughfall deposition (mg element per m2 per rain event)
##########################################################
## Two-way fertilization interacts with LAI, with random effects (replicate, plot, and collection)
dep.lme.PO4 <- lmer(data = dataForStats, log(PO4.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
dep.lme.NO3 <- lmer(data = dataForStats, log(NO3.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
dep.lme.NH4 <- lmer(data = dataForStats, log(NH4.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
dep.lme.N <- lmer(data = dataForStats, log(N.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
dep.lme.K <- lmer(data = dataForStats, log(K.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
dep.lme.Mg <- lmer(data = dataForStats, log(Mg.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
dep.lme.Ca <- lmer(data = dataForStats, log(Ca.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
dep.lme.Na <- lmer(data = dataForStats, log(Na.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))

## Examine significant terms (listed below for p < 0.01)
summary(dep.lme.PO4) ## +P:LAI
summary(dep.lme.NO3) ## -LAI
summary(dep.lme.NH4) ## N.S.
summary(dep.lme.N) ## N.S.
summary(dep.lme.K) ## +LAI
summary(dep.lme.Ca) ## -N:LAI
summary(dep.lme.Mg) ## N.S.
summary(dep.lme.Na) ## N.S.

## Compute effect sizes of LAI:fert
100*(exp(fixef(dep.lme.PO4)[2][[1]]) - 1) ## (-P) +1.8% PO4 per leaf layer (N.S.)
100*(exp(fixef(dep.lme.PO4)[2][[1]])*exp(fixef(dep.lme.PO4)[4][[1]]) - 1) ## (+P) +17.7% PO4 per leaf layer
100*(exp(fixef(dep.lme.NH4)[2][[1]]) - 1) ## 4.3% NH4 per leaf layer (N.S.)
100*(exp(fixef(dep.lme.NO3)[2][[1]]) - 1) ## -7.4% NO3 per leaf layer
100*(exp(fixef(dep.lme.K)[2][[1]]) - 1) ## +14.6% K per leaf layer
100*(exp(fixef(dep.lme.Mg)[2][[1]]) - 1) ## 4.9% Mg per leaf layer (N.S.)
100*(exp(fixef(dep.lme.Ca)[2][[1]]) - 1) ## (-N) +4.6% Ca per leaf layer (N.S.)
100*(exp(fixef(dep.lme.Ca)[2][[1]])*exp(fixef(dep.lme.Ca)[3][[1]]) - 1) ## (+N) -2.3% Ca per leaf layer
100*(exp(fixef(dep.lme.Na)[2][[1]]) - 1) ## +0.77% Na per leaf layer (N.S.)

## Get coef tables of fixed effects (for Table S1)
a <- printCoefs(dep.lme.PO4) %>% mutate(Nut = "PO4")
b <- printCoefs(dep.lme.NO3) %>% mutate(Nut = "NO3")
c <- printCoefs(dep.lme.NH4) %>% mutate(Nut = "NH4")
d <- printCoefs(dep.lme.N) %>% mutate(Nut = "N")
e <- printCoefs(dep.lme.K) %>% mutate(Nut = "K")
f <- printCoefs(dep.lme.Ca) %>% mutate(Nut = "Ca")
g <- printCoefs(dep.lme.Mg) %>% mutate(Nut = "Mg")
h <- printCoefs(dep.lme.Na) %>% mutate(Nut = "Na")
coefs <- rbind(a, b, c, d, e, f, g, h)
#write.csv(coefs, "/Users/palaniakana/Desktop/LMER Fixef Tables_Dep.csv", row.names = F)

## Extract parameters from lmer model output; return as data frame
## Coefficients are intercept, LAI effect, and LAI*fertilizer effect
parms.LAIxNut2 <- function(model, interaction = 0) {
  int <- fixef(model)[1][[1]]
  LAI <- fixef(model)[2][[1]]
  if(interaction != 0) LAIxNut <- fixef(model)[interaction][[1]]
  else LAIxNut <- 0
  data.frame(int, LAI, LAIxNut)
}

## For each nutrient, extract LMER regression coefficients (intercept, LAI effect, and LAI*fert effect as applicable)
coefs.dep <- cbind(nut_fert = c("PO4_ - P"), parms.LAIxNut2(dep.lme.PO4)) %>%
  rbind(cbind(nut_fert = c("PO4_+ P"), parms.LAIxNut2(dep.lme.PO4, 4))) %>%
  rbind(cbind(nut_fert = c("NO3_All"), parms.LAIxNut2(dep.lme.NO3))) %>%
  rbind(cbind(nut_fert = c("K_All"), parms.LAIxNut2(dep.lme.K))) %>%
  rbind(cbind(nut_fert = c("Ca_ - N"), parms.LAIxNut2(dep.lme.Ca))) %>%
  rbind(cbind(nut_fert = c("Ca_+ N"), parms.LAIxNut2(dep.lme.Ca, 3)))

##########################################################
## Figure S4
##########################################################
## Throughfall nutrient deposition vs. LAI for various nutrients
##   Lines show statistical fits
LAIfacets.dep <- dataForPlots %>% filter(PO4.dep < 90) %>%
  select(P.fert, N.fert, LAI.4.5, PO4.dep, NH4.dep, NO3.dep, N.dep, K.dep, Mg.dep, Ca.dep, Na.dep) %>%
  pivot_longer(PO4.dep:Na.dep, names_to = "Nutrient", values_to = "Dep") %>%
  mutate(Nutrient = substr(Nutrient, 1, nchar(Nutrient) - 4)) %>%
  mutate(fert_treat = ifelse(P.fert == "Rain", "Rain", "All"),
         fert_treat = ifelse(Nutrient == "PO4" & P.fert == TRUE, "+ P", fert_treat),
         fert_treat = ifelse(Nutrient == "PO4" & P.fert == FALSE, " - P", fert_treat),
         fert_treat = ifelse(Nutrient %in% c("Ca") & N.fert == TRUE, "+ N", fert_treat),
         fert_treat = ifelse(Nutrient %in% c("Ca") & N.fert == FALSE, " - N", fert_treat)) %>%
  mutate(fert_treat = factor(fert_treat, levels = c("Rain", "All", " - P", "+ P", " - N", "+ N"))) %>%
  mutate(nut_fert = paste(Nutrient, fert_treat, sep = "_")) %>%
  left_join(coefs.dep, by = "nut_fert") %>%
  mutate(Dep_est = exp(int)*exp(LAI*LAI.4.5)*exp(LAIxNut*LAI.4.5),
         Nutrient = factor(Nutrient, levels = c("PO4", "NO3", "NH4", "N", "K", "Ca", "Mg", "Na"),
                           labels = c("PO[4]*phantom()^{'3-'}*-P", "NO[3]*phantom()^{'-'}*-N", "NH[4]*phantom()^{'+'}*-N", "(NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'})*-N", "K", "Ca", "Mg", "Na"))) %>%
  ggplot(aes(x = LAI.4.5, y = Dep, color = fert_treat)) + 
  geom_point(pch = 1, size = 0.8) + 
  geom_line(aes(y = Dep_est), lwd = 1, show.legend = F) +
  theme_bw() + 
  facet_wrap(~Nutrient, nrow = 4, scales = "free_y", labeller = label_parsed, as.table = T) +
  scale_color_manual(values = c("#008DF9", "#FF5AAF", "#FF6E3A", "#9F0162", "#009F81", "#8400CD")) +
  labs(x = expression(Leaf~Area~Index~(m^2~m^-2)), 
       y = expression(Nutrient~deposition~(mg~m^-2~event^-1)),
       color = "Fertilization\nTreatment") +
  guides(color = guide_legend(override.aes = list(size = 2, stroke = 1.0))) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2) +
  theme(axis.text = element_text(color = "black", size = 9),
        strip.background = element_rect(fill = "white"))

## Save plot
LAIfacets.dep + scale_y_log10()
#ggsave("../Figures/LAI facets log_dep.png", width = 6, height = 6)


##########################################################
## Compare R2-values of deposition and concentration models
## See https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
library(MuMIn)
r.squaredGLMM(conc.lme.PO4); r.squaredGLMM(dep.lme.PO4)
r.squaredGLMM(conc.lme.NO3); r.squaredGLMM(dep.lme.NO3)
r.squaredGLMM(conc.lme.NH4); r.squaredGLMM(dep.lme.NH4)
r.squaredGLMM(conc.lme.N); r.squaredGLMM(dep.lme.N)
r.squaredGLMM(conc.lme.K); r.squaredGLMM(dep.lme.K)
r.squaredGLMM(conc.lme.Ca); r.squaredGLMM(dep.lme.Ca)
r.squaredGLMM(conc.lme.Mg); r.squaredGLMM(dep.lme.Mg)
r.squaredGLMM(conc.lme.Na); r.squaredGLMM(dep.lme.Na)


##########################################################
## Concentration models with main effects of fert treatment
## Didn't use these because no significant effects (run code below to see this)
main.lme.PO4 <- lmer(data = dataForStats, log(PO4_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.NH4 <- lmer(data = dataForStats, log(NH4_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.NO3 <- lmer(data = dataForStats, log(NO3_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.N <- lmer(data = dataForStats, log(N_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.K <- lmer(data = dataForStats, log(K_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.Mg <- lmer(data = dataForStats, log(Mg_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
main.lme.Ca <- lmer(data = dataForStats, log(Ca_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
main.lme.Na <- lmer(data = dataForStats, log(Na_ppm) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))

## No significant main effects at p < 0.01 in any model
summary(main.lme.PO4)
summary(main.lme.NH4)
summary(main.lme.NO3)
summary(main.lme.N)
summary(main.lme.K)
summary(main.lme.Mg)
summary(main.lme.Ca)
summary(main.lme.Na)


##########################################################
## Deposition models with main effects of fert treatment
main.lme.PO4 <- lmer(data = dataForStats, log(PO4.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.NH4 <- lmer(data = dataForStats, log(NH4.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.NO3 <- lmer(data = dataForStats, log(NO3.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.N <- lmer(data = dataForStats, log(N.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.K <- lmer(data = dataForStats, log(K.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
main.lme.Mg <- lmer(data = dataForStats, log(Mg.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
main.lme.Ca <- lmer(data = dataForStats, log(Ca.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))
main.lme.Na <- lmer(data = dataForStats, log(Na.dep) ~ N*P + P*K + N*K + LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot))

## No significant main effects at p < 0.01 in any model
summary(main.lme.PO4)
summary(main.lme.NH4)
summary(main.lme.NO3)
summary(main.lme.N)
summary(main.lme.K)
summary(main.lme.Mg)
summary(main.lme.Ca)
summary(main.lme.Na)



