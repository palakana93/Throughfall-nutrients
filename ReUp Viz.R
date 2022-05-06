

## Figures and data exploration for GFX throughfall data


#######    ########    ########    ########    ########    ########   
## Note: Need to adjust concentrations based on concentrations in blanks
## Note: May want to remove rain outlier(s)
## August 2021 update: 
##    4-93 rain makes sense to exclude: very high PO4 and NH4
##    Only NH4 has elevated concentrations in lab blanks; a correction mostly affects
##       collection 1 (low NH4 concentrations) and pushes many of these values negative
#######    ########    ########    ########    ########    ########   

rm(list = ls())
library(tidyverse); library(gdata)
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")
source("../R scripts/ReUp fns.R")
load("GFXdata.RData") ## Load data frames (see 'ReUp Merge.R' for descriptions) 


##########################################################
## Main figures
##########################################################
## Set plot parameters
#legend.labs.P <- c("\nRain\n", "\nThroughfall\n(No P Added)", "\nThroughfall\n(P Added)")
legend.labs.P <- c("Rain", " - P", "+ P")
legend.labs.N <- c("\nRain\n", "\nThroughfall\n(No N Added)", "\nThroughfall\n(N Added)")
legend.labs.K <- c("\nRain\n", "\nThroughfall\n(No K Added)", "\nThroughfall\n(K Added)")
#plot.colors <- c("#00798c", "#edae49", "#d1495b")
#plot.colors.K <- c("#00798c", "dark green", "purple")
plot.colors <- c("#008DF9", "#FF6E3A", "#9F0162")
plot.colors.K <- c("#008DF9", "#FFC33B", "#E20134")
scm.default.P <- scale_color_manual(name = "Water Type", labels = legend.labs.P, values = plot.colors)
scm.default.N <- scale_color_manual(name = "Water Type", labels = legend.labs.N, values = plot.colors)
scm.default.K <- scale_color_manual(name = "Water Type", labels = legend.labs.K, values = plot.colors.K)



## Treatment effect plots
########################################################## 
## PO4 deposition
########################################################## 
## Add 'ggbreak' library to incorporate one PO4 outlier (1-42)
##   Note: If you use ggbreak in published research, please cite the following paper: S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers. Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846
library(ggbreak) 

## CONCENTRATION (ppm)
data.nutrients %>%
  mutate(PO4.conc = PO4.conc/1000) %>%
  treatmentEffectPlot("PO4.conc") +
  labs(x = NULL, y = expression(PO[4]~concentration~(ppm))) +
  #theme(legend.position = "right", legend.title = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10)) + 
  theme(legend.position = "none") +
  scale_y_break(c(2.2, 5), ticklabels = c(5, 5.5)) +
  scm.default.P +
  expand_limits(y = c(0, 5.5))
## ggsave("../Figures/PO4 treatment conc.png", width = 6, height = 5)

## Average concentrations
data.nutrients %>% group_by(P.fert) %>% 
  summarize(mean = mean(PO4.conc/1000), n = n(), se = sd(PO4.conc/1000)/sqrt(n()))
data.nutrients %>% filter(Treatment != "Rain") %>%
  summarize(mean = mean(N.conc/1000), n = n(), se = sd(N.conc/1000)/sqrt(n()))
data.nutrients %>% filter(Treatment != "Rain", !is.na(K_ppm)) %>%
  summarize(mean = mean(K_ppm), n = n(), se = sd(K_ppm)/sqrt(n()))

## DRY DEPOSITION (using sodium as tracer ion)
## Assumptions: all Na from dry dep (none from leaching) and all elements have same wet:dry dep ratio
data.nutrients %>% group_by(Type) %>%
  filter(!is.na(Na_ppm)) %>%
  summarize(Na_conc = mean(Na_ppm), Na_dep = mean(Na.dep), n = n())
(11.3-9.3)/9.3 ## By concentration: DD = 21.5% of wet dep
(114-100)/100 ## By deposition: DD = 14% of wet dep
## What does LAI ~ DD(Na) tell me about how LAI scales with DD of other nutrients??


## DEPOSITION (mg/m2/event)
data.nutrients %>%
  treatmentEffectPlot("PO4.dep") +
  labs(y = expression(PO[4]~deposition~(mg~m^-2~event^-1))) +
  #theme(legend.position = "right", legend.title = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10)) + 
  theme(legend.position = "none") +
  scale_y_break(c(30, 80), ticklabels = c(80, 85)) +
  scm.default.P +
  expand_limits(y = c(0, 85))
## ggsave("../Figures/PO4 treatment dep_2.png", width = 6, height = 5)

## DEPOSITION (as % of rain deposition)
data.nutrients %>% filter(Type != "Rain") %>%
  treatmentEffectPlot("PO4.dep.pct.of.rain") +
  labs(y = "Throughfall P (% of rain)") +
  theme(legend.position = c(0.2, 0.85), legend.title = element_blank(), legend.spacing.y = unit(0, "pt")) +
  geom_hline(yintercept = 100, lty = "dashed") +
  scale_color_manual(labels = c(" - P", "+ P"), values = c("#008DF9", "#FF6E3A")) + scale_y_log10()
## ggsave("../Figures/PO4 treatment_pct of rain.png", width = 4, height = 3)


########################################################## 
## Supplemental Figures: N and K deposition
########################################################## 
## Get panel labels
dat_text <- data.frame(label = c("a", "b", "c", "d"), Nutrient = c("NO3", "NH4", "N", "K")) %>%
  mutate(Nutrient = factor(Nutrient, levels = c("NO3", "NH4", "N", "K"),
                           labels = c("NO[3]*phantom()^{'-'}", "NH[4]*phantom()^{'+'}", "NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'}", "K")))
## Make figure
data.nutrients <- data.nutrients %>% mutate(NH4_ppm = NH4.conc/1000, NO3_ppm = NO3.conc/1000, N_ppm = N.conc/1000)
data.nutrients %>% pivot_longer(c(NH4_ppm, NO3_ppm, N_ppm, K_ppm), names_to = "Nutrient", values_to = "Conc_ppm") %>%
  mutate(Nutrient = factor(Nutrient, levels = c("NO3_ppm", "NH4_ppm", "N_ppm", "K_ppm"),
                           labels = c("NO[3]*phantom()^{'-'}", "NH[4]*phantom()^{'+'}", "NO[3]*phantom()^{'-'}~plus~NH[4]*phantom()^{'+'}", "K")),
         Fert = ifelse(Nutrient == "K", as.character(K.fert), as.character(N.fert)),
         Water_Type = ifelse(Fert == T, paste("+", substr(Nutrient, 0, 1)), paste("-", substr(Nutrient, 0, 1))),
         Water_Type = ifelse(Fert == "Rain", "Rain", Water_Type),
         Water_Type = factor(Water_Type, levels = c("Rain", "- N", "+ N", "- K", "+ K"))) %>%
  ggplot(aes(x = Treatment, y = Conc_ppm, color = Water_Type)) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + 
  geom_point(size = 0.8, position = position_jitter(width = 0.2), pch = 1, show.legend = F) +
  labs(x = NULL, y = "Nutrient Concentration (ppm)", color = NULL) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10), strip.background = element_rect(fill = "white")) + 
  facet_wrap(~Nutrient, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("#008DF9", "#009F81", "#8400CD", "#E20134", "#BDA800")) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2)
#ggsave("../Figures/NK treatment dep.png", width = 6, height = 5)


## LAI on throughfall dep
##########################################################
## Note: Color code according to Rain
##########################################################
data.LAI %>% mutate(Rain = ifelse(P.fert == "Rain", "Rain", "Throughfall")) %>%
  filter(!is.na(Water_cm)) %>%
  ggplot(aes(x = LAI.4.5, y = 100*Water_cm/Rain_cm, color = Rain)) + geom_point(pch = 1) +
  labs(x = expression(Leaf~Area~Index~(m^2~m^-2)), y = "Water deposition (% of rain)") +
  geom_smooth(method = "lm", se = F, color = "black", formula = 'y~x') +
  theme_classic() + geom_abline(intercept = 100, slope = 0, lty = "dashed") +
  scale_color_manual(name = NULL, values = c("#008DF9", "#FF5AAF")) +
  theme(legend.position = c(0.85, 0.85))
## ggsave("../Figures/Water Dep.png", width = 5, height = 3)


## P fert on LAI (by fert treatment)
##########################################################  
data.LAI %>% filter(Treatment != "Rain") %>%
  rename(PAI.4.5 = LAI.4.5) %>%
  mutate(P.fert = ifelse(P.fert == T, "+ K", " - K")) %>%
  treatmentEffectPlot("PAI.4.5") +
  labs(y = expression(Leaf~area~index~(m^2~m^-2))) +
  theme(legend.position = c(0.08, 0.88), legend.title = element_blank(),
        #legend.background = element_rect(color = "red"),
        legend.spacing.y = unit(0, "pt")) +
  #theme(legend.position = "none") +
  #expand_limits(y = c(0, 25)) +
  scale_color_manual(labels = c(" - P", "+ P"), values = c("#FF6E3A", "#9F0162"))
## ggsave("../Figures/LAI x P fert by treat.png", width = 6, height = 4)

data %>% filter(LAI.4.5 > 20)
## Violin plot (not too pretty)
data.LAI %>% filter(Treatment != "Rain") %>%
  ggplot(aes(x = Treatment, y = LAI.4.5, color = P.fert)) + 
  geom_violin(draw_quantiles = c(0.5)) + theme_bw() + labs(x = "Leaf Area Index", y = "Count")


## P versus PO4 concentrations
##########################################################  
data.nutrients %>% filter(!is.na(P_ppm)) %>%
  ggplot(aes(x = PO4.conc/1000, y = P_ppm)) + 
  geom_point(pch = 1) + 
  geom_abline(slope = 1, lty = "dashed") +
  labs(x = expression(PO[4]*phantom()^3~phantom()^-~phantom()~(ppm)), y = "Total P (ppm)") +
  theme_bw()
## ggsave("../Figures/P v PO4 concs.png", width = 4.5, height = 3)


##########################################################
##########################################################
## March 2022: K concentration exploration
K.data <- read.xls("/Users/palaniakana/Desktop/1_GFX/DairyOne payment/Updated Palani Akana Water K raw data 02162022.xlsx")
K.data <- K.data %>% select(Collection = Description.1a, Bottle = Description.1b,
                            Set = Set.., Rpt = Data.rpt, K_ppm = Potassium..K...ppm...rpt, 
                            TW_orig = TW.QC.w.Orig...K..ppm, TW_rerun = TW.QC.w.RR...K..ppm) %>%
  mutate(ID = paste(Collection, Bottle, sep = "-"))
K.err <- K.data %>% mutate(TW = ifelse(Rpt == "orig", TW_orig, TW_rerun)) %>%
  filter(!is.na(TW), TW != "") %>%
  filter(!(Set %in% c("Set 1", "Set 2", "Set 3"))) %>%
  group_by(Set) %>% summarize(mean = mean(as.numeric(TW)), sd = sd(as.numeric(TW))) %>%
  mutate(add = max(mean) - mean,
         DL = add + 0.26) 
K.cor <- K.err %>% select(-c(mean, sd))
K.data <- K.data %>% left_join(K.cor, by = "Set")
K.data <- K.data %>% mutate(K_ppm = K_ppm + add)
K.data <- K.data %>% mutate(K_ppm = ifelse(K_ppm < DL, DL/2, K_ppm))
K.data.to.join <- K.data %>% select(ID, K_ppm_recalc = K_ppm)

dataForPlots %>% mutate(K = K.) %>% filter(Collection %in% c(3, 4)) %>%
  inner_join(K.data.to.join, by = "ID") %>%
  mutate(K = ifelse(K == T, "+ K", " - K")) %>%
  mutate(Collection = paste("Event", Collection)) %>%
  ggplot(aes(x = LAI.4.5, y = K_ppm_recalc, color = K)) +
  scale_color_manual(values = c("#E20134", "#009F81")) +
  geom_point(pch = 1) + facet_wrap(~Collection) + theme_bw() +
  theme(legend.position = c(0.09, 0.8)) +
  labs(color = NULL, x = expression(Leaf~Area~Index~(m^2~m^-2)), #y = expression(K~deposition~(mg~m^-2~event^-1)))
       y = expression(K~concentration~(ppm)))

K.err
K.err %>% summarize(mean(mean), mean(sd))
K.data %>% mutate(TW = ifelse(Rpt == "orig", TW_orig, TW_rerun)) %>%
  filter(!is.na(TW), TW != "") %>%
  summarize(sd = sd(as.numeric(TW)))


## Low K rain deposition from collection 2 tends to be concentrated in top half of plots
data.nutrients %>% mutate(Rain = Treatment == "Rain") %>%
  filter(DI.control == FALSE, K_ppm > 0, Collection == 2) %>%
  mutate(low.K = K_ppm < 1.2) %>%
  ggplot(aes(x = Coord_x, y = Coord_y, color = low.K, shape = Rain)) + 
  geom_point() + theme_bw() + coord_fixed()
data.nutrients %>% filter(Treatment == "Rain", !is.na(K_ppm)) %>% select(ID, K_ppm, Coord_x, Coord_y)


## Sept 2021: K deposition by rain event
dataForStats %>% filter(Collection %in% c(2, 3, 4)) %>% 
  mutate(K = ifelse(K == T, "+ K", " - K")) %>%
  mutate(Collection = paste("Event", Collection)) %>%
  ggplot(aes(x = LAI.4.5, y = K_ppm, color = K)) +
  scale_color_manual(values = c("#E20134", "#009F81")) +
  geom_point(pch = 1) + facet_wrap(~Collection) + theme_bw() +
  theme(legend.position = c(0.09, 0.86)) +
  labs(color = NULL, x = expression(Leaf~Area~Index~(m^2~m^-2)), #y = expression(K~deposition~(mg~m^-2~event^-1)))
       y = expression(K~concentration~(ppm)))
## ggsave("../Figures/K conc by event.png", width = 6, height = 4)
lme.K.adj <- lmer(data = filter(dataForStats, Collection %in% c(2, 3, 4)), 
                  log(K.dep) ~ LAI.4.5 + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
summary(lme.K.adj)
## Even for just collection 3 & 4, K fert is insignificant for both K dep and conc


## Examine some outliers
## Low Mg and Ca
dataForPlots %>% filter(Mg_ppm < 0.3) %>% select(ID, Mg_ppm, Ca_ppm, PO4.conc)


## Using ggplot and base R
library(ggpubr)
residuals(dep.lme.PO4) %>% ggqqplot()
plot(fitted(dep.lme.PO4), residuals(dep.lme.PO4))
hist(residuals(dep.lme.PO4))

ggplot(dataForStats, aes(x = log(Volume_mL), y = log(PO4.conc), color = LAI.4.5, shape = P)) + 
  geom_point() + theme_bw()
ggplot(dataForStats, aes(x = log(Volume_mL), y = log(PO4.dep), color = P)) + geom_point() + theme_bw()



## BC trans for concentration
library(MASS)
bcTransPO4 <- boxcox(lm(dataForStats$PO4.conc ~ 1), lambda = seq(-2, 2, 1/20), plotit = F)
detach(package:MASS)
lambdaPO4 <- bcTransPO4$x[which.max(bcTransPO4$y)]
tempPO4 <- dataForStats %>% mutate(bcPO4 = (PO4.conc^lambdaPO4-1)/lambdaPO4)
bc.lme.PO4 <- lmer(data = tempPO4, bcPO4 ~ LAI.4.5 + log(Volume_mL) + (N*P):LAI.4.5 + (N*K):LAI.4.5 + (P*K):LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
summary(bc.lme.PO4)
residuals(bc.lme.PO4) %>% ggqqplot()



## Shapiro-Wilks test for normality for residuals of all models
lmeList <- list(full.lme.PO4, full.lme.NO3, full.lme.NH4, full.lme.K, full.lme.Ca, 
                full.lme.Mg, bc.lme.PO4, bc.lme.NO3, bc.lme.K)
sw <- rep(0, 9)
for(i in 1:(length(lmeList))) {
  sw[i] <- round(shapiro.test(residuals(lmeList[[i]]))[[2]], 6)
}



## Amount of rain per rain event
data %>% group_by(Collection) %>% filter(Type == "Rain") %>% 
  summarize(mean = 10*mean(Water_cm, na.rm = T), n = n())

## Average LAI
data.LAI %>% filter(Type != "Rain") %>%
  summarize(mean(LAI.4.5, na.rm = T), n = n(), quantile(LAI.4.5))



## +P model for LAI with varied LAI thresholds
summary(lmer(data = dataLAIstats, LAI.4.5 ~ P + (1|Replicate/Block/Plot)))                                  ## LAI 4.5 (p = 0.018)
summary(lmer(data = filter(dataLAIstats, !is.infinite(LAI.2.25)), LAI.2.25 ~ P + (1|Replicate/Block/Plot))) ## LAI 2.25 (p = 0.016)
summary(lmer(data = dataLAIstats, LAI.9 ~ P + (1|Replicate/Block/Plot)))                                    ## LAI 9.0 (N.S.)
summary(lmer(data = filter(dataLAIstats, Gap_Fraction.4.5 < 0.33), LAI.4.5 ~ P + (1|Replicate/Block/Plot))) ## LAI 4.5 (p = 0.011 with high gap fraction (>0.33) excluded)
## Lower thresholds (2.25, 4.5) suggest +P increases LAI by ~0.75, N.S. for threshold of 9



## What's with the low NO3 point? (Low water volume and [NO3] below detection limit)
dataForPlots %>% select(ID, NO3.conc, Volume_mL, NO3.dep) %>% filter(NO3.dep < 0.1)



#### Examine Palm, plants above water collector, etc.
## For Collection 2 (K) or 1 & 2 (N, P)
## Examined these Sept. 2021; some small effects (could be through fx on water volume)

test.lme.PO4 <- lmer(data = filter(dataForStats, as.numeric(Collection) < 3), log(PO4.dep) ~ Palm + X1.4m + Collector + LAI.4.5 + P:LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
test.lme.NH4 <- lmer(data = filter(dataForStats, as.numeric(Collection) < 3), log(NH4.dep) ~ Palm + X1.4m + Collector + LAI.4.5 + N:LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
test.lme.NO3 <- lmer(data = filter(dataForStats, as.numeric(Collection) < 3), log(NO3.dep) ~ Palm + X1.4m + Collector + LAI.4.5 + N:LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
test.lme.K <- lmer(data = filter(dataForStats, Collection == 2), log(K.dep) ~ Palm + X1.4m + Collector + LAI.4.5 + K:LAI.4.5 + (1|Replicate/Block/Plot))
test.lme.vol <- lmer(data = filter(dataForStats, as.numeric(Collection) < 3), log(Volume_mL) ~ Palm + X1.4m + LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))

summary(test.lme.PO4) ## Palm decrease; collector=Palani marginal decrease
summary(test.lme.NH4) ## Palm marginal decrease
summary(test.lme.NO3) ## Veg at 1.4m decrease
summary(test.lme.K)   ## Veg at 1.4m decrease
summary(test.lme.vol) ## Veg at 1.4m decrease

