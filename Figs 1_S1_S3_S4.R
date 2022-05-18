

## Figures and calculations of throughfall data

## Import required libraries
library(tidyverse)
library(gdata)
library(ggbreak) ## For broken-axis plots

## Set working directory
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")

## Load functions and data
#source("../R scripts/ReUp fns.R")
load("GFXdata.RData") ## Load data frames (see 'ReUp Merge.R' for descriptions) 


## Function
## Fertilizer boxplots by treatment
treatmentEffectPlot <- function(data, nutrient) {
  data %>% filter(DI.control == FALSE) %>%
    ggplot(aes_string(x = "Treatment", y = nutrient, color = paste(substr(nutrient, 0, 1), "fert", sep = "."))) + 
    geom_boxplot(outlier.shape = NA) + theme_bw() + 
    geom_point(position = position_jitter(width = 0.2), pch = 1, show.legend = F) +
    labs(x = "Fertilizer Added", y = paste(substr(nutrient, 0, 3), " deposition (mg/m2)", sep = ""), 
         color = paste(substr(nutrient, 0, 1), "fertilized")) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10))
}


## Set plot parameters
legend.labs.P <- c("Rain", " - P", "+ P")
legend.labs.N <- c("\nRain\n", "\nThroughfall\n(No N Added)", "\nThroughfall\n(N Added)")
legend.labs.K <- c("\nRain\n", "\nThroughfall\n(No K Added)", "\nThroughfall\n(K Added)")
plot.colors <- c("#008DF9", "#FF6E3A", "#9F0162")
plot.colors.K <- c("#008DF9", "#FFC33B", "#E20134")
scm.default.P <- scale_color_manual(name = "Water Type", labels = legend.labs.P, values = plot.colors)
scm.default.N <- scale_color_manual(name = "Water Type", labels = legend.labs.N, values = plot.colors)
scm.default.K <- scale_color_manual(name = "Water Type", labels = legend.labs.K, values = plot.colors.K)




##########################################################
## FIGURE 1
## Effect of fertilization treatment on throughfall PO4 concentration (ppm)
##########################################################
data.nutrients %>%
  mutate(PO4.conc = PO4.conc/1000) %>%
  treatmentEffectPlot("PO4.conc") +
  labs(x = NULL, y = expression(PO[4]~concentration~(ppm))) +
  theme(legend.position = "none") +
  scale_y_break(c(2.2, 5), ticklabels = c(5, 5.5)) +
  scm.default.P +
  expand_limits(y = c(0, 5.5))
## ggsave("../Figures/PO4 treatment conc.png", width = 6, height = 5)

## Legend for FIGURE 1
data.nutrients %>%
  mutate(PO4.conc = PO4.conc/1000) %>%
  treatmentEffectPlot("PO4.conc") +
  theme(legend.position = "none") +
  scm.default.P +
  theme(legend.position = "right", legend.title = element_blank(), legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10))




########################################################## 
## FIGURE S1
## Effect of fertilization treatment on throughfall inorganic N and K concentration
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




########################################################## 
## FIGURE S3
## Throughfall concentrations of total P versus phosphate (PO4)
##########################################################  
data.nutrients %>% filter(!is.na(P_ppm)) %>%
  ggplot(aes(x = PO4.conc/1000, y = P_ppm)) + 
  geom_point(pch = 1) + 
  geom_abline(slope = 1, lty = "dashed") +
  labs(x = expression(PO[4]*phantom()^3~phantom()^-~phantom()~(ppm)), y = "Total P (ppm)") +
  theme_bw()
## ggsave("../Figures/P v PO4 concs.png", width = 4.5, height = 3)




########################################################## 
## FIGURE S4
## Effect of fertilization treatment on leaf area index
##########################################################  
data.LAI %>% filter(Treatment != "Rain") %>%
  rename(PAI.4.5 = LAI.4.5) %>%
  mutate(P.fert = ifelse(P.fert == T, "+ K", " - K")) %>%
  treatmentEffectPlot("PAI.4.5") +
  labs(y = expression(Leaf~area~index~(m^2~m^-2))) +
  theme(legend.position = c(0.08, 0.88), legend.title = element_blank(),
        legend.spacing.y = unit(0, "pt")) +
  scale_color_manual(labels = c(" - P", "+ P"), values = c("#FF6E3A", "#9F0162"))
## ggsave("../Figures/LAI x P fert by treat.png", width = 6, height = 4)




########################################################## 
## Calculations for main or supplemental text
########################################################## 

## Average concentrations in throughfall
## Inorganic P (PO4)
data.nutrients %>% group_by(P.fert) %>% 
  summarize(mean = mean(PO4.conc/1000), n = n(), se = sd(PO4.conc/1000)/sqrt(n()))
## Inorganic N (NO3 + NH4)
data.nutrients %>% filter(Treatment != "Rain") %>%
  summarize(mean = mean(N.conc/1000), n = n(), se = sd(N.conc/1000)/sqrt(n()))
## Potassium (K)
data.nutrients %>% filter(Treatment != "Rain", !is.na(K_ppm)) %>%
  summarize(mean = mean(K_ppm), n = n(), se = sd(K_ppm)/sqrt(n()))


## DRY DEPOSITION (using sodium as tracer ion)
## Assumptions: all Na from dry dep (none from leaching) and all elements have same wet:dry dep ratio
data.nutrients %>% group_by(Type) %>%
  filter(!is.na(Na_ppm)) %>%
  summarize(Na_conc = mean(Na_ppm), Na_dep = mean(Na.dep), n = n())
(11.3-9.3)/9.3 ## By concentration: DD = 21.5% of wet dep
(114-100)/100 ## By deposition: DD = 14% of wet dep


## Amount of rain per rain event
data %>% group_by(Collection) %>% filter(Type == "Rain") %>% 
  summarize(mean = 10*mean(Water_cm, na.rm = T), n = n())




##########################################################  
## Additional Figures (not in manuscript)
##########################################################  

## PO4 deposition (mg/m2/event)
data.nutrients %>%
  treatmentEffectPlot("PO4.dep") +
  labs(y = expression(PO[4]~deposition~(mg~m^-2~event^-1))) +
  theme(legend.position = "none") +
  scale_y_break(c(30, 80), ticklabels = c(80, 85)) +
  scm.default.P +
  expand_limits(y = c(0, 85))
## ggsave("../Figures/PO4 treatment dep_2.png", width = 6, height = 5)


## PO4 deposition (as % of rain deposition)
data.nutrients %>% filter(Type != "Rain") %>%
  treatmentEffectPlot("PO4.dep.pct.of.rain") +
  labs(y = "Throughfall P (% of rain)") +
  theme(legend.position = c(0.2, 0.85), legend.title = element_blank(), legend.spacing.y = unit(0, "pt")) +
  geom_hline(yintercept = 100, lty = "dashed") +
  scale_color_manual(labels = c(" - P", "+ P"), values = c("#008DF9", "#FF6E3A")) + scale_y_log10()
## ggsave("../Figures/PO4 treatment_pct of rain.png", width = 4, height = 3)


## Effect of leaf area index on throughfall volume
data.LAI %>% mutate(Rain = ifelse(P.fert == "Rain", "Rain", "Throughfall")) %>%
  filter(!is.na(Water_cm)) %>%
  ggplot(aes(x = LAI.4.5, y = 100*Water_cm/Rain_cm, color = Rain)) + geom_point(pch = 1) +
  labs(x = expression(Leaf~Area~Index~(m^2~m^-2)), y = "Water deposition (% of rain)") +
  geom_smooth(method = "lm", se = F, color = "black", formula = 'y~x') +
  theme_classic() + geom_abline(intercept = 100, slope = 0, lty = "dashed") +
  scale_color_manual(name = NULL, values = c("#008DF9", "#FF5AAF")) +
  theme(legend.position = c(0.85, 0.85))
## ggsave("../Figures/Water Dep.png", width = 5, height = 3)

