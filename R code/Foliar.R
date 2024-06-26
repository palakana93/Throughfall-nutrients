
## Foliar data: nutrient concentrations of understory leaves from 3 common canopy species

## Read in data
rm(list = ls())
library(tidyverse)
setwd('/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data/Foliar data')
dat <- read.table("Fresh Lvs Values Sp Tmt.txt", header = T)

## Identify outliers and clean data
clean.dat <- dat %>% filter(micro == 0) %>% 
  pivot_longer(cols = c(P.conc, K.conc, N.conc), names_to = "nut", values_to = "conc") %>%
  group_by(sp, nut) %>% 
  mutate(nut = gsub(".conc", "", nut)) %>%
  mutate(med = median(conc)) %>%
  mutate(MAD = median(abs(conc-med))) %>%
  mutate(mod.z = 0.6745*(conc-med)/(MAD)) %>%
  select(plot, repl, block, tmt, sp, N, P, K, nut, conc, med, MAD, mod.z) %>%
  filter(abs(mod.z) <= 3.5) %>%
  mutate(SND = scale(conc))
clean.snd <- clean.dat %>%
  group_by(plot, repl, block, N, P, K, nut) %>%
  summarize(SND.plot = mean(SND))

## STATS
P.aov <- aov(data = filter(clean.snd, nut == "P"), SND.plot ~ N*P + N*K + P*K + Error(repl/block))
K.aov <- aov(data = filter(clean.snd, nut == "K"), SND.plot ~ N*P + N*K + P*K + Error(repl/block))
N.aov <- aov(data = filter(clean.snd, nut == "N"), SND.plot ~ N*P + N*K + P*K + Error(repl/block))

summary(P.aov) ## P (K) [N:P]
summary(K.aov) ## (K)
summary(N.aov) ## N


## Mean +/- SD and % changes by focal fertilizer addition
## P: 35-83% increase; N: 3-8% increase; K: 5-26% increase
mean.SE <- clean.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  mutate(fert = ifelse(nut == "P", P, K),
         fert = ifelse(nut == "N", N, fert)) %>%
  group_by(sp, nut, fert) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc)) %>%
  arrange(desc(nut), sp, fert) %>%
  mutate(pctChng = mean.conc/first(mean.conc)) %>%
  mutate(dig = nchar(gsub(".*\\.", "", formatC(mean.conc, digits = 3, flag = "#")))) %>%
  mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
                      formatC(sd.conc/4, digits = 3, format = "f"))) %>%
  #mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
  #                    formatC(sd.conc/4, digits = dig, format = "f"))) %>%
  select(sp, nut, fert, stat, pctChng)

## For Table 1
mean.SE.table <- mean.SE %>% select(-pctChng) %>%
  pivot_wider(names_from = "nut", values_from = "stat") %>%
  arrange(sp, desc(fert))
#write.csv(mean.SE.table, file = "/Users/palaniakana/Desktop/table.csv", row.names = F)


## Mean +/- SE by treatment
## For Table S2
mean.SE.tmt <- clean.dat %>%
  mutate(conc = ifelse(nut == "N", conc*10, conc)) %>%
  group_by(sp, nut, tmt) %>%
  summarize(mean.conc = mean(conc), sd.conc = sd(conc)) %>%
  arrange(desc(nut), sp, tmt) %>%
  mutate(dig = nchar(gsub(".*\\.", "", formatC(mean.conc, digits = 3, flag = "#")))) %>%
  mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
                      formatC(sd.conc/2, digits = 3, format = "f"))) %>%
  #mutate(stat = paste(formatC(mean.conc, digits = 3, flag = "#"), "±",
  #                    formatC(sd.conc/2, digits = dig, format = "f"))) %>%
  select(sp, nut, tmt, stat)

mean.SE.tmt.table <- mean.SE.tmt %>%
  pivot_wider(names_from = "nut", values_from = "stat") %>%
  mutate(tmt = factor(tmt, levels = c("CTL", "K", "N", "NK", "P", "PK", "NP", "NPK"))) %>%
  arrange(sp, tmt)
#write.csv(mean.SE.tmt.table, file = "/Users/palaniakana/Desktop/table.tmt.csv", row.names = F)


