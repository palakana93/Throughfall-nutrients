
## Annual budgets of throughfall nutrient deposition

rm(list = ls())
library(tidyverse); library(gdata); library(lubridate)
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")
source("../R scripts/ReUp fns.R")

## Read in data
load("GFXdata.RData") ## Load data frames (see 'ReUp Merge.R' for descriptions) 


##########################################################
## Tables: Means ± SE
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
  group_by(Collection) %>% summarize(mean = mean(Rain_mm))

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
  summarize(across(nuts, list(#mean = mean, SE = ~sd(.)/sqrt(n()), n = ~n(),
    stat = stat)))
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
  arrange(desc(fert)) %>%
  select(-fert)
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
  select(fert, contains("recyc")) %>% slice(-1)
#write.csv(recyc.fertAdd, "../Figures/*Tables/recyc_fertAdd.csv", row.names = F)



##### MAKE LITTERFALL TABLES ###########
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data/Litter data")
library(foreign)

## Part 1: Get litter nutrient data -----------------
## Get litter nutrient concentrations
litter.nutrients <- read.table(header = T, "Gigante 2012 Master Data File PLANTS unified codes for wood with final isotopes FERTILIZER litter.txt")
litter.nutrients <- litter.nutrients %>% mutate(Plot = as.factor(Plot))

## Get total litter biomass (code from Joe)
a = read.dbf("GIG_FERTILIZAR_LITTER_20200602.dbf", as.is=F)
names(a) = c("fecha","plot","trap","lvs","repr","branch","dust","total")
a$total = ifelse(a$total==0 & a$lvs>0, a$lvs+a$repr+a$branch+a$dust, a$total) # catches 2 records
a = a[!duplicated(a),] # remove double entry of 2010-08-26

# Assign missing values whenever total==0 (code from Joe)
a$total = ifelse(a$total==0, NA, a$total)

# Total litter mass by plot and year from Nov thru Oct. (code from Joe)
litter.mass <- tibble(a)
litter.mass <- litter.mass %>%
  mutate(date=as_date(fecha, format="%Y-%m-%d")) %>%
  mutate(mo = month(date)) %>%
  mutate(yr = ifelse(mo>=11, year(date)+1, year(date))) %>%
  filter(yr>1998 & yr<2019) %>%
  mutate(tot = total*10/0.57) %>%      # convert to kg/ha
  select(yr,mo,plot,trap,tot) %>%
  group_by(yr, mo, plot, trap) %>%
  summarise(tot = sum(tot)) %>%       # sum within trap & month
  filter(!is.na(tot)) %>%
  group_by(yr, mo, plot) %>%
  summarise(tot = mean(tot)) %>%      # average within plot & month
  group_by(yr, plot) %>%
  summarise(tot = sum(tot)) %>%       # sum within plot & year
  ungroup() %>%
  rename(Year = yr, Plot = plot, Total.mass = tot) %>%
  mutate(Plot = as.factor(Plot))

## Get plot data (NPK fert)
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")
source("../R scripts/ReUp fns.R")
data <- read.csv("GFX Combined Master Data.csv")
data <- data %>% select(Plot, Treatment, P.fert, N.fert, K.fert) %>% 
  distinct() %>%
  filter(!is.na(Plot), !(Plot %in% c("Rain 1", "Rain 2", "Rain 3"))) %>% 
  arrange(as.numeric(Plot))

## Join litter mass with plot data (NPK fert)
litter.mass <- data %>% inner_join(litter.mass, by = "Plot")

#### MARCH 2022: Get annual estimates of litterfall nutrient recycling
## Get mean litter nutrient concentrations by plot
litter.nut <- litter.nutrients %>% 
  mutate(Nutrient = sub(".*_", "", Parameter)) %>% 
  mutate(Value = ifelse(Nutrient == "N", Value/100, Value/1000)) %>%
  select(-c(Species, Parameter)) %>%
  filter(Nutrient %in% c("P", "N", "K", "Mg", "Ca", "Na")) %>%
  group_by(Plot, Nutrient) %>%
  summarize(mean = mean(Value)) %>%
  pivot_wider(names_from = Nutrient, values_from = mean)

## Get litterfall nutrient recycling by plot (litter mass * nutrient concentration)
litter.byPlot <- litter.mass %>% group_by(Plot, Treatment, P.fert, N.fert, K.fert) %>%
  summarize(Mass = mean(Total.mass)) %>% 
  left_join(litter.nut, by = "Plot") %>%
  mutate(across(.cols = c("P", "N", "K", "Mg", "Ca", "Na"), ~ . * Mass)) %>%
  ungroup()

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


##### MAKE FIGURE: ANNUAL NUTRIENT RECYCLING ###########
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
  labs(x = NULL, y = expression(Nutrient~recycling~(kg~ha^-1~y^-1))) +
  geom_text(data = dat_text, mapping = aes(x = -Inf, y = Inf, label = label), inherit.aes = F, hjust = -0.4, vjust = 1.2) +
  theme(axis.text = element_text(color = "black", size = 10), legend.title = element_blank(),
        strip.background = element_rect(fill = "white"))
#ggsave("../Figures/Litter v Tfall_3.png", width = 7, height = 3, dpi = 500)


## Figure 3: STATS (LMERs; also check with ANOVAs)
library(lme4); library(lmerTest)
## Litterfall stats
rep.block <- dataForStats %>% select(Plot, Replicate, Block) %>% distinct() %>% tibble()
litter.byPlot2 <- litter.byPlot %>% left_join(rep.block, by = "Plot")
litter.P.stat <- lmer(data = litter.byPlot2, P ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
litter.N.stat <- lmer(data = litter.byPlot2, N ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
litter.K.stat <- lmer(data = litter.byPlot2, K ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
## Significance for P < 0.01 (< 0.05 or < 0.1 shown in parentheses)
summary(litter.P.stat) ## +P (-N*P at 0.1)
summary(litter.N.stat) ## N.S.
summary(litter.K.stat) ## N.S.
## Model residuals: See https://cran.r-project.org/web/packages/ggResidpanel/vignettes/introduction.html
library(ggpubr); library(gridExtra); library(ggResidpanel)
lit.P <- resid_panel(litter.P.stat, plots = c("resid", "qq", "hist"), nrow = 1)
lit.N <- resid_panel(litter.N.stat, plots = c("resid", "qq", "hist"), nrow = 1)
lit.K <- resid_panel(litter.K.stat, plots = c("resid", "qq", "hist"), nrow = 1)
lit.all <- grid.arrange(lit.P, lit.N, lit.K, nrow = 3)
#ggsave("/Users/palaniakana/Desktop/Litter Resid Plots.png", lit.all, width = 6, height = 6)

## Stats for litterfall: broken into "mass" and "concentration"
## Probably need to log-transform response vars (residuals need to be checked)
litter.mass <- litter.mass %>% left_join(rep.block, by = "Plot")
litter.nut.2 <- litter.nutrients %>% 
  mutate(Nutrient = sub(".*_", "", Parameter)) %>% 
  mutate(Value = ifelse(Nutrient == "N", Value/100, Value/1000)) %>%
  select(-c(Species, Parameter)) %>%
  filter(Nutrient %in% c("P", "N", "K")) %>%
  pivot_wider(names_from = Nutrient, values_from = Value) %>%
  left_join(distinct(select(litter.mass, Plot, P.fert, N.fert, K.fert)), by = "Plot") %>%
  filter(!is.na(P.fert)) %>%
  left_join(rep.block, by = "Plot")
mass.stat <- lmer(data = filter(litter.mass, Year > 2002), Total.mass ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block) + (1|Year))
P.stat <- lmer(data = litter.nut.2, P ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block) + (1|Month))
N.stat <- lmer(data = litter.nut.2, N ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block) + (1|Month))
K.stat <- lmer(data = litter.nut.2, K ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block) + (1|Month))
## Significance for P < 0.01 (< 0.05 or < 0.1 shown in parentheses)
summary(mass.stat) ## +N, +P, -K (-NK at 0.05, +PK at 0.1)
summary(P.stat) ## +P, -NP (+K at 0.1)
summary(N.stat) ## N.S. (+N, +K, -PK at 0.05, +P at 0.1)
summary(K.stat) ## +K (-PK at 0.05)
## Residual plots; mass.stat and N.stat looks good
## P not good but log-transform doesn't fix it
## Log-transform makes K look better, but significant variables don't change
resid_panel(mass.stat, plots = c("resid", "qq", "hist"), nrow = 1)
resid_panel(P.stat, plots = c("resid", "qq", "hist"), nrow = 1)
resid_panel(N.stat, plots = c("resid", "qq", "hist"), nrow = 1)
resid_panel(K.stat, plots = c("resid", "qq", "hist"), nrow = 1)
## Percent changes for P litter
litter.mass %>% group_by(P.fert) %>% summarize(mass = mean(Total.mass)) %>% mutate(mass = 100*mass/first(mass))
litter.nut.2 %>% group_by(P.fert) %>% summarize(P.conc = mean(P)) %>% mutate(P.conc = 100*P.conc/first(P.conc))

## Throughfall stats
dep.byPlot2 <- dep.byPlot %>% left_join(rep.block, by = "Plot")
tfall.P.stat <- lmer(data = dep.byPlot2, PO4 ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
tfall.N.stat <- lmer(data = dep.byPlot2, N ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
tfall.K.stat <- lmer(data = dep.byPlot2, K ~ N.fert*P.fert + P.fert*K.fert + N.fert*K.fert + (1|Replicate/Block))
## Significance for P < 0.01
summary(tfall.P.stat) ## +P (—N*P at 0.05; —P*K at 0.1)
summary(tfall.N.stat) ## N.S. (+N*P at 0.1)
summary(tfall.K.stat) ## N.S. (+N*P at 0.05)
## Residual plots: P and K not great, but log-transform doesn't fix them
tf.P <- resid_panel(tfall.P.stat, plots = c("resid", "qq", "hist"), nrow = 1)
tf.N <- resid_panel(tfall.N.stat, plots = c("resid", "qq", "hist"), nrow = 1)
tf.K <- resid_panel(tfall.K.stat, plots = c("resid", "qq", "hist"), nrow = 1)
tf.all <- grid.arrange(tf.P, tf.N, tf.K, nrow = 3)
#ggsave("/Users/palaniakana/Desktop/Tfall Resid Plots.png", tf.all, width = 6, height = 6)




##########################################################
## Annual recycling (Old code)
##  <Not summed by plot before calculating mean; no SE>
##########################################################

# #### Format data in prep for summarizing annual budgets ####
# ## Average rainfall per rain event (mm) for each nutrient
# rain_mm <- tibble(Nutrient = c("PO4", "NH4", "NO3", "N", "K", "P", "Mg", "Ca", "Na"),
#                   Rain_mm = c(83.6/4, 83.6/4, 83.6/4, 83.6/4, 31.5/3, 10.9, 10.9, 10.9, 10.9))
# ## Convert nutrient data to long format (column for "nutrient")
# data.long <- data.nutrients %>% select(ID, Type, grep("dep", colnames(data.nutrients)),
#                                        grep("fert", colnames(data.nutrients)),
#                                        -grep("rain", colnames(data.nutrients))) %>%
#   pivot_longer(P.dep:N.dep, names_to = "Nutrient", values_to = "dep")
# 
# ## Estimate annual nutrient dep in rain and tfall (not separated by fert treatment)
# all <- data.long %>% group_by(Nutrient, Type) %>%
#   summarize(dep = mean(dep, na.rm = T)) %>%
#   pivot_wider(id_cols = Nutrient, names_from = Type, values_from = dep) %>%
#   mutate(Nutrient = substr(Nutrient, 0, nchar(Nutrient) - 4)) %>%
#   left_join(rain_mm, by = "Nutrient") %>%
#   mutate(Rain_dep = Rain*(2623/Rain_mm)/100,                   ## kg/ha/y
#          Tfall_dep = Tfall*(2623/Rain_mm)/100,                 ## kg/ha/y
#          Tfall_recyc = (Tfall - Rain)*(2623/Rain_mm)/100) %>%  ## kg/ha/y
#   #filter(!(Nutrient %in% c("NH4", "PO4", "Ca", "P"))) %>%
#   select(-c(Rain, Tfall, Rain_mm))
# 
# ## Estimate annual nutrient dep in tfall, separated by P fert
# p.fert <- data.long %>% group_by(Nutrient, P.fert) %>%
#   summarize(dep = mean(dep, na.rm = T)) %>%
#   pivot_wider(id_cols = Nutrient, names_from = P.fert, values_from = dep) %>%
#   rename(Plus.P = `TRUE`, Minus.P = `FALSE`) %>%
#   mutate(Nutrient = substr(Nutrient, 0, nchar(Nutrient) - 4)) %>%
#   left_join(rain_mm, by = "Nutrient") %>%
#   mutate(Dep_no_P = Minus.P*(2623/Rain_mm)/100,              ## kg/ha/y
#          Dep_P = Plus.P*(2623/Rain_mm)/100,                  ## kg/ha/y
#          Recyc_no_P = (Minus.P - Rain)*(2623/Rain_mm)/100,   ## kg/ha/y
#          Recyc_P = (Plus.P - Rain)*(2623/Rain_mm)/100,) %>%  ## kg/ha/y
#   filter(Nutrient %in% c("PO4", "P")) %>%
#   select(-c(Rain, Minus.P, Plus.P, Rain_mm)) %>%
#   mutate(Dep_ratio = 100*((Dep_P / Dep_no_P) - 1),
#          Recyc_ratio = 100*((Recyc_P / Recyc_no_P) - 1))
# 
# ## Estimate annual nutrient dep in tfall, separated by N fert
# n.fert <- data.long %>% group_by(Nutrient, N.fert) %>%
#   summarize(dep = mean(dep, na.rm = T)) %>%
#   pivot_wider(id_cols = Nutrient, names_from = N.fert, values_from = dep) %>%
#   rename(Plus.N = `TRUE`, Minus.N = `FALSE`) %>%
#   mutate(Nutrient = substr(Nutrient, 0, nchar(Nutrient) - 4)) %>%
#   left_join(rain_mm, by = "Nutrient") %>%
#   mutate(Dep_no_N = Minus.N*(2623/Rain_mm)/100,              ## kg/ha/y
#          Dep_N = Plus.N*(2623/Rain_mm)/100,                  ## kg/ha/y
#          Recyc_no_N = (Minus.N - Rain)*(2623/Rain_mm)/100,   ## kg/ha/y
#          Recyc_N = (Plus.N - Rain)*(2623/Rain_mm)/100,) %>%  ## kg/ha/y
#   filter(Nutrient %in% c("NH4", "Ca")) %>%
#   select(-c(Rain, Minus.N, Plus.N, Rain_mm)) %>%
#   mutate(Dep_ratio = 100*((Dep_N / Dep_no_N) - 1),
#          Recyc_ratio = 100*((Recyc_N / Recyc_no_N) - 1))
# 
# 
# fert <- tibble(fert = c("Rain", "+N", "-N", "Rain", "+P", "-P", "Rain", "+K", "-K"))
# #all %>% filter(Nutrient %in% c("PO4", "N", "K"))
# inputs <- p.fert %>% filter(Nutrient == "PO4") %>% 
#   select(Nutrient, Recyc_fert = Recyc_P, Recyc_unfert = Recyc_no_P) %>%
#   right_join(filter(all, Nutrient %in% c("PO4", "N", "K")), by = "Nutrient") %>%
#   mutate(Nutrient = ifelse(Nutrient == "PO4", "P", Nutrient),
#          Tfall_fert = ifelse(is.na(Recyc_fert), Tfall_recyc, Recyc_fert),
#          Tfall_unfert = ifelse(is.na(Recyc_unfert), Tfall_recyc, Recyc_unfert)) %>%
#   select(Nutrient, Rain = Rain_dep, Tfall_fert, Tfall_unfert) %>%
#   mutate()
#   pivot_longer(Rain:Tfall_unfert, names_to = "type", values_to = "input") %>%
#   cbind(fert)
# write.csv(inputs, "/Users/palaniakana/Desktop/inputs.csv", row.names = F)
# 
# rain <- all %>% filter(Nutrient %in% c("PO4", "N", "K")) %>% 
#   select(Nutrient, Input = Rain_dep) %>%
#   mutate(Fert = "Rain", Type = "Rain", 
#          Nutrient = ifelse(Nutrient == "PO4", "P", Nutrient))
# tfall.P <- p.fert %>% filter(Nutrient == "PO4") %>% 
#   select(Nutrient, `+P` = Recyc_P, `-P` = Recyc_no_P) %>%
#   pivot_longer(-Nutrient, names_to = "Fert", values_to = "Input") %>%
#   mutate(Nutrient = "P", Type = "Throughfall")
# inputs <- all %>% filter(Nutrient %in% c("N", "K")) %>% select(Input = Tfall_recyc) %>% 
#   inner_join(tibble(Nutrient = c("K", "K", "N", "N"), Fert = c("-K", "+K", "-N", "+N")), by = "Nutrient") %>%
#   mutate(Type = "Throughfall") %>%
#   rbind(tfall.P, rain)
# write.csv(inputs, "/Users/palaniakana/Desktop/inputs.csv", row.names = F)




##########################################################
## From here on, old code (ignore)
##########################################################
data %>% filter(!grepl("MIC", ID), Type %in% c("Rain", "Tfall")) %>%
  group_by(P.fert) %>% 
  summarize(P.dep = mean(P.dep, na.rm = T),
            PO4.dep = mean(PO4.dep, na.rm = T))
## Old numbers (not sure why different; slightly changed funnel area?)
(5.97-0.464)*(2623/(87.5/4))/100 ## kg P/m2/yr (+ P)
(1.27-0.464)*(2623/(87.5/4))/100 ## kg P/m2/yr (- P)
## August 2021 numbers
(5.69-0.439)*(2623/(83.6/4))/100 ## kg P/m2/yr (+ P)
(1.23-0.439)*(2623/(83.6/4))/100 ## kg P/m2/yr (- P)
## August 2021 without rainfall subtracted
(5.69)*(2623/(83.6/4))/100 ## kg P/m2/yr (+ P)
(1.23)*(2623/(83.6/4))/100 ## kg P/m2/yr (- P)
(0.439)*(2623/(83.6/4))/100 ## Rain

data %>% filter(!grepl("MIC", ID), Type %in% c("Rain", "Tfall")) %>%
  group_by(N.fert) %>% 
  summarize(NH4.dep = mean(NH4.dep, na.rm = T),
            NO3.dep = mean(NO3.dep, na.rm = T)) %>%
  mutate(N.dep = NH4.dep + NO3.dep)
## Old numbers (not sure why different)
(4.51-4.07)*(2623/(87.5/4))/100 ## kg N/m2/yr (+ N)
(4.89-4.07)*(2623/(87.5/4))/100 ## kg N/m2/yr (- N)
## August 2021 numbers
(4.33-3.87)*(2623/(83.6/4))/100 ## kg N/m2/yr (+ N)
(4.68-3.87)*(2623/(83.6/4))/100 ## kg N/m2/yr (- N)
## August 2021 without rainfall subtracted
(4.33)*(2623/(83.6/4))/100 ## kg N/m2/yr (+ N)
(4.68)*(2623/(83.6/4))/100 ## kg N/m2/yr (- N)
(3.87)*(2623/(83.6/4))/100 ## Rain

## Mean concentrations (ppm)
## WHICH DATA FRAME??
data %>% filter(!grepl("MIC", ID), Type %in% c("Rain", "Tfall")) %>%
  group_by(Type) %>% 
  summarize(N.conc = mean(N.conc/1000, na.rm = T),
            #NH4.conc = mean(NH4.conc/1000, na.rm = T),
            NO3.conc = mean(NO3.conc/1000, na.rm = T),
            #P.conc = mean(P.conc/1000, na.rm = T),
            K.conc = mean(K_ppm, na.rm = T),
            Mg.conc = mean(Mg_ppm, na.rm = T),
            #Ca.conc = mean(Ca_ppm, na.rm = T),
            Na.conc = mean(Na_ppm, na.rm = T))

## Annual deposition: rain and throughfall (all fert treatments combined)
## GET MEAN EVENT RAINFALL BY NUTRIENT (initialize df, then join() with dep)
dep <- data %>% filter(!grepl("MIC", ID), Type %in% c("Rain", "Tfall")) %>%
  group_by(Type) %>% 
  summarize(N.dep = mean(N.dep, na.rm = T),
            #NH4.dep = mean(NH4.dep, na.rm = T),
            NO3.dep = mean(NO3.dep, na.rm = T),
            #P.dep = mean(P.dep, na.rm = T),
            K.dep = mean(K.dep, na.rm = T),
            Mg.dep = mean(Mg.dep, na.rm = T),
            #Ca.dep = mean(Ca.dep, na.rm = T),
            Na.dep = mean(Na.dep, na.rm = T))
dep <- dep %>% select(-Type) %>% t() %>% data.frame()
colnames(dep) <- c("Rain", "Tfall")
dep <- dep %>% mutate(nutrient = rownames(dep)) %>%
  mutate(nutrient = substr(nutrient, 0, nchar(nutrient) - 4)) %>%
  select(nutrient, Tfall_dep = Tfall, Rain_dep = Rain) %>%
  cbind(Mean_event_rainfall_mm = c(83.6/4, 31.5/3, 10.9, 10.9, 10.9)) %>%
  mutate(Rain_dep_annual = Rain_dep*(2623/Mean_event_rainfall_mm)/100,
         Tfall_dep_annual = Tfall_dep*(2623/Mean_event_rainfall_mm)/100,
         Tfall_recyc_annual = (Tfall_dep - Rain_dep)*(2623/Mean_event_rainfall_mm)/100)
dep %>% select(nutrient, grep("annual", colnames(dep)))


##########################################################
## How much does throughfall contribute to nutrient cycling?
##########################################################
data %>% group_by(Collection, P.fert) %>% filter(!is.na(PO4.dep)) %>%
  summarize(meanPO4 = mean(PO4.conc, na.rm = T)) %>% pivot_wider(names_from = P.fert, values_from = meanPO4) %>%
  mutate(tfall.leach = (`FALSE` - Rain)/30, 
         tfall.fert.leach = (`TRUE` - Rain)/30) ## Estimate: in kg/ha (3 kg/ha unfertilized; 20 kg/ha fertilized)
## P from litterfall (Kaspari et al 2008) = 10,000 kg litterfall/ha * 6e-4 = 6 kg/ha/yr (throughfall recycling about half of litterfall in unfertilized plots; thrice litterfall in fertilized plots!)
## N from litterfall (Kaspari et al 2008) = 10,000 kg litterfall/ha * 1.5e-2 = 150 kg/ha/yr (throughfall not important)
## N:P ratio = 25:1

## How much rain per rain event?
filter(data, Treatment == "Rain") %>% group_by(Treatment, Collection) %>% 
  summarize(Water_mm = 10*mean(Water_cm))

## How much total P deposition
filter(data, DI.control == FALSE) %>% filter(!is.na(PO4.dep)) %>%
  filter(ID != "4-93") %>% ## Remove rain outlier
  group_by(P.fert, Collection) %>% 
  summarize(Water_mm = 10*mean(Water_cm, na.rm = T), PO4.dep = mean(PO4.dep, na.rm = T)) %>% 
  summarize(Total.precip = sum(Water_mm), Total.PO4.dep = sum(PO4.dep))
PO4.total.dep.FERT <- (30.1*2623/87.5/1000)*10
PO4.total.dep.noFERT <- (6.42*2623/87.5/1000)*10
PO4.total.dep.rain <- (2.07*2623/87.5/1000)*10
PO4.total.enrich.FERT <- PO4.total.dep.FERT - PO4.total.dep.rain
PO4.total.enrich.noFERT <- PO4.total.dep.noFERT - PO4.total.dep.rain
PO4.total.enrich.FERT; PO4.total.enrich.noFERT ## in kg/ha/yr
PO4.total.enrich.FERT/PO4.total.enrich.noFERT ## +P leaching enrichment factor
PO4.total.dep.FERT/PO4.total.dep.noFERT ## +P total dep enrichment factor is greater than the 3.5-ish LMER estimate (note that model error is log-normally distributed)




##########################################################
## Some PO4 in rain/DI field controls; increasing over time
## But this could be driven by natural processes; a bit more PO4 in rain, DI field controls reflecting previous high concentrations
## Decision: don't adjust PO4 estimates based on DI field or rain values
##   So don't use the script below to adjust PO4 concentrations
##########################################################

## Examine DI controls as function of concentration in previous collection
DI.bottles <- filter(data, DI.control == TRUE, Collection %in% c(2,3,4)) %>% select(ID, Collection, Bottle) %>%
  mutate(prev_Collection = as.numeric(Collection) - 1, prevID = paste(prev_Collection, Bottle, sep = "-")) %>%
  select(ID, prevID)
prev <- data %>% filter(ID %in% DI.bottles$prevID) %>% select(prevID = ID, PO4.prev = PO4.conc, NO3.prev = NO3.conc, NH4.prev = NH4.conc)
DI.bottles <- DI.bottles %>% left_join(prev, by = "prevID") %>% left_join(select(data, ID, Collection, PO4.conc, NO3.conc, NH4.conc), by = "ID")
DI.bottles %>% ggplot(aes(x = PO4.conc, y = PO4.prev, shape = Collection)) + geom_point() + theme_bw()
DI.bottles %>% ggplot(aes(x = NO3.conc, y = NO3.prev, shape = Collection)) + geom_point() + theme_bw()
DI.bottles %>% ggplot(aes(x = NH4.conc, y = NH4.prev, shape = Collection)) + geom_point() + theme_bw()
## PO4 shows definite correlation; NO3 less so, and NH4 all less than 20 µg/L anyway


## How much PO4 in DI controls?
filter(data, DI.control == TRUE | Treatment == "Rain") %>% 
  filter(ID != "4-93") %>% ## Remove rain outlier
  group_by(Collection, DI.control) %>% 
  summarize(n = n(), PO4.conc = mean(PO4.conc, na.rm = T),
            NO3.conc = mean(NO3.conc, na.rm = T),
            NH4.conc = mean(NH4.conc, na.rm = T))
## In other words, PO4 in rain is below detection limit


## Re-adjusting PO4
PO4.conc.adj <- filter(data, DI.control == TRUE) %>% 
  group_by(Collection) %>% summarize(PO4.conc.adjustment = mean(PO4.conc, na.rm = T))
dataForStats.adj <- dataForStats %>% full_join(PO4.conc.adj, by = "Collection") %>%
  mutate(PO4.conc.adj = PO4.conc - PO4.conc.adjustment) %>%
  mutate(PO4.dep.adj = (Volume_mL/100)*PO4.conc.adj/Funnel_area)
dataForStats.adj %>% select(ID, Collection, PO4.conc, PO4.conc.adjustment, PO4.conc.adj, PO4.dep, PO4.dep.adj) %>% tail()
dataForStats.adj %>% select(ID, Collection, PO4.conc, PO4.conc.adjustment, PO4.conc.adj, LAI.4.5) %>% filter(PO4.conc.adj < 1)
dataForStats.adj <- filter(dataForStats.adj, PO4.conc.adj > 1) ## For now; fix later
## LMER (use this one!)
lme.PO4.adj <- lmer(data = dataForStats.adj, log(PO4.dep.adj) ~ N*P + P*K + N*K + LAI.4.5 + (1|Replicate/Block/Plot) + (1|Collection))
summary(lme.PO4.adj)
## How much PO4 dep
filter(dataForStats.adj, DI.control == FALSE) %>% 
  group_by(P, Collection) %>% 
  summarize(Water_mm = 10*mean(Water_cm, na.rm = T), PO4.dep.adj = mean(PO4.dep.adj, na.rm = T)) %>% 
  summarize(Total.precip = sum(Water_mm), Precip.sd = sd(Water_mm), Total.PO4.dep = sum(PO4.dep.adj))
PO4.total.dep.FERT <- (28.2*2623/87.5/1000)*10 ## 87.5 is total rainwater in collectors
PO4.total.dep.noFERT <- (4.57*2623/87.5/1000)*10
PO4.total.dep.FERT; PO4.total.dep.noFERT
