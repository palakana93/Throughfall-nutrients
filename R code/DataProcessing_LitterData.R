
## R script to process Gigante litterfall data

## Takes in raw data on litterfall mass and litterfall nutrient concentrations and returns
## a data frame with the plot-level average mass of litterfall and litterfall nutrients

rm(list = ls())
library(tidyverse); library(foreign)
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data/Litter data")

## Read in litter nutrient concentrations
litter.nutrients <- read.table(header = T, "Gigante 2012 Master Data File PLANTS unified codes for wood with final isotopes FERTILIZER litter.txt")
litter.nutrients <- litter.nutrients %>% 
  mutate(Plot = as.factor(Plot)) %>%
  as.tibble()

## Read in total litter biomass (code from Joe)
litter.mass <- read.dbf("GIG_FERTILIZAR_LITTER_20200602.dbf", as.is = F)
names(litter.mass) <- c("Date","Plot","Trap","Leaves","Reproductive","Branches","Dust","Total")
litter.mass$Total <- ifelse(litter.mass$Total==0 & litter.mass$Leaves>0, litter.mass$Leaves+litter.mass$Reproductive+litter.mass$Branches+litter.mass$Dust, litter.mass$Total) # catches 2 records
litter.mass <- litter.mass[!duplicated(litter.mass),] # remove double entry of 2010-08-26

# Assign missing values whenever Total==0 (code from Joe)
litter.mass$Total <- ifelse(litter.mass$Total==0, NA, litter.mass$Total)

# Sum monthly measurements to get total litter mass by plot and year from Nov through Oct. (code from Joe)
litter.mass <- tibble(litter.mass)
litter.mass.annual <- litter.mass %>%
  mutate(Date = as_date(Date, format="%Y-%m-%d")) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = ifelse(Month>=11, year(Date)+1, year(Date))) %>%
  filter(Year>1998 & Year<2019) %>%
  mutate(Total = Total*10/0.57) %>%      # convert to kg/ha
  select(Year,Month,Plot,Trap,Total) %>%
  group_by(Year, Month, Plot, Trap) %>%
  summarise(Total = sum(Total)) %>%       # sum within trap & month
  filter(!is.na(Total)) %>%
  group_by(Year, Month, Plot) %>%
  summarise(Total = mean(Total)) %>%      # average within plot & month
  group_by(Year, Plot) %>%
  summarise(Total = sum(Total)) %>%       # sum within plot & year
  ungroup() %>%
  mutate(Plot = as.factor(Plot))

## Add plot data (NPK fertilization)
setwd("/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data")
fert.treatment <- read.csv("GFX Combined Master Data.csv")
fert.treatment <- fert.treatment %>% select(Plot, Treatment, P.fert, N.fert, K.fert) %>% 
  distinct() %>%
  filter(!is.na(Plot), !(Plot %in% c("Rain 1", "Rain 2", "Rain 3"))) %>% 
  arrange(as.numeric(Plot)) %>% 
  as.tibble()

## Join litter mass with plot data (NPK fert)
litter.mass.annual <- fert.treatment %>% inner_join(litter.mass.annual, by = "Plot")


#### Get annual estimates of litterfall nutrient recycling
## Get mean litter nutrient concentrations by plot
litter.nutrient.plot.averages <- litter.nutrients %>% 
  mutate(Nutrient = sub(".*_", "", Parameter)) %>% 
  mutate(Value = ifelse(Nutrient == "N", Value/100, Value/1000)) %>%
  select(-c(Species, Parameter)) %>%
  filter(Nutrient %in% c("P", "N", "K", "Mg", "Ca", "Na")) %>%
  group_by(Plot, Nutrient) %>%
  summarize(mean = mean(Value)) %>%
  pivot_wider(names_from = Nutrient, values_from = mean)

## Get litterfall nutrient recycling by plot (litter mass * nutrient concentration)
## Omit litterfall mass data from first 5 years of experiment (when fertilization effect was likely weak)
litter.byPlot <- litter.mass.annual %>% 
  filter(Year > 2002) %>%  ## Omit litterfall mass data from first 5 years of experiment (1998-2002)
  group_by(Plot, Treatment, P.fert, N.fert, K.fert) %>%
  summarize(Mass = mean(Total)) %>% 
  left_join(litter.nutrient.plot.averages, by = "Plot") %>%
  mutate(across(.cols = c("P", "N", "K", "Mg", "Ca", "Na"), ~ . * Mass)) %>%
  ungroup()

## Write .CSV spreadsheet containing litter data (units: kg/ha/yr)
write.csv(litter.byPlot, "/Users/palaniakana/Desktop/Columbia/*Research/BCI/Data/Litter data/Litter merged_plot means.csv", row.names = F)
