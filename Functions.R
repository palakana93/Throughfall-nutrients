
########################################################
########################################################
## Functions for GFX throughfall data visualization


## Merging and manipulating data frames
########################################################
########################################################

## Merge and return collector data with LAI data
getCollectorData <- function() {
  
  ## Read in collector data
  collectorData <- read.xls("Collector Data.xlsx", sheet = 1)
  collectorData <- collectorData %>% 
    mutate(ID.full = ID, ID = paste(Collection, Bottle, sep = "-"),
           Bottle = as.character(Bottle))
  
  ## Merge canopy photos gap fraction/LAI (zenith angles of 4.5, 9, and 2.25) with collector data
  picNums <- as.numeric(substring(list.files("Canopy Photos/Canon/dual"), 5, 8))
  gfLAI <- read.csv("Canopy Photos/gfLAI4.5.csv", header = F)
  gfLAI <- cbind(picNums, gfLAI)
  colnames(gfLAI) = c("Photo_number", "Gap_Fraction.4.5", "LAI.4.5")
  gfLAI.2.25 <- read.csv("Canopy Photos/gfLAI2.25.csv", header = F)
  gfLAI.2.25 <- cbind(picNums, gfLAI.2.25)
  colnames(gfLAI.2.25) = c("Photo_number", "Gap_Fraction.2.25", "LAI.2.25")
  gfLAI.9 <- read.csv("Canopy Photos/gfLAI9.csv", header = F)
  gfLAI.9 <- cbind(picNums, gfLAI.9)
  colnames(gfLAI.9) = c("Photo_number", "Gap_Fraction.9", "LAI.9")
  collectorData <- collectorData %>% left_join(gfLAI, by = "Photo_number") %>% 
    left_join(gfLAI.2.25, by = "Photo_number") %>%
    left_join(gfLAI.9, by = "Photo_number")
  
  collectorData
  
}

## Make some columns factors
makeFactors <- function(df) {
  df %>% mutate(Collection = factor(Collection),
                P.fert = factor(P.fert, levels = c("Rain", FALSE, TRUE)),
                N.fert = factor(N.fert, levels = c("Rain", FALSE, TRUE)),
                K.fert = factor(K.fert, levels = c("Rain", FALSE, TRUE)),
                Type = factor(Type, c("DI_lab", "DI", "Rain", "Tfall")),
                Treatment = factor(Treatment, levels = c("Rain", "C", "K", "N", "NK", "P", "PK", "NP", "NPK"),
                                   labels = c("Rain", "Control", "+K", "+N", "+NK", "+P", "+PK", "+NP", "+NPK")))
}

## Get Block (north/south) and Replicate (1, 2, 3, 4)
getBlockRepl <- function(df) {
  df %>% mutate(Block = ifelse(Plot %in% c(1,2,4,5,9,12,13,15,19,20,23,24,28,29,30,32), 1, 2)) %>%
    mutate(Replicate = ifelse(Plot %in% c(1:7,10), 1, 0)) %>%
    mutate(Replicate = ifelse(Plot %in% c(9,11:15,17,18), 2, Replicate)) %>%
    mutate(Replicate = ifelse(Plot %in% c(19:24,26,27), 3, Replicate)) %>%
    mutate(Replicate = ifelse(Plot %in% c(28:30,32:36), 4, Replicate))
}

## Get parameters for lmer regression lines
########################################################
########################################################

## Extract parameters from lmer model output; old
parms.LAIxNut <- function(model, interaction = TRUE) {
  int <- fixef(model)[1][[1]]
  LAI <- fixef(model)[2][[1]]
  if(interaction == TRUE) LAIxNut <- fixef(model)[3][[1]]
  else LAIxNut <- 0
  data.frame(int, LAI, LAIxNut)
}

## Extract parameters from lmer model output; return as data frame
## Coefficients are intercept, LAI effect, and LAI*fertilizer effect
parms.LAIxNut2 <- function(model, interaction = 0) {
  int <- fixef(model)[1][[1]]
  LAI <- fixef(model)[2][[1]]
  if(interaction != 0) LAIxNut <- fixef(model)[interaction][[1]]
  else LAIxNut <- 0
  data.frame(int, LAI, LAIxNut)
}

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

## Figures / plots
########################################################
########################################################

## Fertilizer boxplots by treatment
treatmentEffectPlot <- function(data, nutrient) {
  data %>% filter(DI.control == FALSE) %>% # filter(Funnel_area < 100) %>%
    ggplot(aes_string(x = "Treatment", y = nutrient, color = paste(substr(nutrient, 0, 1), "fert", sep = "."))) + 
    geom_boxplot(outlier.shape = NA) + theme_bw() + 
    geom_point(position = position_jitter(width = 0.2), pch = 1, show.legend = F) +
    labs(x = "Fertilizer Added", y = paste(substr(nutrient, 0, 3), " deposition (mg/m2)", sep = ""), 
         color = paste(substr(nutrient, 0, 1), "fertilized")) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10))
}

## LAI scatterplot
LAIEffectPlot <- function(data, nutrient) {
  data %>% filter(DI.control == FALSE, LAI.4.5 < 20) %>% ## Omits one datum with LAI ~25
    # filter(Funnel_area < 100) %>%
    ggplot(aes_string(x = "LAI.4.5", y = nutrient, color = paste(substr(nutrient, 0, 1), "fert", sep = "."))) + 
    geom_point() + theme_bw() + 
    labs(x = "Leaf Area Index", y = paste(substr(nutrient, 0, 3), " deposition (mg/m2)", sep = ""), 
         color = paste(substr(nutrient, 0, 1), "fertilized"))
}

## Fertilizer boxplots by treatment (no color)
treatmentEffectPlot_noColor <- function(data, nutrient) {
  data %>% filter(DI.control == FALSE) %>% # filter(Funnel_area < 100) %>%
    ggplot(aes_string(x = "Treatment", y = nutrient)) + 
    geom_boxplot() + theme_bw() + 
    geom_point(position = position_dodge(width=0.75)) +
    labs(x = "", y = paste(substr(nutrient, 0, 2), " deposition (g/m2)", sep = "")) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))
}

## LAI scatterplot (no color)
LAIEffectPlot_noColor <- function(data, nutrient) {
  data %>% filter(DI.control == FALSE, LAI.4.5 < 20) %>% ## Omits one datum with LAI ~25
    # filter(Funnel_area < 100) %>%
    mutate(Rain = (Treatment == "Rain")) %>%
    ggplot(aes_string(x = "LAI.4.5", y = nutrient)) + 
    geom_point() + theme_bw() + 
    geom_smooth(method = "lm", se = F) +
    labs(x = "Leaf Area Index", y = paste(substr(nutrient, 0, 2), " deposition (g/m2)", sep = ""), 
         color = "Rain")
}

## Fertilizer boxplots
fertEffectPlot <- function(nutrient) {
  data  %>% filter(DI.control == FALSE) %>% # filter(Funnel_area < 100) %>%
    ggplot(aes_string(x = "Collection", y = nutrient, color = paste(substr(nutrient, 0, 1), "fert", sep = "."))) + 
    geom_boxplot() + theme_bw() + geom_point(position = position_dodge(width=0.75)) +
    labs(x = "Rain Event", y = paste("ln(", substr(nutrient, 0, 3), " [µg/L])", sep = ""), 
         color = paste(substr(nutrient, 0, 1), "fertilized"))
}

## Fertilizer boxplots (grouped by N + P fertilizer only)
npEffectPlot <- function(df, nutrient) {
  nut <- substring(nutrient, 1, 1)
  np.or.pn.combo <- if(nut == "P") "PNcombo" else "NPcombo"
  
  df %>% 
    ## For P plot
    mutate(PNcombo = paste(N.fert, P.fert, sep = "_")) %>%
    mutate(P.fert = ordered(P.fert, levels = c(FALSE, TRUE, "Rain"))) %>%
    mutate(PNcombo = recode(PNcombo, FALSE_FALSE = "-P, -N", FALSE_TRUE = "+P, -N", TRUE_FALSE = "-P, +N", TRUE_TRUE = "+P, +N", Rain_Rain = "Rain")) %>%
    
    ## For N plot
    mutate(NPcombo = paste(N.fert, P.fert, sep = "_")) %>%
    mutate(N.fert = ordered(N.fert, levels = c(FALSE, TRUE, "Rain"))) %>%
    mutate(NPcombo = recode(NPcombo, FALSE_FALSE = "-N, -P", FALSE_TRUE = "-N, +P", TRUE_FALSE = "+N, -P", TRUE_TRUE = "+N, +P", Rain_Rain = "Rain")) %>%
    
    ## For P and N plots
    mutate(P.fert = recode(P.fert, `FALSE` = " - P", `TRUE` = "+ P", Rain = "Rain")) %>%
    mutate(N.fert = recode(N.fert, `FALSE` = " - N", `TRUE` = "+ N", Rain = "Rain")) %>%
    filter(DI.control == FALSE) %>%
    
    ## Create plot
    ggplot(aes_string(x = np.or.pn.combo, y = nutrient, color = paste(nut, ".fert", sep = ""))) +
      geom_boxplot() + theme_bw() + geom_point(position = position_dodge(width=0.8)) +
      labs(x = NULL, y = paste(substr(nutrient, 0, 3), "(mg/m2)"), color = NULL) +
    scale_y_log10() + 
    theme(legend.position = "right", axis.text = element_text(color = "black", size = 10)) +
    scale_color_brewer(palette="Dark2")
}

