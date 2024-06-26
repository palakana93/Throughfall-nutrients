# Throughfall-nutrients
Throughfall nutrients from Gigante, Panama

## Data files:
```
(1) Rainfall and throughfall nutrient data ("GFX Combined Master Data.csv")
     (1a) Subsetted and processed rainfall and throughfall data ("GFXdata.RData")
          This file contains five data table R "objects"
             (1) "data" contains all data from "GFX Combined Master Data.csv"
             (2) "data.nutrients" omits observations with unusable nutrient data (e.g., samples where nutrient data was not obtained, DI water controls)
             (3) "data.LAI" omits observations with unusable LAI data (7 canopy photos were unusable due to sunlight penetration into foliage)
             (4) "dataForPlots" omits observations with either unusable nutrient or unusable LAI data
             (5) "dataForStats" is "dataForPlots" with rain samples removed
(2) Plot-level litter nutrients - processed data ("Litter merged_plot means.csv")
     (2a) Litter mass - raw data ("GIG_FERTILIZAR_LITTER_20200602.DBF")
     (2b) Litter nutrient concentration - raw data ("Gigante 2012 Master Data File PLANTS unified codes for wood with final isotopes FERTILIZER litter.txt")
(3) Foliar nutrient concentration data ("Fresh Lvs Values Sp Tmt.txt")
```  
  
## R scripts:
``` 
(1) Effects of fertilizer treatment on throughfall and litterfall nutrients: Figures 1 and S5, calculations for Tables 1 and S2, other miscellaneous calculations for paper ("Fig 1_Table 1_Calcs.R")
(2) Stats (linear mixed models) and figures for LAI vs. throughfall nutrient concentrations: Figures 2 and S4, model output in Table S1 ("Figs 2_S4_LAI.R")
(3) Supplemental figures of nutrient concentrations/nutrient recycling: Figures S1, S2, S3 ("Figs S1_S2_S3.R")
(4) Foliar nutrient concentrations: Used in Tables 1 and S2 ("Foliar.R")
(5) Convert litter raw data (Data files 2a, 2b) into plot-level means (Data file 2) ("DataProcessing_LitterData.R"; used in R script 1)
``` 
