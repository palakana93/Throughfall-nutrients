# Throughfall-nutrients
Throughfall nutrients from Gigante, Panama

## Data files:
```
(1) Rainfall and throughfall nutrient data ("GFX Combined Master Data.csv")
     (1a) Subsetted and processed rainfall and throughfall data ("GFXdata.RData")
(2) Plot-level litter nutrients - processed data ("Litter merged_plot means.csv")
     (2a) Litter mass - raw data ("GIG_FERTILIZAR_LITTER_20200602.DBF")
     (2b) Litter nutrient concentration - raw data ("Gigante 2012 Master Data File PLANTS unified codes for wood with final isotopes FERTILIZER litter.txt")
(3) Foliar nutrient concentration data ("Fresh Lvs Values Sp Tmt.txt")
```  
  
## R scripts:
``` 
(1) Effects of fertilizer treatment on throughfall and litterfall nutrients: Figure 1, calculations for Tables 1 and S2, other miscellaneous calculations for paper ("Fig 1_Table 1_Calcs.R")
(2) Stats (linear mixed models) and figures for LAI vs. throughfall nutrient concentrations: Figures 2 and S4, model output in Table S1 ("Figs 2_S2_LAI.R")
(3) Supplemental figures of nutrient concentrations/nutrient recycling: Figures S1, S2, S3, and S5 ("Figs S1_S2_S3_S5.R")
(4) Foliar nutrient concentrations: Used in Tables 1 and S2 ("Foliar.R")
(5) Convert litter raw data (Data files 2a, 2b) into plot-level means (Data file 2) ("DataProcessing_LitterData.R"; used in R script 1)
``` 
