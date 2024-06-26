# Throughfall-nutrients
Throughfall nutrients from Gigante, Panama

## Data files:
```
(1) Rainfall and throughfall nutrient data ("GFX Combined Master Data.csv");  
     (1a) Subsetted and processed rainfall and throughfall data ("GFXdata.RData");  
(2) Plot-level litter nutrients - processed data ("Litter merged_plot means.csv");  
     (2a) Litter mass - raw data ("GIG_FERTILIZAR_LITTER_20200602.DBF");  
     (2b) Litter nutrient concentration - raw data ("Gigante 2012 Master Data File PLANTS unified codes for wood with final isotopes FERTILIZER litter.txt");  
(3) Foliar nutrient concentration data ("Fresh Lvs Values Sp Tmt.txt")
```  
  
## R scripts:
``` 
(1) Annual throughfall and litterfall budgets ("Fig 1_Table 1_Calcs.R"); 
(2) Stats and figures for LAI ~ throughfall nutrient concentrations: Linear mixed models and associated figures (Figure 2 and Figure S4 ("Figs 2_S2_LAI.R");  
(3) Supplemental figures with throughfall nutrient concentrations ("Figs S1_S2_S3.R"); 
(4) Foliar nutrient concentrations ("Foliar.R");  
(5) Convert litter raw data (data files 2a, 2b) into plot-level means (data file 2) ("DataProcessing_LitterData.R")  
``` 
