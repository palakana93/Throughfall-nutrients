
## Study overview
These data derive from the Gigante NPK Experiment, a long-term factorial nitrogen (N), phosphorus (P), and potassium (K) fertilization experiment located in old-growth tropical forest (~600 years old) in the Barro Colorado Nature Monument, central Panama (9°06′31″ N, 79°50′37″ W). Since 1998, 32 plots (40 × 40 m) have received factorial additions of N, P, and K in a replicated incomplete-block design (four replicates per fertilization treatment). An additional four plots received micronutrient additions only.

The datasets support analyses of fertilization effects on:
(1) Canopy throughfall nutrient concentrations and fluxes (measured during the 2018 rainy season, ~20 years after fertilization began).
(2) Aboveground fine litterfall mass and nutrient fluxes, derived from long-term litter production (2003–2018) and litter nutrient concentrations measured in 2012.
(3) Foliar nutrient concentrations in sun-exposed canopy leaves (measured in 2009) and understory leaves (measured in 2012) from common tree species.

Because logistical constraints prevented resampling of litter and foliar nutrients in 2018, foliar and litter nutrient concentrations precede throughfall measurements by 6–9 years. However, fertilization had been applied for 11–14 years at the time of foliar and litter sampling and for 20 years at the time of throughfall sampling, a duration expected to capture fully developed fertilization responses.


## Citation and related publication
These data support a manuscript examining long-term fertilization effects on throughfall and litter nutrient fluxes at the Gigante NPK Experiment. A formal citation will be added upon publication.


## Files
Experimental setup metadata:
 - plot_metadata.csv

Foliar data files:
 - raw_canopy_nutrients.csv
 - raw_understory_nutrients.txt

Litter data files:
 - raw_litter_mass_2020-06-02.csv
 - raw_litter_nutrient conc.txt
 - processed_litter_mass_by_year.csv
 - processed_litter_plot_means.csv

Throughfall data files:
 - raw_throughfall.csv
 - processed_throughfall_plot_means.csv


## Missing values
 - NA where applicable
 - "-9" used only in raw_litter_mass_2020-06-02.csv and should be treated as missing


## Treatment and indicator conventions
 - Treatment labels distinguish factorial fertilization plots (Control, N, P, K, NP, NK, PK, NPK), micronutrient-only plots, and rainfall samples (Rain)
 - N/P/K indicators represent nutrient addition status for fertilization plots (generally encoded as TRUE/FALSE, occasionally as 1/0); these indicators are not applicable to rainfall samples
 - Raw files preserve original variable names (e.g., Treatment vs. tmt, Species vs. sp); processed files standardize names and encodings
 - Species codes are dataset-specific and defined per file (applies to foliar nutrient data files)
 - Coordinates are site-relative and do not represent geographic coordinates


## Temporal scope
 - Canopy foliar nutrients: 2009
 - Understory foliar nutrients: 2012
 - Litter nutrients: 2012
 - Litter mass: 1998–2018 (processed data files from 2003–2018; first five years of fertilization excluded)
 - Throughfall chemistry and leaf area index (LAI): 2018


## Data processing and aggregation
Processed datasets summarize raw measurements at the plot or plot-by-year level using documented averaging and scaling procedures. Raw files retain original sampling structure and encodings; processed files are intended for statistical analysis and figure generation.


## File: plot_metadata.csv
Description:
Plot-level metadata describing fertilization treatments, treatment replicates, and experimental block assignment for the factorial N–P–K fertilization experiment. Micronutrient-only plots are not included in this file.

Rows:
One row per plot (n = 32).

Columns:
 - Plot: Plot identifier (n = 32)
 - Treatment: Factorial fertilization treatment (Control, +P, +N, +K, +PK, +NK, +NP, +NPK)
 - Replicate: Treatment replicate (integer; 1–4)
 - Block: Experimental block (integer; 1 or 2)
 - N: Nitrogen addition indicator (binary; TRUE = N added, FALSE = no N)
 - P: Phosphorus addition indicator (binary; TRUE = P added, FALSE = no P)
 - K: Potassium addition indicator (binary; TRUE = K added, FALSE = no K)


## File: raw_canopy_nutrients.csv
Description:
Foliar nutrient concentrations measured in sun-exposed canopy leaves from three common tree species; samples collected in 2009. One individual per species was sampled per plot; not all species were present in every plot. Four additional plots received a micronutrient addition only and were not part of the factorial N–P–K fertilization design.

Rows:
One row per plot (n = 36; n = 32 factorial fertilization plots plus n = 4 micronutrient-only plots) × species (n = 3).

Columns:
 - Plot: Plot identifier (n = 36)
 - Treatment: Fertilization treatment (Control, P, N, K, PK, NK, NP, NPK, Micronutrient)
 - Species: Tree species code (ALSEBL = Alseis blackiana, TET2PA = Protium stevensonii [formerly: Tetragastris panamensis] , SIMAAM = Simarouba amara)
 - Carbon: Foliar carbon concentration (% dry mass)
 - N.conc: Foliar nitrogen concentration (% dry mass)
 - Al: Foliar aluminum concentration (mg g⁻¹ dry mass)
 - Ca: Foliar calcium concentration (mg g⁻¹ dry mass)
 - Cu: Foliar copper concentration (mg g⁻¹ dry mass)
 - K.conc: Foliar potassium concentration (mg g⁻¹ dry mass)
 - Mg: Foliar magnesium concentration (mg g⁻¹ dry mass)
 - Mn: Foliar manganese concentration (mg g⁻¹ dry mass)
 - P.conc: Foliar phosphorus concentration (mg g⁻¹ dry mass)
 - Zn: Foliar zinc concentration (mg g⁻¹ dry mass)


## File: raw_understory_nutrients.txt
Description:
Foliar nutrient concentrations measured in understory leaves from three common tree species; samples collected in 2012. One individual per species was sampled per plot; not all species were present in every plot. Four additional plots received a micronutrient addition only and were not part of the factorial N–P–K fertilization design.

Rows:
One row per plot (n = 36; n = 32 factorial fertilization plots plus n = 4 plots micronutrient-only plots) × species (n = 3).

Columns:
 - plot: Plot identifier (n = 36)
 - ID: Unique tree identifier
 - sp: Tree species code (ALSEBL = Alseis blackiana, TET2PA = Protium stevensonii [formerly: Tetragastris panamensis] , HEISCO = Heisteria concinna)
 - Al: Foliar aluminum concentration (mg g⁻¹ dry mass)
 - B: Foliar boron concentration (mg g⁻¹ dry mass)
 - Ca: Foliar calcium concentration (mg g⁻¹ dry mass)
 - Cu: Foliar copper concentration (mg g⁻¹ dry mass)
 - Fe: Foliar iron concentration (mg g⁻¹ dry mass)
 - K.conc: Foliar potassium concentration (mg g⁻¹ dry mass)
 - Mg: Foliar magnesium concentration (mg g⁻¹ dry mass)
 - Mn: Foliar manganese concentration (mg g⁻¹ dry mass)
 - Na: Foliar sodium concentration (mg g⁻¹ dry mass)
 - P.conc: Foliar phosphorus concentration (mg g⁻¹ dry mass)
 - Zn: Foliar zinc concentration (mg g⁻¹ dry mass)
 - C: Foliar carbon concentration (% dry mass)
 - N.conc: Foliar nitrogen concentration (% dry mass)
 - Delta13C: Stable carbon isotope composition of foliar tissue (δ¹³C, ‰)
 - Delta15N: Stable nitrogen isotope composition of foliar tissue (δ¹⁵N, ‰)
 - Cnratio: Ratio of foliar C to foliar N
 - Cpratio: Ratio of foliar C to foliar P
 - Npratio: Ratio of foliar N to foliar P
 - N: Nitrogen addition indicator (binary; 1 = N added, 0 = no N)
 - P: Phosphorus addition indicator (binary; 1 = P added, 0 = no P)
 - K: Potassium addition indicator (binary; 1 = K added, 0 = no K)
 - block: Experimental block (integer; 1 or 2; 0 for micronutrient plots)
 - repl: Treatment replicate (integer; 1–4; 0 for micronutrient plots)
 - micro: Micronutrient plot indicator (binary; 1 = micronutrient plot, 0 = not micronutrient)
 - tmt: Fertilization treatment (Control, P, N, K, PK, NK, NP, NPK, MICRO)


## File: raw_litter_mass_2020-06-02.csv
Description:
Monthly litter mass collected from 0.57 m² litter traps within each plot. Litter was collected on the final Thursday of each month from three to five traps per plot (three traps prior to February 2007; five traps thereafter). Litter was separated into four components (leaves, reproductive material [flowers and fruits], branches, and dust). The value -9 is used to denote missing values for litter components (Leaves, Reproductive, Branches, Dust), primarily prior to November 1998 when litter was not separated into components. Missing values for Total litter mass for a given trap are denoted by NA. Four additional plots received a micronutrient addition only and were not part of the factorial N–P–K fertilization design.

Rows:
One row per litter trap × month.

Columns:
 - Date: Sample date (Month/Day/Year)
 - Plot: Plot identifier (n = 36)
 - Trap_Coords_x.y: Litter trap coordinates within the experimental site reference system (site extent ~480 × 800 m; format: x_coordinate.y_coordinate)
 - Leaves: Mass of leaf litterfall (g)
 - Reproductive: Mass of reproductive litterfall (g; includes flowers and fruits)
 - Branches: Mass of branch litterfall (g)
 - Dust: Mass of fine particulate (dust) litterfall (g)
 - Total: Total litterfall mass (g; sum of Leaves, Reproductive, Branches, and Dust where components were measured)

Notes:
A value of −9 indicates missing values for litter component columns (Leaves, Reproductive, Branches, Dust). Missing values for Total litter mass are denoted by NA.


## File: raw_litter_nutrient_conc.txt
Description:
Litter nutrient concentrations measured from litter collected for chemical analysis in April and November 2012, following one week after which litter traps were previously emptied. Collected litter was oven-dried to constant mass, and an equal dry mass from each of the five litter traps per plot was pooled for nutrient analysis. Four additional plots received a micronutrient addition only and were not part of the factorial N–P–K fertilization design.

Rows:
One row per plot × month × nutrient metric.

Columns:
 - Plot: Plot identifier (n = 36)
 - Species: Sample type identified; all values are "Litter_Trap"
 - Month: Collection month (April or November)
 - Parameter: Nutrient metric (Total_Al, Total_B, Total_Ca, Total_Cu, Total_Fe, Total_K, Total_Mg, Total_Mn, Total_Na, Total_P, Total_Zn, Total_C, Total_N, CN_Ratio, CP_Ratio, NP_Ratio)
 - Value: Nutrient concentration for Total_* variables (mg g⁻¹ dry mass, except % dry mass for Total_C and Total_N) or mass-based ratio for *_Ratio variables


## File: processed_litter_mass_by_year.csv
Description:
Annual litter mass per plot, derived from raw litter mass data after excluding the first five years of fertilization (1998-2002) to account for a potential lag in fertilization effects. Data span 2003–2018. Micronutrient-only plots were excluded because they were not part of the factorial N–P–K design.

Rows:
One row per plot × year.

Columns:
 - Year: Collection year (2003–2018)
 - Plot: Plot identifier (n = 32)
 - Treatment: Factorial fertilization treatment (Control, +P, +N, +K, +PK, +NK, +NP, +NPK)
 - Replicate: Treatment replicate (integer; 1–4)
 - Block: Experimental block (integer; 1 or 2)
 - N: Nitrogen addition indicator (binary; TRUE = N added, FALSE = no N)
 - P: Phosphorus addition indicator (binary; TRUE = P added, FALSE = no P)
 - K: Potassium addition indicator (binary; TRUE = K added, FALSE = no K)
 - Total: Total litter mass (kg ha⁻¹ y⁻¹)


## File: processed_litter_plot_means.csv
Description:
Plot-level mean litter nutrient concentrations and annual nutrient fluxes. Values represent fine litter only (no coarse woody debris). Annual fluxes were derived by combining litter nutrient concentrations measured in 2012 (raw_litter_nutrient conc.txt) with annual litter mass data from 2003–2018 (processed_litter_mass_by_year.csv). Litter data from the first five years (1998-2002) were excluded to account for a potential lag in fertilization response. Micronutrient-only plots were excluded because they were not part of the factorial N–P–K design.

Rows:
One row per plot (n = 32).

Columns:
 - Plot: Plot identifier (n = 32)
 - Replicate: Treatment replicate (integer; 1–4)
 - Block: Experimental block (integer; 1 or 2)
 - Treatment: Factorial fertilization treatment (Control, +P, +N, +K, +PK, +NK, +NP, +NPK)
 - N: Nitrogen addition indicator (binary; TRUE = N added, FALSE = no N)
 - P: Phosphorus addition indicator (binary; TRUE = P added, FALSE = no P)
 - K: Potassium addition indicator (binary; TRUE = K added, FALSE = no K)
 - Mass: Total litter mass (kg ha⁻¹ y⁻¹)
 - Ca_ppt: Litter calcium concentration (mg g⁻¹ dry mass)
 - K_ppt: Litter potassium concentration (mg g⁻¹ dry mass)
 - Mg_ppt: Litter magnesium concentration (mg g⁻¹ dry mass)
 - N_ppt: Litter nitrogen concentration (mg g⁻¹ dry mass)
 - Na_ppt: Litter sodium concentration (mg g⁻¹ dry mass)
 - P_ppt: Litter phosphorus concentration (mg g⁻¹ dry mass)
 - P_flux: Litter phosphorus flux (kg ha⁻¹ y⁻¹)
 - N_flux: Litter nitrogen flux (kg ha⁻¹ y⁻¹)
 - K_flux: Litter potassium flux (kg ha⁻¹ y⁻¹)
 - Mg_flux: Litter magnesium flux (kg ha⁻¹ y⁻¹)
 - Ca_flux: Litter calcium flux (kg ha⁻¹ y⁻¹)
 - Na_flux: Litter sodium flux (kg ha⁻¹ y⁻¹)


## File: raw_throughfall.csv
Description:
Throughfall and rainfall chemistry from rain events sampled in June–July 2018, including nutrient concentrations and deposition fluxes. Four rain events include nutrient chemistry; a fifth event includes LAI measurements only (no nutrients analyzed). Throughfall samples were not collected from micronutrient-only plots. Rainfall samples come from canopy gaps rather than plots.

Rows:
One row per throughfall or rainfall sample.

Columns:
 - ID: Sample identifier combining rain event and collector ID (format: Collection-Bottle)
 - Plot: Plot identifier for throughfall samples (n = 32) or canopy gap identifier for rain samples (n = 3)
 - Treatment: Treatment label (Control, P, N, K, PK, NK, NP, NPK, Rain)
 - Block: Experimental block (integer; 1 or 2; NA for rain samples)
 - Replicate: Treatment replicate (integer; 1–4; NA for rain samples)
 - P: Phosphorus addition indicator (binary for fertilization plots; TRUE = P added, FALSE = no P; value "Rain" indicates rainfall samples)
 - N: Nitrogen addition indicator (binary for fertilization plots; TRUE = N added, FALSE = no N; value "Rain" indicates rainfall samples)
 - K: Potassium addition indicator (binary for fertilization plots; TRUE = K added, FALSE = no K; value "Rain" indicates rainfall samples)
 - Collection: Rain event identifier (integer, 1–5); nutrient chemistry available for events 1–4 only
 - Type: Water sample type ("Tfall" = throughfall; "Rain" = rainfall)
 - Bottle: Collector ID
 - Date_collected: Collection date (Month/Day/Year)
 - Coord_x: Sample x-coordinate within the experimental site reference system (site extent ~480 × 800 m)
 - Coord_y: Sample y-coordinate within the experimental site reference system (site extent ~480 × 800 m)
 - LAI: Leaf area index above sample collector (m² m⁻²; calculated from canopy photographs cropped to a ±4.5° zenith angle)
 - Volume_mL: Sample volume collected for throughfall or rainfall (mL)
 - Rain_cm: Mean rainfall depth for the rain event (cm)
 - PO4_ppm: Phosphate concentration (mg L⁻¹)
 - NH4_ppm: Ammonium concentration (mg L⁻¹)
 - NO3_ppm: Nitrate concentration (mg L⁻¹)
 - N_ppm: Total inorganic nitrogen (ammonium + nitrate) concentration (mg L⁻¹)
 - K_ppm: Potassium concentration (mg L⁻¹)
 - Ca_ppm: Calcium concentration (mg L⁻¹)
 - Mg_ppm: Magnesium concentration (mg L⁻¹)
 - Na_ppm: Sodium concentration (mg L⁻¹)
 - P_total_ppm: Total phosphorus concentration (mg L⁻¹)
 - PO4.dep: Phosphate deposition flux (mg m⁻² event⁻¹)
 - NH4.dep: Ammonium deposition flux (mg m⁻² event⁻¹)
 - NO3.dep: Nitrate deposition flux (mg m⁻² event⁻¹)
 - N.dep: Inorganic nitrogen (ammonium plus nitrate) deposition flux (mg m⁻² event⁻¹)
 - K.dep: Potassium deposition flux (mg m⁻² event⁻¹)
 - Ca.dep: Calcium deposition flux (mg m⁻² event⁻¹)
 - Mg.dep: Magnesium deposition flux (mg m⁻² event⁻¹)
 - Na.dep: Sodium deposition flux (mg m⁻² event⁻¹)
 - P.total.dep: Total phosphorus deposition flux (mg m⁻² event⁻¹)
 - PO4.rain: Mean rainfall phosphate deposition flux per rain event (mg m⁻² event⁻¹)
 - NH4.rain: Mean rainfall ammonium deposition flux per rain event (mg m⁻² event⁻¹)
 - NO3.rain: Mean rainfall nitrate deposition flux per rain event (mg m⁻² event⁻¹)
 - K.rain: Mean rainfall potassium deposition flux per rain event (mg m⁻² event⁻¹)

Notes:
Variables ending in “.dep” represent nutrient deposition fluxes measured for each throughfall or rainfall collector during each rain event (mg m⁻² event⁻¹). Variables ending in “.rain” represent rain-event means calculated across rainfall collectors (typically n = 9, though occasionally fewer than 9 samples analyzed depending on sample volume).


## File: processed_throughfall_plot_means.csv
Description:
Plot-level mean throughfall nutrient concentrations and deposition fluxes, averaged first within plots by rain event and then across rain events with equal event weighting. Throughfall samples were not collected from micronutrient-only plots. Rainfall samples come from canopy gaps rather than plots.

Rows:
One row per plot or canopy gap (n = 35 total).

Columns:
 - Plot: Plot identifier for throughfall samples (n = 32) or canopy gap identifier for rain samples (n = 3)
 - Treatment: Treatment label (Control, P, N, K, PK, NK, NP, NPK, Rain)
 - Replicate: Treatment replicate (integer; 1–4; NA for rain samples)
 - Block: Experimental block (integer; 1 or 2; NA for rain samples)
 - N: Nitrogen addition indicator (binary for fertilization plots; TRUE = N added, FALSE = no N; value "Rain" indicates rainfall samples)
 - P: Phosphorus addition indicator (binary for fertilization plots; TRUE = P added, FALSE = no P; value "Rain" indicates rainfall samples)
 - K: Potassium addition indicator (binary for fertilization plots; TRUE = K added, FALSE = no K; value "Rain" indicates rainfall samples)
 - NO3_ppm: Nitrate concentration (mg L⁻¹)
 - NH4_ppm: Ammonium concentration (mg L⁻¹)
 - N_ppm: Total inorganic nitrogen (ammonium + nitrate) concentration (mg L⁻¹)
 - PO4_ppm: Phosphate concentration (mg L⁻¹)
 - K_ppm: Potassium concentration (mg L⁻¹)
 - Ca_ppm: Calcium concentration (mg L⁻¹)
 - Mg_ppm: Magnesium concentration (mg L⁻¹)
 - Na_ppm: Sodium concentration (mg L⁻¹)
 - NO3.dep: Nitrate deposition flux (mg m⁻² event⁻¹)
 - NH4.dep: Ammonium deposition flux (mg m⁻² event⁻¹)
 - N.dep: Inorganic nitrogen (ammonium plus nitrate) deposition flux (mg m⁻² event⁻¹)
 - PO4.dep: Phosphate deposition flux (mg m⁻² event⁻¹)
 - K.dep: Potassium deposition flux (mg m⁻² event⁻¹)
 - Ca.dep: Calcium deposition flux (mg m⁻² event⁻¹)
 - Mg.dep: Magnesium deposition flux (mg m⁻² event⁻¹)
 - Na.dep: Sodium deposition flux (mg m⁻² event⁻¹)

