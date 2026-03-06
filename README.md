# Co-migration_fidelity
# Title: "Wind conditions drive co-migration fidelity in trans-Saharan birds at a Mediterranean stopover site"

## Overview

This repository contains the R scripts and data used to analyse temporal
co-occurrence networks of 24 migratory bird species captured at Ponza Island
(Italy) between March and May from 2007 to 2024.

The analysis reconstructs yearly species co-occurrence networks using
Spearman correlations of daily abundances and evaluates the modular
structure of migration timing.

Main analyses include:

• construction of yearly abundance matrices  
• network inference using lag-1 partial correlations  
• modularity analysis using the Louvain algorithm  
• null model simulations  
• co-migration fidelity estimation using Normalised Mutual Information (NMI)

---

## Repository structure
migration_network_analysis/
│
├── README.md
│
├── data/
│ ├── raw/
│ │ └── P24Species.rds
│ │
│ └── processed/
│ └── ponza_abundance_list.rds
│
├── scripts/
│ ├── 01_data_cleaning.R
│ ├── 02_weather_processing.R
│ ├── 03_abundance_timeseries.R
│ ├── 04_network_analysis.R
│ ├── 05_null_model.R
│ └── 06_figures_tables.R
│
├── output/
│ ├── figures/
│ │ ├── Modularity24.png
│ │ ├── Modularity24_avoidance.png
│ │ └── Dendrogram_CoMigration_Fidelity.png
│ │
│ └── tables/
│ └── nmi_species_similarity.csv
│
└── environment/
└── sessionInfo.txt


---

## Data description

P24Species.rds

Dataset containing bird ringing records collected at Ponza Island.

Variables include:

YEAR – sampling year  
JDays – Julian day of capture  
SCIENTIFIC_NAME – species name  
Locality – number of individuals captured  

---

## Running the analysis

Scripts must be executed in the following order:

1. 01_data_cleaning.R  
2. 02_weather_processing.R  
3. 03_abundance_timeseries.R  
4. 04_network_analysis.R  
5. 05_null_model.R  
6. 06_figures_tables.R  

---

## R environment

R version ≥ 4.2 recommended.

Required packages:

igraph  
vegan  
bipartite  
smooth  
data.table  
psych  
Hmisc  
dplyr  
readxl  
netdiffuseR  
tidyverse  
trend  
pheatmap  
dendextend  

---

## Reproducibility

Random seeds were set using:

set.seed(1234)

Package versions used for the analysis are reported in:

environment/sessionInfo.txt
