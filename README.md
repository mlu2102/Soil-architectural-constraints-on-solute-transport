# Soil architectural constraints on solute transport drive variability in SOC redistribution and persistence

This repository contains statistical analyses and plotting scripts used to evaluate 
relationships between allogenic carbon index (ACI) metrics and soil physical properties
and hydroclimatic metrics. Analyses include linear regression models and 
leave-one-out (jackknife) sensitivity analyses supporting figures and tables 
in the accompanying manuscript.

Analyses were conducted and plots were generated using R version 4.5.2 on macOS

## Repository structure

```text
├── data/
│   ├── raw/                      # Raw input data (CSV)
│   └── intermediate/             # Model results and jackknife outputs (RDS)
│
├── scripts/
│   ├── 00_run_all.R              # Master script to execute analysis and plotting scripts
│   ├── 01_stats.R                # Statistical models and jackknife analyses
│   ├── 02_plots.R                # Figure generation
│   ├── 03_depthDistributions.R   # Figure generation
│
├── output/
│   └── plots/                    # Final figures used in manuscript and SI
|   └── data/                     # Data used in Tables S2 & S3
│
└── README.md
```

## Running the analysis and plotting figures

All analyses and figures can be reproduced by running the master script:

```r
source("scripts/00_run_all.R")
```

## R dependencies

This project requires the following R packages:

- tidyverse
- ggplot2
- patchwork
- cowplot
- grid
- ggnewplot

Optional installation lines are included (commented) in `00_run_all.R`.

## Data notes

Raw data files in `data/raw/` are provided as used in the analyses.

Intermediate `.rds` files are generated automatically and are included to
support transparency and rapid figure generation.

## Associated manuscript

This repository supports analyses presented in:

> *Soil architectural constraints on solute transport drive variability in SOC 
redistribution and persistence*, submitted to *Nature Communications Earth & Environment*

Figures and tables referenced in the manuscript correspond directly to outputs
in `output/plots/` and `output/data/`, respectively

## Contact

For questions regarding this repository, contact:

Micah Unruh 
micah.unruh@ku.edu
