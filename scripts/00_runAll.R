# *************************************************************************
# Master script for ACI analysis + figure generation
#
# This script runs the full two-part workflow:
#   1) 01_stats.R  - fits linear models and generates jackknife sensitivity analyses
#   2) 02_plots.R  - generates and saves publication figures
#
# Optional package installation lines are provided below.
# *************************************************************************

# Optional: install required packages -------------------------------------
# Uncomment if packages are not already installed

# pkgs <- c("tidyverse", "ggplot2", "patchwork", "cowplot", "grid", "ggnewscale")
# to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
# if (length(to_install) > 0) install.packages(to_install)

# Clear environment -------------------------------------------------------

rm(list = ls())

# Run scripts -------------------------------------------------------------

source("scripts/01_stats.R")
source("scripts/02_plots.R")
source("scripts/03_depthDistributions.R")