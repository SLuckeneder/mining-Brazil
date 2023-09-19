

library(dplyr)
library(MASS)
library(expm)
library(Matrix)
library(matrixcalc)

if (!dir.exists("data/model_input/SI")){dir.create("data/model_input/SI", recursive = TRUE)}
if (!dir.exists("data/W")){dir.create("data/W", recursive = TRUE)}
if (!dir.exists("data/intermediary/MCMC_draws/SI")){dir.create("data/intermediary/MCMC_draws/SI", recursive = TRUE)}
if (!dir.exists("data/impact_estimates/draws/SI")){dir.create("data/impact_estimates/draws/SI", recursive = TRUE)}
if (!dir.exists("data/impact_estimates/summaries/SI")){dir.create("data/impact_estimates/summaries/SI", recursive = TRUE)}


# run models --------------------------------------------------------------

# Mining area in hectares instead of binary indicator
source("R/31_run_SI_hectares.R")

# Alternative economic growth windows
source("R/31_run_SI_growth_windows.R")

# Alternative spatial weights matrices
source("R/31_run_SI_W.R")


