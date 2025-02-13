

library(dplyr)
library(MASS)
library(expm)
library(Matrix)
library(matrixcalc)

if (!dir.exists("data/W")){dir.create("data/W", recursive = TRUE)}
if (!dir.exists("data/cem/cem_model_input")){dir.create("data/cem/cem_model_input")}
if (!dir.exists("data/intermediary/MCMC_draws_matching")){dir.create("data/intermediary/MCMC_draws_matching")}
if (!dir.exists("data/cem/sdm_impact_estimates/draws")){dir.create("data/cem/sdm_impact_estimates/draws")}
if (!dir.exists("data/cem/sdm_impact_estimates/summaries")){dir.create("data/cem/sdm_impact_estimates/summaries")}


# run models --------------------------------------------------------------

# Mining area in hectares instead of binary indicator
source("R/41_run_SI_hectares_matching.R")
source("R/41_run_SI_hectares_matching_pooled.R")

# Alternative economic growth windows
source("R/42_run_SI_growth_windows_matching.R")
source("R/42_run_SI_growth_windows_matching_pooled.R")

# Alternative spatial weights matrices
source("R/43_run_SI_W_matching.R")


