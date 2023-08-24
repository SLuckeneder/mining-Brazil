

library(dplyr)
library(MASS)
library(expm)
library(Matrix)
library(matrixcalc)

if (!dir.exists("data/model_input")){dir.create("data/model_input")}
if (!dir.exists("data/W")){dir.create("data/W")}
if (!dir.exists("data/intermediary")){dir.create("data/intermediary")}
if (!dir.exists("data/intermediary/MCMC_draws")){dir.create("data/intermediary/MCMC_draws")}
if (!dir.exists("data/impact_estimates")){dir.create("data/impact_estimates")}
if (!dir.exists("data/impact_estimates/draws")){dir.create("data/impact_estimates/draws")}
if (!dir.exists("data/impact_estimates/summaries")){dir.create("data/impact_estimates/summaries")}

# TODO: Start script with if(data does not exist){load from Zenodo}

# sampler setup
ntot <- 10000
nburn <- 5000
nretain = ntot - nburn
t <- 11 # 11 years in main model

source("./R/11_lndetPaceBarry.R")

# inverse gamma prior for sigma
a_pr <- 0.01
b_pr <- 0.01

# rho prior is beta, with
rho_a = 1.01

beta_prob = function(rho,a) 1/beta(a,a) * ((1+rho)^(a-1) *(1-rho)^(a-1))/(2^(2*a - 1))

# Spatial weights matrix
load("./data/W/W_k5.RData")
WW <- kronecker(.sparseDiagonal(t), W_k)

# Economic growth model yearly
source("R/12_run_gdp_yearly.R")

# Economic growth model pooled
source("R/12_run_gdp_pooled.R")

# Relative forest loss model yearly
source("R/12_run_def_rel_yearly.R")

# Relative forest loss model pooled
source("R/12_run_def_rel_pooled.R")

# Absolute forest loss model yearly
source("R/12_run_def_abs_yearly.R")

# Absolute forest loss model pooled
source("R/12_run_def_abs_pooled.R")



