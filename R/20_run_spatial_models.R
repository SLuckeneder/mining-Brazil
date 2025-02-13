

if (!dir.exists("data/W")){dir.create("data/W")}
if (!dir.exists("data/cem/cem_model_input")){dir.create("data/cem/cem_model_input")}
if (!dir.exists("data/intermediary")){dir.create("data/intermediary")}
if (!dir.exists("data/intermediary/MCMC_draws_matching")){dir.create("data/intermediary/MCMC_draws_matching")}
if (!dir.exists("data/cem/sdm_impact_estimates/draws")){dir.create("data/cem/sdm_impact_estimates/draws")}
if (!dir.exists("data/cem/sdm_impact_estimates/summaries")){dir.create("data/cem/sdm_impact_estimates/summaries")}


# industrial --------------------------------------------------------------

# Economic growth model yearly
source("R/22_run_gdp_yearly_industrial.R")

# Economic growth model pooled
source("R/22_run_gdp_pooled_industrial.R")

# Relative forest loss model yearly
source("R/23_run_def_rel_yearly_industrial.R")

# Relative forest loss model pooled
source("R/23_run_def_rel_pooled_industrial.R")

# Absolute forest loss model yearly
source("R/24_run_def_abs_yearly_industrial.R")

# Absolute forest loss model pooled
source("R/24_run_def_abs_pooled_industrial.R")

# garimpo --------------------------------------------------------------

# Economic growth model yearly
source("R/22_run_gdp_yearly_garimpo.R")

# Economic growth model pooled
source("R/22_run_gdp_pooled_garimpo.R")

# Relative forest loss model yearly
source("R/23_run_def_rel_yearly_garimpo.R")

# Relative forest loss model pooled
source("R/23_run_def_rel_pooled_garimpo.R")

# Absolute forest loss model yearly
source("R/24_run_def_abs_yearly_garimpo.R")

# Absolute forest loss model pooled
source("R/24_run_def_abs_pooled_garimpo.R")



