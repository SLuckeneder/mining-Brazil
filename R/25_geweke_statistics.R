

library(coda)

results_path <- "./data/intermediary/MCMC_draws_matching/"

type <- "industrial" # "industrial", "garimpo"

# Economic growth model yearly
chains <- list.files(results_path, pattern = paste0("gdp_yearly_", type))
load(file.path(results_path, chains))
full_chain_m = cbind(store[["coef_store"]],store[["rho_store"]], store[["sigma2_store"]])
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z
any(gconv < -3 | gconv > 3) # z>3 or z<-3 indicates non-convergence!

# Economic growth model pooled
chains <- list.files(results_path, pattern = paste0("gdp_pooled_", type))
load(file.path(results_path, chains))
full_chain_m = cbind(store[["coef_store"]],store[["rho_store"]], store[["sigma2_store"]])
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z
any(gconv < -3 | gconv > 3) # z>3 or z<-3 indicates non-convergence!

# Relative forest loss model yearly
chains <- list.files(results_path, pattern = paste0("rel_yearly_", type))
load(file.path(results_path, chains))
full_chain_m = cbind(store[["coef_store"]],store[["rho_store"]], store[["sigma2_store"]])
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z
any(gconv < -3 | gconv > 3) # z>3 or z<-3 indicates non-convergence!

# Relative forest loss model pooled
chains <- list.files(results_path, pattern = paste0("rel_pooled_", type))
load(file.path(results_path, chains))
full_chain_m = cbind(store[["coef_store"]],store[["rho_store"]], store[["sigma2_store"]])
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z
any(gconv < -3 | gconv > 3) # z>3 or z<-3 indicates non-convergence!

# Absolute forest loss model yearly
chains <- list.files(results_path, pattern = paste0("abs_yearly_", type))
load(file.path(results_path, chains))
full_chain_m = cbind(store[["coef_store"]],store[["rho_store"]], store[["sigma2_store"]])
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z
any(gconv < -3 | gconv > 3) # z>3 or z<-3 indicates non-convergence!

# Absolute forest loss model pooled
chains <- list.files(results_path, pattern = paste0("abs_pooled_", type))
load(file.path(results_path, chains))
full_chain_m = cbind(store[["coef_store"]],store[["rho_store"]], store[["sigma2_store"]])
mh.draws <- coda::mcmc(full_chain_m[,]) # seq(1,1000,by = 10)
gconv = coda::geweke.diag(mh.draws)$z
any(gconv < -3 | gconv > 3) # z>3 or z<-3 indicates non-convergence!


