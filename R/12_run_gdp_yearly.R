


# prepare estimation ------------------------------------------------------

# read data
YX <- read.csv2(file = "./data/model_input/YX_gdp_yearly_2005-2015_5y.csv", sep = ",", stringsAsFactors = FALSE) %>% mutate_all(as.numeric)
Y <- YX %>% dplyr::select(1) %>% as.matrix()
X <- YX %>% dplyr::select(-1) %>% as.matrix()

# make dummies out of year column and then remove this column 
D <- dummies::dummy(X[,"year"], sep = "_")[,-1]
X <- X[, colnames(X) != "year"]
k_small <- ncol(X)  # no. of covariates excluding spatial lag (needed later for impact calculations)
X <- cbind(1, X, WW %*% X, D) # construct SDM design matrix

k_large <- ncol(X)
N <- nrow(W_k)

# beta priors
beta_prior_mean2 = matrix(0,k_large ,1)
beta_prior_var2 = diag(k_large) * 10^4



# start estimation --------------------------------------------------------

# starting values
curr.sigma2 <- 10
curr.rho = 0

# set-up for griddy gibbs
griddy_n = 200
logdets = lndetPaceBarry(WW,length.out = griddy_n+2)
logdets = logdets[-c(1,griddy_n + 2),]
rrhos = logdets[,2]

# pre-calculate some terms for faster draws
WY = WW %*% Y
XpXi <- solve(crossprod(X))
beta_prior_var_inv2 = solve(beta_prior_var2)
XpXi_pr <- solve(beta_prior_var_inv2 + crossprod(X))
tX <- t(X)
BB <- beta_prior_var_inv2%*%beta_prior_mean2
SY <- (Y-curr.rho*WY)

# store parameters
coef_store <- matrix(NA, nrow = nretain, ncol = k_large)
sigma2_store <- matrix(NA, nrow = nretain, ncol = 1)
rho_store <- matrix(NA, nrow = nretain, ncol = 1)

for(irep in 1:ntot){
  cat(irep,"RHO:",(curr.rho),"\n")
  # -------------------------------------------------------------------------------
  # 2: draw beta
  
  H = XpXi_pr*curr.sigma2
  b = H %*% (BB + (tX %*% SY)*1/curr.sigma2)
  curr.beta = mvrnorm(1,b,H)
  curr.xb = X %*% curr.beta
  
  
  # --------------------------------------------------------------------------------
  # 2: draw sigma2
  
  a_po <- a_pr + (t*N)/2
  b_po <- as.numeric(b_pr + crossprod(SY - curr.xb)/2)
  curr.sigma2 <- 1/rgamma(1, a_po, b_po)
  
  # -------------------------------------------------------------------------------
  # 3: sample rho with griddy gibbs
  
  ess.grid = sapply(logdets[,2], function(x) sum(dnorm(as.matrix((Y - x*WY)), mean=as.matrix(curr.xb), sd=sqrt(curr.sigma2),log = T))  )
  log.cond.post.rho = logdets[,1] + ess.grid + log(beta_prob(rrhos,rho_a))
  log.cond.post.rho=log.cond.post.rho-max(log.cond.post.rho);
  cond.post.rho=exp(log.cond.post.rho);
  z=cumsum(cond.post.rho) / sum(cond.post.rho)
  rnd = runif(1) #* sum(z)
  ind = min(which(rnd <= z))
  if (is.integer(ind) && ind <= length(logdets[,2]) && logdets[ind,2]!=curr.rho) {
    curr.rho = logdets[ind,2]
    SY <- (Y-curr.rho*WY)
  }
  
  # --------------------------------------------------------------------------------
  # 4: save draws
  if(irep > nburn){
    coef_store[irep-nburn,] <- as.vector(curr.beta)
    sigma2_store[irep-nburn,] <- curr.sigma2
    rho_store[irep-nburn,1] <- curr.rho
  }
  # pb$tick()
}

store <- list("coef_store" = coef_store, 
              "sigma2_store" = sigma2_store, 
              "rho_store" = rho_store)

save(store, file = paste0("data/intermediary/MCMC_draws/gdp_yearly_", Sys.time(), ".Rdata"))


var_names <- c("int", colnames(X)[c(2:(k_small+1))], paste0("W", colnames(X)[c((k_small+2):(1+k_small*2))]), colnames(X)[(k_small*2+2):ncol(X)])

# # check draws
# coef_summary <- cbind(var_names, colMeans(coef_store))
# coef_summary <- cbind(coef_summary, t(apply( coef_store , 2 , quantile , probs = c(0.01, 0.05, 0.95, 0.99) , na.rm = TRUE )))[,c(1, 3, 4, 2, 5, 6)]
# colnames(coef_summary)[c(1, 4)] <- c("Variable", "Mean")
# coef_summary
# 
# plot((rho_store), type = "l")
# plot((density(rho_store)))
# plot(coef_store[,1], type="l")
# plot(density(coef_store[,6]))


# calculate impacts -------------------------------------------------------

coef_store <- store$coef_store
nstore <- nrow(coef_store)

thinning <- 10
choef_thin <- coef_store[seq(1, nstore, by = thinning),]
rho_thin <- store$rho_store[seq(1, nstore, by = thinning),]

## pre-calculate (I-rhoW)^(-1) for all posterior rhos
AI_rhos <- list()
for (i_rho in seq_along(unique(rho_thin))){
  cat("*")
  AI <- Matrix::solve(Diagonal(N,1) - unique(rho_thin)[i_rho] * W_k); gc()
  AI_rhos[[as.character(unique(rho_thin)[i_rho])]] <- AI
}

### create store
direct_store <- matrix(NA, nrow = nrow(choef_thin), ncol = k_small)
indirect_store <- matrix(NA, nrow = nrow(choef_thin), ncol = k_small)
total_store <- matrix(NA, nrow = nrow(choef_thin), ncol = k_small)

### calculate impacts
for (i_coef in 2:(k_small+1)) {
  
  cat("\n", colnames(X)[i_coef])
  
  for(i_thin in 1:nrow(choef_thin)){
    cat("*")
    
    # find corresponding rho
    rho_select <- rho_thin[i_thin]
    
    # multiplier times coef, add up direct and indirect effect
    SW <- crossprod(as.matrix(AI), Diagonal(N,1) * choef_thin[i_thin, i_coef] + W_k * choef_thin[i_thin, k_small + i_coef]); gc() # add up effect
    
    direct_store[i_thin, i_coef-1] <- sum(diag(SW))/N
    total_store[i_thin, i_coef-1] <- sum(SW)/N
    indirect_store[i_thin, i_coef-1] <- total_store[i_thin, i_coef-1] - direct_store[i_thin, i_coef-1]
    
  }
  
}


store_impacts <- list(direct_store, total_store, indirect_store)
save(store_impacts, file = paste0("data/impact_estimates/draws/impacts_gdp_yearly_", Sys.time(), ".Rdata"))

# summarise impacts
impact_summary <- cbind(colnames(X)[c(2:(k_small+1))], 
                        colMeans(store_impacts[[1]]), # direct mean
                        colMeans(store_impacts[[3]])) # indirect, mean

impact_summary <- cbind(impact_summary, 
                        t(apply( store_impacts[[1]] , 2 , quantile , probs = c(0.01, 0.025, 0.05, 0.95, 0.975, 0.99) , na.rm = TRUE )), # direct CI
                        t(apply( store_impacts[[3]] , 2 , quantile , probs = c(0.01, 0.025, 0.05, 0.95, 0.975, 0.99) , na.rm = TRUE ))) # indirect CI


impact_summary <- impact_summary[,c(1, 4, 5, 6, 2, 7, 8, 9, 10, 11, 12, 3, 13, 14, 15)]

colnames(impact_summary) <- c("Variable",
                              "Direct 1%", "Direct 2.5%", "Direct 5%", "Direct Mean", "Direct 95%", "Direct 97.5%", "Direct 99%", 
                              "Indirect 1%", "Indirect 2.5%", "Indirect 5%", "Indirect Mean", "Indirect 95%", "Indirect 97.5%", "Indirect 99%")

impact_summary <- rbind(impact_summary, c(c("Dependent variable: 5-year avg. annual per capita GDP growth"), rep(NA, 14)))

rho_summary <- c(mean(store$rho_store), quantile(store$rho_store, probs = c(0.01, 0.025, 0.05, 0.95, 0.975, 0.99)) )
rho_summary <- rho_summary[c(2, 3, 4, 1, 5, 6, 7)]

impact_summary <- rbind(impact_summary, c("Rho:",  rho_summary, rep(NA, 7)))

# save as Excel
write.csv(impact_summary, file = paste0("data/impact_estimates/summaries/gdp_yearly_" , ntot, "draws_", Sys.time(), ".csv"), row.names = FALSE)


