
# set window for calculation of average annual growth rates, main model uses 5 years, alternatives are 3 and 7 years
window_yrs_alternatives <- c(3, 7) 

# set end year of panel minus window_yrs, main model uses 2020-5 = 2015, alternatives are 2016 (2017 not possible due to missing education data) and 2013
to_yrs <- c(2016, 2013)

mining_types <- c("industrial", "garimpo")
matching_model <- "baseline"
dependent <- "gdp"

# prepare data ------------------------------------------------------------

for(mining_type in mining_types){
  
  for (i in seq_along(window_yrs_alternatives)){
    
    window_yrs <- window_yrs_alternatives[i]
    to_yr <- to_yrs[i]
    from_yr <- 2005 # set start year of panel
    knn_set <- 5 # set k for k-nearest neighbours weights matrix
    
    # read matched data
    M_matched_industrial <- read.csv(paste0("data/cem/cem_results/cem_", matching_model, "_data_industrial_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"))
    M_matched_garimpo <- read.csv(paste0("data/cem/cem_results/cem_", matching_model, "_data_garimpo_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"))
    
    # count municipalities for mention in manuscript text
    length(unique(M_matched_industrial$cod_municipio_long))
    length(unique(M_matched_garimpo$cod_municipio_long))
    
    # filter to balanced panels
    check <- M_matched_industrial %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
      dplyr::filter(n == length(unique(M_matched_industrial$year)))
    M_matched_industrial <- M_matched_industrial %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)
    
    check <- M_matched_garimpo %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
      dplyr::filter(n == length(unique(M_matched_garimpo$year)))
    M_matched_garimpo <- M_matched_garimpo %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)
    
    # check if there is any NA left in the panel
    sum(is.na(M_matched_industrial))
    sum(is.na(M_matched_garimpo))
    
    
    # sampler setup
    ntot <- 2000
    nburn <- 1000
    nretain = ntot - nburn
    t <- length(c(from_yr:to_yr))
    
    source("./R/21_lndetPaceBarry.R")
    
    # inverse gamma prior for sigma
    a_pr <- 0.01
    b_pr <- 0.01
    
    # rho prior is beta, with
    rho_a = 1.01
    
    beta_prob = function(rho,a) 1/beta(a,a) * ((1+rho)^(a-1) *(1-rho)^(a-1))/(2^(2*a - 1))
    
    
    
    # spatial weights matrix --------------------------------------------------
    
    if (mining_type == "industrial"){
      
      if(! file.exists(paste0("./data/W/W_k", knn_set, "_matching_", matching_model, "_industrial_", window_yrs, "y.RData"))){
        
        if(!file.exists(paste0("data/raw/geobr/base_mun_2015.gpkg"))){source("R/01_base_maps.R")}
        base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
        
        # subset to relevant municipalities and order in the same way as Y and X
        W_base <- base_mun %>%
          dplyr::filter(code_muni %in% unique(M_matched_industrial$cod_municipio_long)) %>%
          dplyr::arrange(code_muni)
        
        # Create W matrix: neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html
        coords_sf <-  sf::st_coordinates(sf::st_centroid(W_base))
        W_k <- spdep::knearneigh(coords_sf, k = knn_set)
        knear_nb <- spdep::knn2nb(W_k)
        W_k <- spdep::nb2mat(knear_nb)
        
        save(W_k, file = paste0("./data/W/W_k", knn_set, "_matching_", matching_model, "_industrial_", window_yrs, "y.RData"))
      } else {load(paste0("./data/W/W_k", knn_set, "_matching_", matching_model, "_industrial_", window_yrs, "y.RData"))}
      
    } else {
      
      if(! file.exists(paste0("./data/W/W_k", knn_set, "_matching_", matching_model, "_garimpo_", window_yrs, "y.RData"))){
        
        if(!file.exists(paste0("data/raw/geobr/base_mun_2015.gpkg"))){source("R/01_base_maps.R")}
        base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
        
        # subset to relevant municipalities and order in the same way as Y and X
        W_base <- base_mun %>%
          dplyr::filter(code_muni %in% unique(M_matched_garimpo$cod_municipio_long)) %>%
          dplyr::arrange(code_muni)
        
        # Create W matrix: neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html
        coords_sf <-  sf::st_coordinates(sf::st_centroid(W_base))
        W_k <- spdep::knearneigh(coords_sf, k = knn_set)
        knear_nb <- spdep::knn2nb(W_k)
        W_k <- spdep::nb2mat(knear_nb)
        
        save(W_k, file = paste0("./data/W/W_k", knn_set, "_matching_", matching_model, "_garimpo_", window_yrs, "y.RData"))
      } else {load(paste0("./data/W/W_k", knn_set, "_matching_", matching_model, "_garimpo_", window_yrs, "y.RData"))}
      
    }
    
    WW <- kronecker(.sparseDiagonal(t), W_k)
    
    
    # prepare model input -----------------------------------------------------
    
    if (mining_type == "industrial"){
      
      # Economic growth model yearly (dependent is 5y average annual GDP
      Y <- as.numeric(M_matched_industrial$gdp_capita_growth)
      X <- M_matched_industrial %>%
        dplyr::select(gdp_capita_log, pop_growth, pop_dens, educ,
                      gva_agri_log, gva_indu_log, gva_serv_log,
                      share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                      mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                      mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                      mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                      mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                      mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                      precip_norm, elevation,
                      mining_industrial,
                      year) %>%
        dplyr::mutate(mining_industrial = ifelse(mining_industrial > 0, 1, 0)) %>%
        dplyr::mutate(mining_industrial_x_pre_2010 = ifelse(mining_industrial * year %in% c(2000:2009), 1, 0),
                      mining_industrial_x_since_2010 = ifelse(mining_industrial * year > 2009, 1, 0)) %>%
        dplyr::select(-mining_industrial) %>%
        as.matrix()
      YX <- cbind(Y, X)
      write.csv(YX, file = paste0("./data/cem/cem_model_input/YX_gdp_pooled_industrial_", matching_model, "_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)
      
      
    } else {
      
      X_mining_garimpo <- fastDummies::dummy_cols( M_matched_garimpo %>% dplyr::select(year), select_columns = "year") %>% dplyr::select(-year) *  M_matched_garimpo$mining_garimpo
      X_mining_garimpo <- X_mining_garimpo %>% dplyr::mutate_all(function(x) ifelse(x > 0, 1, 0))
      colnames(X_mining_garimpo) <- gsub("year", "mining_garimpo", colnames(X_mining_garimpo))
      
      # Economic growth model yearly (dependent is 5y average annual GDP
      Y <- as.numeric(M_matched_garimpo$gdp_capita_growth)
      X <- M_matched_garimpo %>%
        dplyr::select(gdp_capita_log, pop_growth, pop_dens, educ,
                      gva_agri_log, gva_indu_log, gva_serv_log,
                      share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                      mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                      mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                      mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                      mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                      mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                      precip_norm, elevation,
                      mining_garimpo,
                      year) %>%
        dplyr::mutate(mining_garimpo = ifelse(mining_garimpo > 0, 1, 0)) %>%
        dplyr::mutate(mining_garimpo_x_pre_2010 = ifelse(mining_garimpo * year %in% c(2000:2009), 1, 0),
                      mining_garimpo_x_since_2010 = ifelse(mining_garimpo * year > 2009, 1, 0)) %>%
        dplyr::select(-mining_garimpo) %>%
        as.matrix()
      YX <- cbind(Y, X)
      write.csv(YX, file = paste0("./data/cem/cem_model_input/YX_gdp_pooled_garimpo_", matching_model, "_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)
      
      
    }
    
    
    
    
    # prepare estimation ------------------------------------------------------
    
    # read data
    if(mining_type == "industrial"){
      YX <- read.csv2(file = paste0("./data/cem/cem_model_input/YX_gdp_pooled_industrial_", matching_model, "_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), sep = ",", stringsAsFactors = FALSE) %>% mutate_all(as.numeric)
    } else {
      YX <- read.csv2(file = paste0("./data/cem/cem_model_input/YX_gdp_pooled_garimpo_", matching_model, "_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), sep = ",", stringsAsFactors = FALSE) %>% mutate_all(as.numeric)
    }
    
    Y <- YX %>% dplyr::select(1) %>% as.matrix()
    X <- YX %>% dplyr::select(-1) %>% as.matrix()
    
    
    # make dummies out of year column and then remove this column 
    if(mining_type == "industrial"){
      D <- fastDummies::dummy_cols( M_matched_industrial %>% dplyr::select(year), select_columns = "year")[,-c(1:2)]  %>% as.matrix()
    } else {
      D <- fastDummies::dummy_cols( M_matched_garimpo %>% dplyr::select(year), select_columns = "year")[,-c(1:2)]  %>% as.matrix()
    }
    
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
    
    save(store, file = paste0("data/intermediary/MCMC_draws_matching/SI/", dependent, "_pooled_", mining_type, "_", matching_model, "_", Sys.time(), ".Rdata"))
    
    
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
    save(store_impacts, file = paste0("data/cem/sdm_impact_estimates/draws/SI/impacts_", dependent, "_pooled_", mining_type, "_", matching_model, "_", Sys.time(), ".Rdata"))
    
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
    
    impact_summary <- rbind(impact_summary, c(c(dependent), rep(NA, 14)))
    
    rho_summary <- c(mean(store$rho_store), quantile(store$rho_store, probs = c(0.01, 0.025, 0.05, 0.95, 0.975, 0.99)) )
    rho_summary <- rho_summary[c(2, 3, 4, 1, 5, 6, 7)]
    
    impact_summary <- rbind(impact_summary, c("Rho:",  rho_summary, rep(NA, 7)))
    
    # save as Excel
    write.csv(impact_summary, file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", dependent, "_pooled_", mining_type, "_", window_yrs, "y_window_" , ntot, "draws_", Sys.time(), ".csv"), row.names = FALSE)
    
  }
}

