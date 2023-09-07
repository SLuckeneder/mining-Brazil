

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

window_yrs <- 5 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2005 # set start year of panel, main model uses 2005
to_yr <- 2015 # set end year of panel minus window_yrs, main model uses 2020-5 = 2015
knn_set <- 5 # set k for k-nearest neighbours weights matrix, main model uses k=5 
if(! file.exists("data/full_data_2000-2020_5y.csv")){source("R/00_compile_data.R")}
M <- read.csv2(file = "data/full_data_2000-2020_5y.csv", sep = ",", stringsAsFactors = FALSE) # read full data matrix
M <- M %>% dplyr::mutate_at(c(4:121), as.numeric)

# filter to selected years (e.g. 2005-2015 covers data up to 2020 due to 5-year growth windows)
M <- M %>% dplyr::filter(year %in% c(from_yr:to_yr))

# filter to balanced panel
check <- M %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == length(unique(M$year)))
M <- M %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)

# check if there is any NA left in the panel
sum(is.na(M))

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

# spatial weights matrix --------------------------------------------------

if(! file.exists(paste0("./data/W/W_k", knn_set, ".RData"))){
  
  library(spdep)
  
  if(!file.exists(paste0("data/raw/geobr/base_mun_2015.gpkg"))){source("R/01_base_maps.R")}
  base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
  
  # subset to relevant municipalities and order in the same way as Y and X
  W_base <- base_mun %>%
    dplyr::filter(code_muni %in% unique(M$cod_municipio_long)) %>%
    dplyr::arrange(code_muni)
  
  # Create W matrix: neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html
  coords_sf <-  sf::st_coordinates(sf::st_centroid(W_base))
  W_k <- spdep::knearneigh(coords_sf, k = knn_set)
  knear_nb <- spdep::knn2nb(W_k)
  W_k <- spdep::nb2mat(knear_nb)
  
  save(W_k, file = paste0("./data/W/W_k", knn_set, ".RData"))
} else {load(paste0("./data/W/W_k", knn_set, ".RData"))}

WW <- kronecker(.sparseDiagonal(t), W_k)


# prepare model input -----------------------------------------------------


# prepare matrix with yearly mining dummy variables
library(fastDummies)
X_mining_industrial <- fastDummies::dummy_cols( M %>% dplyr::select(year), select_columns = "year") %>% dplyr::select(-year) *  M$mining_industrial
X_mining_industrial <- X_mining_industrial %>% dplyr::mutate_all(function(x) ifelse(x > 0, 1, 0))
colnames(X_mining_industrial) <- gsub("year", "mining_industrial", colnames(X_mining_industrial))
X_mining_garimpo <- fastDummies::dummy_cols( M %>% dplyr::select(year), select_columns = "year") %>% dplyr::select(-year) *  M$mining_garimpo
X_mining_garimpo <- X_mining_garimpo %>% dplyr::mutate_all(function(x) ifelse(x > 0, 1, 0))
colnames(X_mining_garimpo) <- gsub("year", "mining_garimpo", colnames(X_mining_garimpo))

# Economic growth model yearly (dependent is 5y average annual GDP per capita growth)
Y <- as.numeric(M$gdp_capita_growth)
X <- M %>%
  dplyr::select(gdp_capita_log, pop_growth, pop_dens, educ,
                gva_agri_log, gva_indu_log, gva_serv_log,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                precip_norm, elevation,
                year) %>%
  dplyr::bind_cols(X_mining_industrial) %>%
  dplyr::bind_cols(X_mining_garimpo) %>%
  as.matrix()
YX <- cbind(Y, X)
write.csv(YX, file = paste0("./data/model_input/YX_gdp_yearly_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)

# Economic growth model pooled (dependent is 5y average annual GDP per capita growth)
Y <- as.numeric(M$gdp_capita_growth)
X <- M %>%
  dplyr::select(gdp_capita_log, pop_growth, pop_dens, educ,
                gva_agri_log, gva_indu_log, gva_serv_log,
                mining_industrial, mining_garimpo,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                precip_norm, elevation,
                year) %>%
  dplyr::mutate(mining_industrial = ifelse(mining_industrial > 0, 1, 0),
                mining_garimpo = ifelse(mining_garimpo > 0, 1, 0)) %>%
  dplyr::mutate(mining_industrial_x_pre_2010 = ifelse(mining_industrial * year %in% c(2000:2009), 1, 0),
                mining_industrial_x_since_2010 = ifelse(mining_industrial * year > 2009, 1, 0)) %>%
  dplyr::mutate(mining_garimpo_x_pre_2010 = ifelse(mining_garimpo * year %in% c(2000:2009), 1, 0),
                mining_garimpo_x_since_2010 = ifelse(mining_garimpo * year > 2009, 1, 0)) %>%
  dplyr::select(-mining_industrial, -mining_garimpo) %>%
  as.matrix()
YX <- cbind(Y, X)
write.csv(YX, file = paste0("./data/model_input/YX_gdp_pooled_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)


# Relative forest loss model yearly (dependent is ha per km2 of forest in municipality)
Y <- as.numeric(M$natural_forest_loss_ha_km2)
X <- M %>% 
  dplyr::select(gdp_capita_growth,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log, mav_Grassland_Agriculture_log, mav_Grassland_Pasture_log, mav_Pasture_Agriculture_log, mav_Pasture_Grassland_log,
                precip_norm, elevation,
                year) %>%
  dplyr::bind_cols(X_mining_industrial) %>%
  dplyr::bind_cols(X_mining_garimpo) %>%
  as.matrix()
YX <- cbind(Y, X)
write.csv(YX, file = paste0("./data/model_input/YX_def_rel_yearly_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)

# Relative forest loss model pooled (dependent is ha per km2 of forest in municipality)
Y <- as.numeric(M$natural_forest_loss_ha_km2)
X <- M %>% 
  dplyr::select(gdp_capita_growth,
                mining_industrial, mining_garimpo,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log, mav_Grassland_Agriculture_log, mav_Grassland_Pasture_log, mav_Pasture_Agriculture_log, mav_Pasture_Grassland_log,
                precip_norm, elevation,
                year) %>%
  dplyr::mutate(mining_industrial = ifelse(mining_industrial > 0, 1, 0),
                mining_garimpo = ifelse(mining_garimpo > 0, 1, 0)) %>%
  dplyr::mutate(mining_industrial_x_pre_2010 = ifelse(mining_industrial * year %in% c(2000:2009), 1, 0),
                mining_industrial_x_since_2010 = ifelse(mining_industrial * year > 2009, 1, 0)) %>%
  dplyr::mutate(mining_garimpo_x_pre_2010 = ifelse(mining_garimpo * year %in% c(2000:2009), 1, 0),
                mining_garimpo_x_since_2010 = ifelse(mining_garimpo * year > 2009, 1, 0)) %>%
  dplyr::select(-mining_industrial, -mining_garimpo) %>%
  as.matrix()
YX <- cbind(Y, X)
write.csv(YX, file = paste0("./data/model_input/YX_def_rel_pooled_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)


# Absolute forest loss model yearly (dependent is annual deforestation absolute ha)
Y <- as.numeric(M$natural_forest_loss)
X <- M %>% 
  dplyr::select(gdp_capita_growth,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log, mav_Grassland_Agriculture_log, mav_Grassland_Pasture_log, mav_Pasture_Agriculture_log, mav_Pasture_Grassland_log,
                precip_norm, elevation,
                year) %>%
  dplyr::bind_cols(X_mining_industrial) %>%
  dplyr::bind_cols(X_mining_garimpo) %>%
  as.matrix()
YX <- cbind(Y, X)
write.csv(YX, file = paste0("./data/model_input/YX_def_abs_yearly_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)

# Absolute forest loss model pooled (dependent is annual deforestation absolute ha)
Y <- as.numeric(M$natural_forest_loss)
X <- M %>% 
  dplyr::select(gdp_capita_growth,
                mining_industrial, mining_garimpo,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log, mav_Grassland_Agriculture_log, mav_Grassland_Pasture_log, mav_Pasture_Agriculture_log, mav_Pasture_Grassland_log,
                precip_norm, elevation,
                year) %>%
  dplyr::mutate(mining_industrial = ifelse(mining_industrial > 0, 1, 0),
                mining_garimpo = ifelse(mining_garimpo > 0, 1, 0)) %>%
  dplyr::mutate(mining_industrial_x_pre_2010 = ifelse(mining_industrial * year %in% c(2000:2009), 1, 0),
                mining_industrial_x_since_2010 = ifelse(mining_industrial * year > 2009, 1, 0)) %>%
  dplyr::mutate(mining_garimpo_x_pre_2010 = ifelse(mining_garimpo * year %in% c(2000:2009), 1, 0),
                mining_garimpo_x_since_2010 = ifelse(mining_garimpo * year > 2009, 1, 0)) %>%
  dplyr::select(-mining_industrial, -mining_garimpo) %>%
  as.matrix()
YX <- cbind(Y, X)
write.csv(YX, file = paste0("./data/model_input/YX_def_abs_pooled_", from_yr, "-", to_yr, "_", window_yrs, "y", ".csv"), row.names = FALSE)

# run models --------------------------------------------------------------



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



