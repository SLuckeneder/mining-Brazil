
library(dplyr)
library(cem)
library(fastDummies)

if (!dir.exists("data/cem")){dir.create("data/cem")}
if (!dir.exists("data/cem/cem_results")){dir.create("data/cem/cem_results")}


# 3-year growth rates -----------------------------------------------------

window_yrs <- 3 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2005 # set start year of panel, main model uses 2005
to_yr <- 2016 # set end year of panel minus window_yrs, main model uses 2020-5 = 2015
M <- read.csv2(file = "data/full_data_2005-2019_3y.csv", sep = ",", stringsAsFactors = FALSE) # read full data matrix
M <- M %>% dplyr::mutate_at(c(4:121), as.numeric)

# filter to selected years
M <- M %>% dplyr::filter(year %in% c(from_yr:to_yr))

# filter to balanced panel
check <- M %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == length(unique(M$year)))
M <- M %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)

# select all variables used in the modelling + others interesting for matching (cod_municipio_long, municipality area, absolute forest area)
M <- M %>%
  dplyr::select(gdp_capita_growth, 
                natural_forest_loss_ha_km2, natural_forest_loss,
                gdp_capita_log, pop_growth, pop_dens, educ,
                gva_agri_log, gva_indu_log, gva_serv_log,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                precip_norm, elevation, AREA_mun, Forest.Formation,
                year, cod_municipio_long, 
                mining_industrial, mining_garimpo, mining) %>%
  dplyr::mutate(mining_treatment = ifelse(mining > 0, TRUE, FALSE),
                mining_treatment_industrial = ifelse(mining_industrial > 0, TRUE, FALSE), 
                mining_treatment_garimpo = ifelse(mining_garimpo > 0, TRUE, FALSE))

# prepare selection of matching variables
match_on_baseline <- c("pop_dens", "share_Forest.Formation", "precip_norm", "elevation", "AREA_mun") # baseline

drop_but <- function(x, but) {
  names(x)[!grepl(paste0(but, collapse = "|"), names(x))]
}

# baseline ----------------------------------------------------------------

# perform matching for industrial mining
out_cem_industrial <- cem::cem(treatment = "mining_treatment_industrial", data = M,
                               drop = drop_but(M, match_on_baseline),
                               keep.all = TRUE)

M$w <- out_cem_industrial[["w"]]
M_matched_industrial <- M %>% dplyr::filter(w > 0) %>% dplyr::select(-w)

# perform matching for garimpo mining
out_cem_garimpo <- cem::cem(treatment = "mining_treatment_garimpo", data = M,
                            drop = drop_but(M, match_on_baseline),
                            keep.all = TRUE)

M$w <- out_cem_garimpo[["w"]]
M_matched_garimpo <- M %>% dplyr::filter(w > 0) %>% dplyr::select(-w)

# store matching results
write.csv(M_matched_industrial, file = paste0("data/cem/cem_results/cem_baseline_data_industrial_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)
write.csv(M_matched_garimpo, file = paste0("data/cem/cem_results/cem_baseline_data_garimpo_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)

# 3-year growth rates -----------------------------------------------------

window_yrs <- 7 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2005 # set start year of panel, main model uses 2005
to_yr <- 2013 # set end year of panel minus window_yrs, main model uses 2020-5 = 2015
M <- read.csv2(file = "data/full_data_2005-2020_7y.csv", sep = ",", stringsAsFactors = FALSE) # read full data matrix
M <- M %>% dplyr::mutate_at(c(4:121), as.numeric)

# filter to selected years
M <- M %>% dplyr::filter(year %in% c(from_yr:to_yr))

# filter to balanced panel
check <- M %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == length(unique(M$year)))
M <- M %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)

# select all variables used in the modelling + others interesting for matching (cod_municipio_long, municipality area, absolute forest area)
M <- M %>%
  dplyr::select(gdp_capita_growth, 
                natural_forest_loss_ha_km2, natural_forest_loss,
                gdp_capita_log, pop_growth, pop_dens, educ,
                gva_agri_log, gva_indu_log, gva_serv_log,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                precip_norm, elevation, AREA_mun, Forest.Formation,
                year, cod_municipio_long, 
                mining_industrial, mining_garimpo, mining) %>%
  dplyr::mutate(mining_treatment = ifelse(mining > 0, TRUE, FALSE),
                mining_treatment_industrial = ifelse(mining_industrial > 0, TRUE, FALSE), 
                mining_treatment_garimpo = ifelse(mining_garimpo > 0, TRUE, FALSE))

# prepare selection of matching variables
match_on_baseline <- c("pop_dens", "share_Forest.Formation", "precip_norm", "elevation", "AREA_mun") # baseline

drop_but <- function(x, but) {
  names(x)[!grepl(paste0(but, collapse = "|"), names(x))]
}

# baseline ----------------------------------------------------------------

# perform matching for industrial mining
out_cem_industrial <- cem::cem(treatment = "mining_treatment_industrial", data = M,
                               drop = drop_but(M, match_on_baseline),
                               keep.all = TRUE)

M$w <- out_cem_industrial[["w"]]
M_matched_industrial <- M %>% dplyr::filter(w > 0) %>% dplyr::select(-w)

# perform matching for garimpo mining
out_cem_garimpo <- cem::cem(treatment = "mining_treatment_garimpo", data = M,
                            drop = drop_but(M, match_on_baseline),
                            keep.all = TRUE)

M$w <- out_cem_garimpo[["w"]]
M_matched_garimpo <- M %>% dplyr::filter(w > 0) %>% dplyr::select(-w)

# store matching results
write.csv(M_matched_industrial, file = paste0("data/cem/cem_results/cem_baseline_data_industrial_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)
write.csv(M_matched_garimpo, file = paste0("data/cem/cem_results/cem_baseline_data_garimpo_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)


