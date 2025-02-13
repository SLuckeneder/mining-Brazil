
library(dplyr)
library(xtable)

window_yrs <- 5 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2005 # set start year of panel, main model uses 2005
to_yr <- 2015 # set end year of panel minus window_yrs, main model uses 2020-5 = 2015

matching_model <- "baseline"


# load matches ------------------------------------------------------------

# baseline matching
M_matched_industrial <- read.csv(paste0("data/cem/cem_results/cem_baseline_data_industrial_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"))
M_matched_garimpo <- read.csv(paste0("data/cem/cem_results/cem_baseline_data_garimpo_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"))

# industrial mining -------------------------------------------------------

# split into mining and non-mining
M_mining_industrial <- M_matched_industrial %>% dplyr::filter(mining_treatment_industrial == TRUE) %>% 
  dplyr::select(-mining_treatment, -mining_treatment_industrial, -mining_treatment_garimpo, -year)
M_non_mining_industrial <- M_matched_industrial %>% dplyr::filter(mining_treatment_industrial == FALSE) %>% 
  dplyr::select(-mining_treatment, -mining_treatment_industrial, -mining_treatment_garimpo, -year)

n_mining_industrial <- nrow(M_mining_industrial)
n_non_mining_industrial <- nrow(M_non_mining_industrial)
n_mining_industrial + n_non_mining_industrial

# take averages and merge
M_industrial_means <- dplyr::bind_rows(
  round(M_mining_industrial %>% dplyr::summarise_all(mean), 3),
  round(M_non_mining_industrial %>% dplyr::summarise_all(mean), 3)
)
M_industrial_means$n <- c(n_mining_industrial, n_non_mining_industrial)
rownames(M_industrial_means) <- c("mining_industrial",  "non_mining_industrial")

# select interesting variables
M_industrial_means <- M_industrial_means %>% dplyr::select(
  gdp_capita_log, pop_growth, pop_dens, educ,
  gva_agri_log, gva_indu_log, gva_serv_log,
  share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
  precip_norm, elevation, AREA_mun,
  gdp_capita_growth, natural_forest_loss_ha_km2, natural_forest_loss,
  mining_industrial, mining_garimpo, mining, n)

M_industrial_means <- t(M_industrial_means)

# garimpo mining ----------------------------------------------------------

M_mining_garimpo <- M_matched_garimpo %>% dplyr::filter(mining_treatment_garimpo == TRUE) %>% 
  dplyr::select(-mining_treatment, -mining_treatment_industrial, -mining_treatment_garimpo, -year)
M_non_mining_garimpo <- M_matched_garimpo %>% dplyr::filter(mining_treatment_garimpo == FALSE) %>% 
  dplyr::select(-mining_treatment, -mining_treatment_industrial, -mining_treatment_garimpo, -year)

n_mining_garimpo <- nrow(M_mining_garimpo)
n_non_mining_garimpo <- nrow(M_non_mining_garimpo)
n_mining_garimpo + n_non_mining_garimpo

# take averages and merge
M_garimpo_means <- dplyr::bind_rows(
  round(M_mining_garimpo %>% dplyr::summarise_all(mean), 3),
  round(M_non_mining_garimpo %>% dplyr::summarise_all(mean), 3)
)
M_garimpo_means$n <- c(n_mining_garimpo, n_non_mining_garimpo)
rownames(M_garimpo_means) <- c("mining_garimpo",  "non_mining_garimpo")

# select interesting variables
M_garimpo_means <- M_garimpo_means %>% dplyr::select(
  gdp_capita_log, pop_growth, pop_dens, educ,
  gva_agri_log, gva_indu_log, gva_serv_log,
  share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
  precip_norm, elevation, AREA_mun,
  gdp_capita_growth, natural_forest_loss_ha_km2, natural_forest_loss,
  mining_industrial, mining_garimpo, mining, n)

M_garimpo_means <- t(M_garimpo_means)


# load original data ------------------------------------------------------

M <- read.csv2(file = "data/full_data_2000-2020_5y.csv", sep = ",", stringsAsFactors = FALSE) # read full data matrix
M <- M %>% dplyr::mutate_at(c(4:121), as.numeric)

# filter to selected years (e.g. 2005-2015 covers data up to 2020 due to 5-year growth windows)
M <- M %>% dplyr::filter(year %in% c(from_yr:to_yr))

# filter to balanced panel
check <- M %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == length(unique(M$year)))
M <- M %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)

# select interesting variables
M <- M %>% dplyr::select(
  gdp_capita_log, pop_growth, pop_dens, educ,
  gva_agri_log, gva_indu_log, gva_serv_log,
  share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
  precip_norm, elevation, AREA_mun,
  gdp_capita_growth, natural_forest_loss_ha_km2, natural_forest_loss,
  mining_industrial, mining_garimpo, mining)

# split into mining and non-mining
M_mining <- M %>% dplyr::filter(mining > 0) #%>% dplyr::select(-mining, mining_industrial, mining_garimpo)
M_mining_industrial <- M %>% dplyr::filter(mining_industrial > 0) #%>% dplyr::select(-mining, mining_industrial, mining_garimpo)
M_mining_garimpo <- M %>% dplyr::filter(mining_garimpo > 0) #%>% dplyr::select(-mining, mining_industrial, mining_garimpo)
M_non_mining <- M %>% dplyr::filter(mining == 0) #%>% dplyr::select(-mining, mining_industrial, mining_garimpo)

n_mining <- nrow(M_mining)
n_mining_industrial <- nrow(M_mining_industrial)
n_mining_garimpo <- nrow(M_mining_garimpo)
n_non_mining <- nrow(M_non_mining)
n_mining + n_non_mining


# take averages and merge
M_means <- dplyr::bind_rows(
  round(M_mining %>% dplyr::summarise_all(mean), 3),
  round(M_mining_industrial %>% dplyr::summarise_all(mean), 3),
  round(M_mining_garimpo %>% dplyr::summarise_all(mean), 3),
  round(M_non_mining %>% dplyr::summarise_all(mean), 3)
)
M_means$n <- c(n_mining, n_mining_industrial, n_mining_garimpo, n_non_mining)
rownames(M_means) <- c("orig_mining", "orig_mining_industrial", "orig_mining_garimpo", "orig_non_mining")
M_means <- t(M_means)


# merge -------------------------------------------------------------------

tab <- dplyr::bind_cols(
  Variable = rownames(M_means),
  M_means,
  M_industrial_means,
  M_garimpo_means
)

# latex -------------------------------------------------------------------

# build concordance for variable names
raw_name <- c("mining_industrial_2005","mining_industrial_2006","mining_industrial_2007","mining_industrial_2008","mining_industrial_2009","mining_industrial_2010",
              "mining_industrial_2011","mining_industrial_2012","mining_industrial_2013","mining_industrial_2014","mining_industrial_2015",
              "mining_industrial_x_pre_2010","mining_industrial_x_since_2010",
              "mining_garimpo_2005","mining_garimpo_2006","mining_garimpo_2007","mining_garimpo_2008","mining_garimpo_2009","mining_garimpo_2010",
              "mining_garimpo_2011","mining_garimpo_2012","mining_garimpo_2013","mining_garimpo_2014","mining_garimpo_2015",
              "mining_garimpo_x_pre_2010","mining_garimpo_x_since_2010",
              "mav_Agriculture_Forest_Plantation_log","mav_Agriculture_Grassland_log","mav_Agriculture_Pasture_log","mav_Natural_Forest_Agriculture_log",
              "mav_Natural_Forest_Forest_Plantation_log","mav_Natural_Forest_Grassland_log","mav_Natural_Forest_Pasture_log","mav_Forest_Formation_Agriculture_log",
              "mav_Forest_Formation_Forest_Plantation_log","mav_Forest_Formation_Grassland_log","mav_Forest_Formation_Pasture_log","mav_Forest_Plantation_Agriculture_log","mav_Forest_Plantation_Grassland_log",
              "mav_Forest_Plantation_Pasture_log","mav_Grassland_Agriculture_log","mav_Grassland_Forest_Plantation_log","mav_Grassland_Pasture_log","mav_Pasture_Agriculture_log",
              "mav_Pasture_Forest_Plantation_log","mav_Pasture_Grassland_log",
              "share_Agriculture","share_Natural.Forest","share_Forest.Formation","share_Forest.Plantation","share_Grassland","share_Pasture",
              "gdp_capita_log","educ","pop_growth","pop_dens","gva_agri_log","gva_indu_log","gva_serv_log", "gdp_capita_growth", "precip_norm","elevation",
              "Rho:", "Observations",
              "AREA_mun", "natural_forest_loss_ha_km2", "natural_forest_loss", "mining_industrial", "mining_garimpo", "mining", "n")
long_name <- c("Industrial mining 2005", "Industrial mining 2006", "Industrial mining 2007", "Industrial mining 2008", "Industrial mining 2009", "Industrial mining 2010", 
               "Industrial mining 2011", "Industrial mining 2012", "Industrial mining 2013", "Industrial mining 2014", "Industrial mining 2015", 
               "Industrial mining $\\times$ pre 2010", "Industrial mining $\\times$ since 2010", 
               "Garimpo mining 2005", "Garimpo mining 2006", "Garimpo mining 2007", "Garimpo mining 2008", "Garimpo mining 2009", "Garimpo mining 2010", 
               "Garimpo mining 2011", "Garimpo mining 2012", "Garimpo mining 2013", "Garimpo mining 2014", "Garimpo mining 2015", 
               "Garimpo mining $\\times$ pre 2010", "Garimpo mining $\\times$ since 2010",
               "LUC$^{Agriculture, Forest\ Plantation}$","LUC$^{Agriculture, Grassland}$","LUC$^{Agriculture, Pasture}$", "LUC$^{Natural\ Forest, Agriculture}$","LUC$^{Natural\ Forest, Forest\ Plantation}$",
               "LUC$^{Natural\ Forest, Grassland}$","LUC$^{Natural\ Forest, Pasture}$", "LUC$^{Natural\ Forest, Agriculture}$",
               "LUC$^{Natural\ Forest, Forest\ Plantation}$","LUC$^{Natural\ Forest, Grassland}$","LUC$^{Natural\ Forest, Pasture}$", "LUC$^{Forest\ Plantation, Agriculture}$","LUC$^{Forest\ Plantation, Grassland}$",
               "LUC$^{Forest\ Plantation, Pasture}$","LUC$^{Grassland, Agriculture}$","LUC$^{Grassland, Forest\ Plantation}$","LUC$^{Grassland, Pasture}$",
               "LUC$^{Pasture, Agriculture}$","LUC$^{Pasture, Forest\ Plantation}$","LUC$^{Pasture, Grassland}$",
               "Initial agriculture","Initial natural forest","Initial natural forest","Initial forest plantation","Initial grassland","Initial pasture","Initial income",
               "Human capital","Population growth","Population density","GVA agriculture","GVA industry","GVA services", "GPD growth", "Precipitation","Elevation",
               "$\\rho$", "Observations",
               "Area", "Forest loss (rel)", "Forest loss (abs)", "Area$_{ind}$", "Area$_{gar}$", "Area_{mining}", "Observations")
concordance <- data.frame(raw_name, long_name)


colnames(tab) <- c("raw_name", 
                   "Mining", "Mining$_{ind}$ ", "Mining$_{gar}$ ", "Non-mining", 
                   "Mining$_{ind}$", "Non-mining$_{ind}$", 
                   "Mining$_{gar}$", "Non-mining$_{gar}$")

tab <- tab %>% dplyr::filter(! raw_name %in% c("mining_industrial", "mining_garimpo", "mining", "natural_forest_loss_ha_km2", "natural_forest_loss", "gdp_capita_growth"))

tab <- concordance %>% dplyr::right_join(tab) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename("Variable" = "long_name")


colnames(tab) <- c("Variable", 
                   "Mining", "Ind. mining", "Gar. mining", "Non-mining", 
                   "Ind. mining ", "Non-mining ",
                   "Gar. mining ", " Non-mining ")
print(xtable::xtable(tab %>% dplyr::select(-2), 
                     digits = 2,
                     caption = "\\textbf{Caption title.} Caption."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)




