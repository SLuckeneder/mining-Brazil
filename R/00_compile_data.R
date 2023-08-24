
# NOTE: Users can move directly to the models (10_run_spatial_models.R and herein sourced 11_lndetPaceBarry.R and 12_run_*.R), 
# as 10_run_spatial_models.R will automatically download ready-to-use datasets from ZENODO-LINK-HERE and store it in ./model_input and ./W.

window_yrs <- 5 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2005
to_yr <- 2015
knn_set <- 5 # set k for k-nearest neighbours weights matrix, main model uses k=5 

if (!dir.exists("data")){dir.create("data")}
if (!dir.exists("data/raw")){dir.create("data/raw")}
if (!dir.exists("data/intermediary")){dir.create("data/intermediary")}
if (!dir.exists("data/model_input")){dir.create("data/model_input")}
if (!dir.exists("data/W")){dir.create("data/W")}

# load data ---------------------------------------------------------------

source("R/01_base_maps.R") # spatial polygons for Brazil, states and municipalities

source("R/01_mapbiomas_v6_mining.R") # mine data MapBiomas area in ha

source("R/01_mapbiomas_v6_LU.R") # land cover and change

source("R/01_biophysical_data.R") # biophysical characteristics: Elevation, precipitation

source("R/01_ibge_population.R") # population in thousand

source("R/01_ibge_econ.R") # GVA in agriculture, GVA in industry, GVA in services, GVA in public sector, GVA total, GDP total, GDP per capita

source("R/01_firjan_data.R") # Education score

# prepare for merging the data --------------------------------------------

# create matching IDs
mine_mun_panel <- mine_mun_panel %>% 
  dplyr::rename(cod_municipio_long = code_mn) %>%
  dplyr::mutate(cod_municipio = substr(cod_municipio_long, 3, 7)) %>%
  dplyr::mutate(unid = paste0(cod_municipio_long, "_", year))

mapbiomas_data <- mapbiomas_data %>% 
  dplyr::mutate(unid = paste0(geo_code, "_", year))

cruTS_data <- cruTS_data %>% 
  dplyr::mutate(unid = paste0(code_mn, "_", year)) %>%
  dplyr::filter(code_mn %in% selected_mun)

elev_dat <- elev_dat %>%
  dplyr::mutate(code_mn = as.character(code_mn)) %>% 
  dplyr::filter(code_mn %in% selected_mun)

# merge data --------------------------------------------------------------

M <- ibge_pop %>% 
  dplyr::left_join(ibge_econ %>% dplyr::select(-year, -cod_municipio, -cod_municipio_long, -tax), by = "unid") %>%
  dplyr::left_join(mine_mun_panel %>% dplyr::select(unid, mining_industrial, mining_garimpo, mining), by = "unid") %>%
  dplyr::left_join(mapbiomas_data %>% dplyr::select(-year, -geo_code), by = "unid") %>%
  dplyr::left_join(cruTS_data %>% dplyr::select(-year, -code_mn), by = "unid") %>%
  dplyr::left_join(elev_dat, by = c("cod_municipio_long" = "code_mn"))

# add FIRJAN educ data, which has no 7th digit in ID (which is not a problem, because IDs are still unique)
M <- M %>% dplyr::mutate("code_mn_6" = as.numeric(substr(cod_municipio_long, 1, 6))) %>%
  dplyr::mutate(unid = paste(code_mn_6, year, sep = "_")) %>%
  dplyr::left_join(firjan_educ %>% dplyr::select(unid, educ), by = "unid")

# arrange and clean data ---------------------------------------------------

# arrange by year and municipality ID
M <- M %>% dplyr::arrange(year, cod_municipio_long) %>%
  dplyr::filter(year %in% c(from_yr:(to_yr+window_yrs))) %>%
  dplyr::select(-codigo_uf, -cod_municipio, -nome_munic, -unid, -cod_reg, -name_reg, -cod_uf, -name_uf, -code_mn_6) %>%
  dplyr::select(3, 5, 4, 1, 2, 35, 110, 6:34, 36:109)

# remove 1 municipality where there is no landcover data
check <- M %>% dplyr::filter(is.na(Agriculture))
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# deal with negative values in GDP and GVA reporting, which may result as statistical artifacts from imputation: remove from sample
# negative values in GDP (1 municipality: Guamaré 2012) 
check <- M %>% dplyr::select(cod_municipio_long, name_munic, year, gdp_total, gdp_capita, gva_agri, gva_indu) %>%
  dplyr::filter(gdp_total < 0)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))
# negative values in GVA agriculture (1 municipality: Taquaral 2015)
check <- M %>% dplyr::select(cod_municipio_long, name_munic, year, gdp_total, gdp_capita, gva_agri, gva_indu) %>%
  dplyr::filter(gva_agri < 0)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))
# negative values in GVA industry (31 municipalities)
check <- M %>% dplyr::select(cod_municipio_long, name_munic, year, gdp_total, gdp_capita, gva_agri, gva_indu) %>%
  dplyr::filter(gva_indu < 0)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))
rm(check)

# missing precipitation data (35 municipalities)
check <- M %>%  dplyr::filter(year <= to_yr, is.na(precip_average))
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# missing education data (6 municipalities)
check <- M %>%  dplyr::filter(year <= to_yr, is.na(educ))
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# missing population data (2 municipalities)
check <- M %>%  dplyr::filter(year <= to_yr, is.na(pop))
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# calculate additional variables ------------------------------------------

# population density
M <- M %>%
  dplyr::mutate(pop_dens = pop / AREA_mun) # thousand per km2

# growth rates of gdp per capita and population
M <- M %>% 
  dplyr::group_by(cod_municipio_long) %>%
  dplyr::mutate(gdp_capita_growth = log(gdp_capita / lag(gdp_capita, window_yrs)) / window_yrs * 100) %>%
  dplyr::mutate(gdp_capita_growth = lead(gdp_capita_growth, window_yrs)) %>%
  dplyr::mutate(gdp_total_growth = log(gdp_total / lag(gdp_total, window_yrs)) / window_yrs * 100) %>%
  dplyr::mutate(gdp_total_growth = lead(gdp_total_growth, window_yrs)) %>%
  dplyr::mutate(pop_growth = log(pop / lag(pop, window_yrs)) / window_yrs * 100) %>%
  dplyr::mutate(pop_growth = lead(pop_growth, window_yrs)) %>%
  dplyr::ungroup()

# take logs (change form GVA agriculure = 0 to 1 to enable taking logs)
M <- M %>% 
  dplyr::mutate(gva_agri = ifelse(gva_agri == 0, 1, gva_agri)) %>%
  dplyr::mutate(gdp_total_log = log(gdp_total),
                gdp_capita_log = log(gdp_capita),
                gva_agri_log = log(gva_agri),
                gva_indu_log = log(gva_indu),
                gva_serv_log = log(gva_serv))

# re-order columns
M <- M %>% dplyr::select(1:4, 6, 5, 114, 111:113, 7:14, 115:119, 15:110)

# filter to selected years (e.g. 2005-2015 covers data up to 2020 due to 5-year growth windows)
M <- M %>% dplyr::filter(year %in% c(from_yr:to_yr))

# filter to balanced panel
check <- M %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == length(unique(M$year)))
M <- M %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)

# also filter out remaining NAs in population growth count (29 municipalities)
check <- M %>%  dplyr::filter(is.na(pop_growth))
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# check if there is any NA left in the panel
sum(is.na(M))



# save full data matrix ---------------------------------------------------

summary(M)
length(unique(M$cod_municipio_long)) # municipalities
nrow(M) # observations

write.csv(M, file = paste0("data/full_data_", from_yr, "-", to_yr, "_", window_yrs, "y.csv"), row.names = FALSE)


# save model inputs -------------------------------------------------------

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
                share_Agriculture, `share_Forest Formation`, `share_Forest Plantation`, share_Grassland, share_Pasture,
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
                share_Agriculture, `share_Forest Formation`, `share_Forest Plantation`, share_Grassland, share_Pasture,
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
                share_Agriculture, `share_Forest Formation`, `share_Forest Plantation`, share_Grassland, share_Pasture,
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
                share_Agriculture, `share_Forest Formation`, `share_Forest Plantation`, share_Grassland, share_Pasture,
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
                share_Agriculture, `share_Forest Formation`, `share_Forest Plantation`, share_Grassland, share_Pasture,
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
                share_Agriculture, `share_Forest Formation`, `share_Forest Plantation`, share_Grassland, share_Pasture,
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


# spatial weights matrix --------------------------------------------------

if(! file.exists(paste0("./data/W/W_k", knn_set, ".RData"))){
  
  library(spdep)
  
  # subset to relevant municipalities and order in the same way as Y and X
  W_base <- base_mun %>%
    dplyr::filter(code_mn %in% unique(M$cod_municipio_long)) %>%
    dplyr::arrange(code_mn)
  
  # Create W matrix: neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html
  coords_sf <-  sf::st_coordinates(sf::st_centroid(W_base))
  W_k <- spdep::knearneigh(coords_sf, k = knn_set)
  knear_nb <- spdep::knn2nb(W_k)
  W_k <- spdep::nb2mat(knear_nb)
  
  save(W_k, file = paste0("./data/W/W_k", knn_set, ".RData"))
}
  

