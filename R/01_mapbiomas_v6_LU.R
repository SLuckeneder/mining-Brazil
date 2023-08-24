
if (!dir.exists("data/raw/MapBiomas-v6")){dir.create("data/raw/MapBiomas-v6")}
# Add data:
# MapBiomas municipality statistics downloaded from https://mapbiomas.org/en/statistics (1 June 2022)

library(dplyr)
library(tidyr)
library(readxl)

if(!file.exists(paste0("data/intermediary/mapbiomas_v6_2000_2020.Rdata"))){
  
  # load selected municipalities from base municipality layers
  load("data/raw/geobr/selected_mun.Rdata")
  
  years <- c(2000:2020)
  window_yrs <- 5 # 5-year window for average land use change computations
  subs <- c("Forest Formation", "Forest Plantation", "Grassland", "Wetland", "Pasture", "Agriculture", "Urban Area")
  
  # Land cover --------------------------------------------------------------

  # read data
  LU_data_raw <- readxl::read_excel("data/raw/MapBiomas-v6/1-ESTATISTICAS_MapBiomas_COL6.0_UF-MUNICIPIOS_v12_SITE.xlsx", sheet = 3)
  
  # tidy
  LU_data <- LU_data_raw %>% 
    dplyr::select(-c(1, 2, 4:8, 10:13)) %>%
    tidyr::gather(key = "year", value = "value", -geo_code, -level_2) %>%
    dplyr::rename(LU_class = level_2) %>%
    dplyr::filter(year %in% years) %>%
    dplyr::group_by(geo_code, year, LU_class) %>%
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()
  
  LU_data <- LU_data %>%
    dplyr::filter(LU_class != "Non Observed") %>% 
    tidyr::spread(key = LU_class, value = value) %>%
    dplyr::mutate(unid = paste(geo_code, year, sep = "_"))
  
  
  # Land transition ---------------------------------------------------------

  # read data
  LUC_data_raw <- readxl::read_excel("data/raw/MapBiomas-v6/1-ESTATISTICAS_MapBiomas_COL6.0_UF-MUNICIPIOS_v12_SITE.xlsx", sheet = 4)
  
  # tidy
  LUC_data <- LUC_data_raw %>% dplyr::filter(from_level_1 != "Non Observed", to_level_1 != "Non Observed") %>%
    dplyr::select(-c(1, 2, 4:8, 10:16, 18:48, 42:44, 46, 48, 50:52, 54, 56, 60, 64, 65, 68:70, 73, 77)) %>%
    dplyr::filter(from_level_2 != to_level_2) %>%
    dplyr::mutate(LUC_1y = paste(from_level_2, to_level_2, sep = "_")) %>%
    dplyr::filter(from_level_2 %in% subs & to_level_2 %in% subs) %>%
    dplyr::select(-from_level_2, -to_level_2)
  
  LUC_data <- LUC_data %>%
    tidyr::gather(key = "year", value = "value", -geo_code, -LUC_1y) %>%
    dplyr::mutate(year = substr(year, 1, 4),
                  unid = paste(geo_code, year, sep = "_")) %>%
    dplyr::select(-geo_code, -year) %>%
    dplyr::group_by(unid, LUC_1y) %>%
    dplyr::summarise(value = sum(value)) %>%
    tidyr::spread(key = "LUC_1y", value = "value")
  
  # calculate percentages ---------------------------------------------------
  
  # total area
  areas <- LU_data_raw %>% 
    dplyr::select(geo_code, `2020`) %>% 
    dplyr::group_by(geo_code) %>% 
    dplyr::summarise(AREA = sum(`2020`, na.rm = TRUE))
  
  # merge area to LUC data
  LU_data <- dplyr::left_join(LU_data, areas); rm(areas)
  
  # calculate shares
  LU_data <- LU_data %>% dplyr::mutate(share_Agriculture = Agriculture / AREA,
                                       share_Grassland = Grassland / AREA,
                                       `share_Forest Plantation` = `Forest Plantation` / AREA,
                                       `share_Forest Formation` = `Forest Formation` / AREA,
                                       share_Pasture = Pasture / AREA)
  
  
  # merge LU and LUC --------------------------------------------------------
  
  mapbiomas_data <- dplyr::full_join(LU_data, LUC_data, by = "unid") %>%
    dplyr::select(-unid)
  mapbiomas_data[is.na(mapbiomas_data)] <- 0
  
  
  # calculate LUC -----------------------------------------------------------
  
  # average change in ha over the 5-year growth window and log thereof
  mav <- function(x,n){ stats::filter(x, rep(1/n,n), sides=1)} 
  
  mapbiomas_data <- mapbiomas_data %>% 
    dplyr::group_by(geo_code) %>%
    dplyr::mutate(mav_Agriculture_Forest_Plantation = mav(`Agriculture_Forest Plantation`, window_yrs),
                  mav_Agriculture_Forest_Plantation = lead(mav_Agriculture_Forest_Plantation, window_yrs),
                  mav_Agriculture_Forest_Plantation_log = log(mav_Agriculture_Forest_Plantation), 
                  
                  mav_Agriculture_Pasture = mav(Agriculture_Pasture, window_yrs),
                  mav_Agriculture_Pasture = lead(mav_Agriculture_Pasture, window_yrs),
                  mav_Agriculture_Pasture_log = log(mav_Agriculture_Pasture), 
                  
                  mav_Agriculture_Grassland = mav(Agriculture_Grassland, window_yrs),
                  mav_Agriculture_Grassland = lead(mav_Agriculture_Grassland, window_yrs),
                  mav_Agriculture_Grassland_log = log(mav_Agriculture_Grassland), 
                  
                  mav_Forest_Plantation_Agriculture = mav(`Forest Plantation_Agriculture`, window_yrs),
                  mav_Forest_Plantation_Agriculture = lead(mav_Forest_Plantation_Agriculture, window_yrs),
                  mav_Forest_Plantation_Agriculture_log = log(mav_Forest_Plantation_Agriculture), 
                  
                  mav_Forest_Plantation_Grassland = mav(`Forest Plantation_Grassland`, window_yrs),
                  mav_Forest_Plantation_Grassland = lead(mav_Forest_Plantation_Grassland, window_yrs),
                  mav_Forest_Plantation_Grassland_log = log(mav_Forest_Plantation_Grassland), 
                  
                  mav_Forest_Plantation_Pasture = mav(`Forest Plantation_Pasture`, window_yrs),
                  mav_Forest_Plantation_Pasture = lead(mav_Forest_Plantation_Pasture, window_yrs),
                  mav_Forest_Plantation_Pasture_log = log(mav_Forest_Plantation_Pasture), 
                  
                  mav_Grassland_Agriculture = mav(Grassland_Agriculture, window_yrs),
                  mav_Grassland_Agriculture = lead(mav_Grassland_Agriculture, window_yrs),
                  mav_Grassland_Agriculture_log = log(mav_Grassland_Agriculture),
                  
                  mav_Grassland_Forest_Plantation = mav(`Grassland_Forest Plantation`, window_yrs),
                  mav_Grassland_Forest_Plantation = lead(mav_Grassland_Forest_Plantation, window_yrs),
                  mav_Grassland_Forest_Plantation_log = log(mav_Grassland_Forest_Plantation),
                  
                  mav_Grassland_Pasture = mav(Grassland_Pasture, window_yrs),
                  mav_Grassland_Pasture = lead(mav_Grassland_Pasture, window_yrs),
                  mav_Grassland_Pasture_log = log(mav_Grassland_Pasture),
                  
                  mav_Forest_Formation_Agriculture = mav(`Forest Formation_Agriculture`, window_yrs),
                  mav_Forest_Formation_Agriculture = lead(mav_Forest_Formation_Agriculture, window_yrs),
                  mav_Forest_Formation_Agriculture_log = log(mav_Forest_Formation_Agriculture),
                  
                  mav_Forest_Formation_Forest_Plantation = mav(`Forest Formation_Forest Plantation`, window_yrs),
                  mav_Forest_Formation_Forest_Plantation = lead(mav_Forest_Formation_Forest_Plantation, window_yrs),
                  mav_Forest_Formation_Forest_Plantation_log = log(mav_Forest_Formation_Forest_Plantation),
                  
                  mav_Forest_Formation_Grassland = mav(`Forest Formation_Grassland`, window_yrs),
                  mav_Forest_Formation_Grassland = lead(mav_Forest_Formation_Grassland, window_yrs),
                  mav_Forest_Formation_Grassland_log = log(mav_Forest_Formation_Grassland), 
                  
                  mav_Forest_Formation_Pasture = mav(`Forest Formation_Pasture`, window_yrs),
                  mav_Forest_Formation_Pasture = lead(mav_Forest_Formation_Pasture, window_yrs),
                  mav_Forest_Formation_Pasture_log = log(mav_Forest_Formation_Pasture), 
                  
                  mav_Pasture_Agriculture = mav(Pasture_Agriculture, window_yrs),
                  mav_Pasture_Agriculture = lead(mav_Pasture_Agriculture, window_yrs),
                  mav_Pasture_Agriculture_log = log(mav_Pasture_Agriculture), 
                  
                  mav_Pasture_Grassland = mav(Pasture_Grassland, window_yrs),
                  mav_Pasture_Grassland = lead(mav_Pasture_Grassland, window_yrs),
                  mav_Pasture_Grassland_log = log(mav_Pasture_Grassland), 
                  
                  mav_Pasture_Forest_Plantation = mav(`Pasture_Forest Plantation`, window_yrs),
                  mav_Pasture_Forest_Plantation = lead(mav_Pasture_Forest_Plantation, window_yrs),
                  mav_Pasture_Forest_Plantation_log = log(mav_Pasture_Forest_Plantation) 
                  
    ) %>%
    dplyr::ungroup()
  
  # set -inf log(LUC) to 0 (resulting from LUC = 0)
  mapbiomas_data <- mapbiomas_data %>% dplyr::mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
  
  # calculate change in forest cover
  mapbiomas_data <- mapbiomas_data %>%
    dplyr::group_by(geo_code) %>%
    dplyr::mutate(forest_cover_change_inverse = lag(`Forest Formation`, 1) - `Forest Formation`) %>%
    dplyr::ungroup() 
  
  # calculate natural forest loss (i.e. only negative changes in forest cover)
  mapbiomas_data <- mapbiomas_data %>%
    dplyr::group_by(geo_code) %>%
    dplyr::mutate(natural_forest_loss = ifelse(forest_cover_change_inverse > 0, forest_cover_change_inverse, 0)) %>%
    dplyr::ungroup()
  mapbiomas_data <- mapbiomas_data %>%
    dplyr::mutate(natural_forest_loss_relative = natural_forest_loss / lag(`Forest Formation`, 1) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year != 2000) # initial year is NA when computing changes
  
  # NA in natural_forest_loss_relative (resulting from 0 forest cover) to 0
  mapbiomas_data <- mapbiomas_data %>%
    dplyr::mutate(natural_forest_loss_relative = ifelse(is.na(natural_forest_loss_relative), 0, natural_forest_loss_relative))
  
  # View(mapbiomas_data %>%
  #   dplyr::select(geo_code, year, `Forest Formation`, natural_forest_loss, natural_forest_loss_relative))
  
  # filter to selected municipalities, transform area to km2, calculate ha deforestation per km2 municipality area
  mapbiomas_data <- mapbiomas_data %>% 
    dplyr::filter(geo_code %in% selected_mun) %>%
    dplyr::rename(AREA_mun = AREA) %>%
    dplyr::mutate(AREA_mun = AREA_mun / 100) %>% # km2
    dplyr::mutate(natural_forest_loss_ha_km2 = natural_forest_loss / AREA_mun) # ha per km2 municipality area
  
  # save --------------------------------------------------------------------
  save(mapbiomas_data, file = paste0("data/intermediary/mapbiomas_v6_2000_2020.Rdata"))
  
} else {
  load(paste0("data/intermediary/mapbiomas_v6_2000_2020.Rdata"))
}
