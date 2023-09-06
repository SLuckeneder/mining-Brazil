
library(dplyr)
library(tidyr)
library(readxl)

# MapBiomas municipality statistics downloaded from https://brasil.mapbiomas.org/en/estatisticas/ (1 September 2023)

if (!dir.exists("data/raw/MapBiomas-v8")){dir.create("data/raw/MapBiomas-v8")}

if(!file.exists(paste0("data/intermediary/mapbiomas_v8_2000_2022.Rdata"))){
  
  # load selected municipalities from base municipality layers
  load("data/raw/geobr/selected_mun.Rdata")
  
  years <- c(2000:2020)
  window_yrs <- 5 # 5-year window for average land use change computations
  subs <- c("Forest Formation", "Forest Plantation", "Grassland", "Wetland", "Pasture", "Agriculture", "Urban Area")
  
  # download data (note that MapBiomas constantly updates versions, please contact the authors in case of unavailable collection 8)
  # https://brasil.mapbiomas.org/en/estatisticas/
  if (! file.exists("data/raw/MapBiomas-v8/MapBiomas_col8_municipalities.xlsx")){
    download.file("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/downloads/statistics/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx", 
                  destfile = "data/raw/MapBiomas-v8/MapBiomas_col8_municipalities.xlsx") 
  }
  

  # Land Use ----------------------------------------------------------------

  # read data
  LU_data_raw <- readxl::read_excel("data/raw/MapBiomas-v8/MapBiomas_col8_municipalities.xlsx", sheet = 2)

  # concordance for official municipality ID and MapBiomas feature ID (to match later with mining)
  mun_conc <- LU_data_raw %>% dplyr::select(feature_id, geocode) %>%
    dplyr::group_by(geocode) %>%
    dplyr::slice(1)
  
  # tidy
  LU_data <- LU_data_raw %>% 
    dplyr::select(-c(1:4, 6:9, 11:14)) %>%
    tidyr::gather(key = "year", value = "value", -geocode, -level_2) %>%
    dplyr::rename(LU_class = level_2) %>%
    dplyr::filter(year %in% years) %>%
    dplyr::group_by(geocode, year, LU_class) %>%
    dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()
  
  LU_data <- LU_data %>%
    dplyr::filter(LU_class != "Non Observed") %>% 
    tidyr::spread(key = LU_class, value = value) %>%
    dplyr::mutate(unid = paste(geocode, year, sep = "_"))
  
  LU_data <- LU_data %>% dplyr::left_join(mun_conc)
  
  
  # Land transition ---------------------------------------------------------
  
  # read data
  LUC_data_raw <- readxl::read_excel("data/raw/MapBiomas-v8/MapBiomas_col8_municipalities.xlsx", sheet = 5)
  
  # tidy
  LUC_data <- LUC_data_raw %>% dplyr::filter(from_level_1 != "Non Observed", to_level_1 != "Non Observed") %>%
    dplyr::select(-c(1:4, 6:13, 15:24, 26:56, 59:62, 64, 66:68, 72, 76, 77, 80:82, 85, 89, 91, 94)) %>%
    dplyr::filter(from_level_2 != to_level_2) %>%
    dplyr::mutate(LUC_1y = paste(from_level_2, to_level_2, sep = "_")) %>%
    dplyr::filter(from_level_2 %in% subs & to_level_2 %in% subs) %>%
    dplyr::select(-from_level_2, -to_level_2)
  
  LUC_data <- LUC_data %>%
    tidyr::gather(key = "year", value = "value", -geocode, -LUC_1y) %>%
    dplyr::mutate(year = substr(year, 1, 4),
                  unid = paste(geocode, year, sep = "_")) %>%
    dplyr::filter(year %in% years) %>%
    dplyr::select(-geocode, -year) %>%
    dplyr::group_by(unid, LUC_1y) %>%
    dplyr::summarise(value = sum(value)) %>%
    tidyr::spread(key = "LUC_1y", value = "value")
  
  # calculate percentages ---------------------------------------------------
  
  # total area
  areas <- LU_data_raw %>% 
    dplyr::select(geocode, `2020`) %>% 
    dplyr::group_by(geocode) %>% 
    dplyr::summarise(AREA = sum(`2020`, na.rm = TRUE))
  
  # merge area to LU data
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
    dplyr::group_by(geocode) %>%
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
    dplyr::group_by(geocode) %>%
    dplyr::mutate(forest_cover_change_inverse = lag(`Forest Formation`, 1) - `Forest Formation`) %>%
    dplyr::ungroup() 
  
  # calculate natural forest loss (i.e. only negative changes in forest cover)
  mapbiomas_data <- mapbiomas_data %>%
    dplyr::group_by(geocode) %>%
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
    dplyr::filter(geocode %in% selected_mun) %>%
    dplyr::rename(AREA_mun = AREA) %>%
    dplyr::mutate(AREA_mun = AREA_mun / 100) %>% # km2
    dplyr::mutate(natural_forest_loss_ha_km2 = natural_forest_loss / AREA_mun) # ha per km2 municipality area



  # Mining ------------------------------------------------------------------

  # download data (note that MapBiomas constantly updates versions, please contact the authors in case of unavailable collection 8)
  # https://brasil.mapbiomas.org/en/estatisticas/
  if (! file.exists("data/raw/MapBiomas-v8/MapBiomas_col8_mining.xlsx")){
    download.file("https://mapbiomas-br-site.s3.amazonaws.com/downloads/Estatisticas%20/Colecao_7_Mining_BR_UF_Biome_Mun_TI_SITE.xlsx", 
                  destfile = "data/raw/MapBiomas-v8/MapBiomas_col8_mining.xlsx")
  }
  
  # read data
  mining_data_raw <- readxl::read_excel("data/raw/MapBiomas-v8/MapBiomas_col8_mining.xlsx", sheet = 5)
  
  # tidy
  mining_data <- mining_data_raw %>% 
    tidyr::gather(key = "year", value = "value", 
                  -state, -municipality, -feature_id, -class_id, -group, -level_1, -level_2, -level_3) %>%
    dplyr::group_by(feature_id, year, level_1) %>%
    dplyr::mutate(value = sum(value, na.rm = TRUE)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unid = paste(feature_id, year, level_1, sep = "_")) %>%
    dplyr::group_by(unid) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = "level_1", value = "value") %>%
    dplyr::rename(mining_industrial = `2. Industrial`,
                  mining_garimpo = `1. Garimpo`)
  
  # sum over multiple observations per feature_id
  mining_data <- mining_data %>% dplyr::group_by(feature_id, year) %>%
    dplyr::mutate(mining_industrial = sum(mining_industrial, na.rm = T),
                  mining_garimpo = sum(mining_garimpo, na.rm = T)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # create municipality identifier and merge with LU data
  conc_LU <- LU_data_raw %>%
    dplyr::filter(level_2 == "Mining")
  mining <- data.frame("mining" = rowSums(conc_LU[15:51], na.rm = TRUE))
  conc_LU <- dplyr::bind_cols(conc_LU, mining) %>%
    dplyr::select(feature_id, geocode, municipality, state_acronym, mining) %>%
    dplyr::mutate(mining_key = paste(state_acronym, municipality, sep = "_")) %>%
    dplyr::group_by(mining_key) %>%
    dplyr::mutate(mining_sum = sum(mining, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::select(feature_id, geocode, municipality, mining_key)
  colnames(conc_LU) <- c("feature_id_LU", "geocode_LU", "municipality_LU", "mining_key")

  conc_mining <- mining_data %>%
    dplyr::select(state, municipality, feature_id, mining_garimpo, mining_industrial) %>%
    dplyr::mutate(mining_key = paste(state, municipality, sep = "_")) %>%
    dplyr::group_by(mining_key) %>%
    dplyr::mutate(mining = sum(mining_garimpo, mining_industrial, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::select(feature_id, mining_key)
  colnames(conc_mining) <- c("feature_id_MI", "mining_key")

  conc <- dplyr::left_join(conc_LU, conc_mining, by = "mining_key") %>%
    dplyr::filter(!is.na(feature_id_MI))
  
  mining_data <- mining_data %>% 
    dplyr::left_join(conc %>% dplyr::select(feature_id_MI, geocode_LU), by = c("feature_id" = "feature_id_MI")) %>%
    dplyr::rename(geocode = geocode_LU) %>%
    dplyr::mutate(geocode = as.character(geocode))

  # merge to base municipality layer 
  base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg") %>%
    dplyr::mutate(code_muni = as.character(code_muni))
  mining_data <- dplyr::left_join(base_mun, mining_data, by = c("code_muni" = "geocode")) %>%
    dplyr::select(-feature_id, -class_id, -group, -level_2, -level_3) %>%
    dplyr::mutate(unid = paste(code_muni, year, sep = "_"))
  
  # # map for a brief check
  # p_dat <- mining_data %>% dplyr::filter(year == 2020) %>% dplyr::mutate(mining = mining_industrial + mining_garimpo)
  # brks <- c(0, 10, 100, 1000, 10000, max(p_dat$mining, na.rm = T))
  # p_dat <- p_dat %>%
  #   dplyr::mutate(mining_2020_d = cut(x = p_dat$mining, breaks = brks, include.lowest = FALSE, dig.lab = 5)) %>%
  #   dplyr::mutate(mining_2020_d = ifelse(is.na(mining_2020_d), 0, as.character(mining_2020_d)))
  # p_dat %>%
  #   ggplot2::ggplot(aes(fill = mining_2020_d)) +
  #   ggplot2::geom_sf(lwd = 0.1) +
  #   ggplot2::scale_fill_manual(values = c(viridis::viridis(5, direction = -1), "white"),
  #                              labels = c("0 to 10", "10 to 100", "100 to 1,000", "1,000 to 10,000", "10,000 to 41,327", "No mining"))
  
  # make base_mun a panel and add mining data
  mine_mun_panel <- do.call("rbind", replicate(21, base_mun %>% sf::st_drop_geometry(), simplify = FALSE)) %>%
    dplyr::mutate(year = rep(c(2000:2020), each = nrow(base_mun))) %>%
    dplyr::mutate(unid = paste(code_muni, year, sep = "_")) %>%
    dplyr::left_join(mining_data %>% sf::st_drop_geometry() %>% dplyr::select(unid, mining_garimpo, mining_industrial), by = "unid") %>%
    dplyr::mutate(mining_garimpo = ifelse(is.na(mining_garimpo), 0, mining_garimpo)) %>%
    dplyr::mutate(mining_industrial = ifelse(is.na(mining_industrial), 0, mining_industrial)) %>%
    dplyr::mutate(mining = mining_industrial + mining_garimpo) %>%
    dplyr::select(-code_state, -unid)
  
  # save --------------------------------------------------------------------
  save(mapbiomas_data, file = paste0("data/intermediary/mapbiomas_v8_2000_2020.Rdata"))
  
  } else {
  load(paste0("data/intermediary/mapbiomas_v8_2000_2020.Rdata"))
}