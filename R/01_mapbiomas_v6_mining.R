
if (!dir.exists("data/raw/MapBiomas-v6")){dir.create("data/raw/MapBiomas-v6")}
# Add data:
# MapBiomas municipality statistics downloaded from https://mapbiomas.org/en/statistics (1 June 2022)

library(sf)
library(dplyr)

if(!file.exists("data/intermediary/mapbiomas_mine_data.Rdata")){
  
  # load selected municipalities from base municipality layers
  load("data/raw/geobr/selected_mun.Rdata")
  
  # read data
  mining_data_raw <- readxl::read_excel("data/raw/MapBiomas-v6/Colecao_6_Mining_BR_UF_Biome_Mun_TI_SITE.xlsx", sheet = 6) %>%
    dplyr::select(-category)
  
  # read land use data for municipality codes
  conc <- readxl::read_excel("data/raw/MapBiomas-v6/1-ESTATISTICAS_MapBiomas_COL6.0_UF-MUNICIPIOS_v12_SITE.xlsx", sheet = 3) %>%
    dplyr::select(3:4) %>%
    dplyr::group_by(feature_id) %>%
    dplyr::slice(1)
  
  # tidy
  mining_data <- mining_data_raw %>% 
    tidyr::gather(key = "year", value = "value", 
                  -state, -city, -feature_id, -class_id, -group, -level_1, -level_2, -level_3) %>%
    dplyr::group_by(feature_id, year, level_1) %>%
    dplyr::filter(level_1 != "3. Outros") %>%
    dplyr::mutate(value = sum(value, na.rm = TRUE)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unid = paste(feature_id, year, level_1, sep = "_")) %>%
    dplyr::group_by(unid) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = "level_1", value = "value") %>%
    dplyr::rename(mining_industrial = `2. Industrial`,
                  mining_garimpo = `1. Garimpo`) %>%
    dplyr::left_join(conc, by = "feature_id")
  
  mining_data <- mining_data %>% dplyr::group_by(feature_id, year) %>%
    dplyr::mutate(mining_industrial = sum(mining_industrial, na.rm = T),
                  mining_garimpo = sum(mining_garimpo, na.rm = T)) %>%
    dplyr::slice(1)
  
  # check <- mining_data %>% dplyr::filter(year == 2010) %>%
  #   dplyr::summarise(sum_garimpo = sum(mining_garimpo, na.rm = T), sum_industrial = sum(mining_industrial, na.rm = T)) %>%
  #   dplyr::mutate(total = sum_garimpo + sum_industrial)
  # sum(check$total)
  
  # merge to spatial data 
  base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.shp") %>%
    dplyr::mutate(code_mn = as.character(code_mn))
  mining_data <- dplyr::left_join(base_mun, mining_data, by = c("code_mn" = "geo_code")) %>%
    dplyr::select(-feature_id, -class_id, -group, -level_2, -level_3) %>%
    dplyr::mutate(unid = paste(code_mn, year, sep = "_"))
  
  # make base_mun a panel and add mining data
  mine_mun_panel <- do.call("rbind", replicate(21, base_mun %>% sf::st_drop_geometry(), simplify = FALSE)) %>%
    dplyr::mutate(year = rep(c(2000:2020), each = nrow(base_mun))) %>%
    dplyr::mutate(unid = paste(code_mn, year, sep = "_")) %>%
    dplyr::left_join(mining_data %>% sf::st_drop_geometry() %>% dplyr::select(unid, mining_garimpo, mining_industrial), by = "unid") %>%
    dplyr::mutate(mining_garimpo = ifelse(is.na(mining_garimpo), 0, mining_garimpo)) %>%
    dplyr::mutate(mining_industrial = ifelse(is.na(mining_industrial), 0, mining_industrial)) %>%
    dplyr::mutate(mining = mining_industrial + mining_garimpo) %>%
    dplyr::select(-cod_stt, -unid)
  
  
  # save --------------------------------------------------------------------
  
  save(mine_mun_panel, file = "data/intermediary/mapbiomas_mine_data.Rdata")
  
} else{load("data/intermediary/mapbiomas_mine_data.Rdata")}
