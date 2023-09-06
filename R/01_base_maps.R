
library(geobr)
years <- c(2000, 2001, 2010, 2013:2020) # years for which geobr package provides maps

library(sf)
library(dplyr)
library(tidyr)

if (!dir.exists("data/raw/geobr")){dir.create("data/raw/geobr", recursive = TRUE)}

if (!file.exists("data/raw/geobr/selected_mun.Rdata")){
  
  # municipalities ----------------------------------------------------------
  
  store_mun <- list()
  
  for (yr in years){
    file_path <- paste0("data/raw/geobr/base_mun_", yr, ".gpkg")
    if(!file.exists(file_path)){
      base_mun <- geobr::read_municipality(code_muni = "all", year = yr, simplified = FALSE)
      sf::st_write(base_mun, file_path)
      store_mun[[yr]] <- base_mun
    } else {
      base_mun <- sf::read_sf(file_path)
      store_mun[[yr]] <- base_mun
    }
  }
  
  store_mun <- store_mun[years]
  
  # find out which municipalities did not change ----------------------------
  # inspired by Resende at al. 2016
  
  for(yr in seq_along(years)){
    store_mun[[yr]] <- store_mun[[yr]]  %>% dplyr::mutate(year = years[yr]) %>%
      dplyr::select(code_muni, name_muni, code_state, abbrev_state, geom, year)
  }
  
  all_mun <- do.call(rbind,store_mun) %>% sf::st_drop_geometry(); rm(store_mun)
  
  all_mun %>% dplyr::group_by(year) %>% dplyr::summarise(n = n())
  
  selected_mun <- all_mun %>% dplyr::group_by(code_muni) %>% dplyr::summarise(n = n()) %>%
    dplyr::filter(n == 11) %>%
    dplyr::select(code_muni)
  selected_mun <- as.character(selected_mun$code_muni)
  save(selected_mun, file = "data/raw/geobr/selected_mun.Rdata")
  
}

# for maps etc: Brazil and states -----------------------------------------

for (yr in years){
  file_path <- paste0("data/raw/geobr/base_nat_", yr, ".gpkg")
  if(!file.exists(file_path)){
    base_nat <- geobr::read_country(year = yr, simplified = FALSE)
    sf::st_write(base_nat, file_path)
  } 
  file_path <- paste0("data/raw/geobr/base_sta_", yr, ".gpkg")
  if(!file.exists(file_path)){
    base_sta <- geobr::read_state(year = yr, simplified = FALSE)
    sf::st_write(base_sta, file_path)
  } 
}
