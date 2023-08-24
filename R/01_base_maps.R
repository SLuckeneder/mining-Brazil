
library(geobr)
years <- c(2000, 2001, 2010, 2013:2020) # years for which geobr package provides maps

library(sf)
library(dplyr)
library(tidyr)

if (!dir.exists("data/raw/geobr")){dir.create("data/raw/geobr")}

# municipalities ----------------------------------------------------------

store_mun <- list()

for (yr in years){
  if(!file.exists(paste0("data/raw/geobr/base_mun_", yr, ".shp"))){
    base_mun <- geobr::read_municipality(code_muni = "all", year = yr, simplified = FALSE) 
    sf::st_write(base_mun, paste0("data/raw/geobr/base_mun_", yr, ".shp"))
    store_mun[[yr]] <- base_mun
  } else {
    base_mun <- sf::read_sf(paste0("data/raw/geobr/base_mun_", yr, ".shp"))
    store_mun[[yr]] <- base_mun
  }
  
}

store_mun <- store_mun[years]

# find out which municipalities did not change ----------------------------
# inspired by Resende at al. 2016

for(yr in seq_along(years)){
  store_mun[[yr]] <- store_mun[[yr]]  %>% dplyr::mutate(year = years[yr]) %>%
    dplyr::select(code_mn, name_mn, cod_stt, abbrv_s, geometry, year)
}

all_mun <- do.call(rbind,store_mun) %>% sf::st_drop_geometry(); rm(store_mun)

all_mun %>% dplyr::group_by(year) %>% dplyr::summarise(n = n())

selected_mun <- all_mun %>% dplyr::group_by(code_mn) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == 11) %>%
  dplyr::select(code_mn)
selected_mun <- as.character(selected_mun$code_mn)
save(selected_mun, file = "data/raw/geobr/selected_mun.Rdata")

# for maps etc: Brazil and states -----------------------------------------

for (yr in years){
  if(!file.exists(paste0("data/raw/geobr/base_nat_", yr, ".shp"))){
    base_nat <- geobr::read_country(year = yr, simplified = FALSE) 
    sf::st_write(base_nat, paste0("data/raw/geobr/base_nat_", yr, ".shp"))
  } else {
    base_nat <- sf::read_sf(paste0("data/raw/geobr/base_nat_", yr, ".shp"))
  }
  
  if(!file.exists(paste0("data/raw/geobr/base_sta_", yr, ".shp"))){
    base_sta <- geobr::read_state(year = yr, simplified = FALSE)
    sf::st_write(base_sta, paste0("data/raw/geobr/base_sta_", yr, ".shp"))
  } else {
    base_sta <- sf::read_sf(paste0("data/raw/geobr/base_sta_", yr, ".shp"))
  }
}





