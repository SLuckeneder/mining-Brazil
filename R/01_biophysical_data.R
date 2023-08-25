
library(dplyr)
library(tidyr)
library(raster)
library(sf)
library(elevatr)
library(exactextractr)
library(stringi)
library(cruts)

if (!dir.exists("data/raw/elevatr")){dir.create("data/raw/elevatr", recursive = TRUE)}
if (!dir.exists("data/raw/cruTS")){dir.create("data/raw/cruTS", recursive = TRUE)}

# elevation ---------------------------------------------------------------

if(!file.exists("data/intermediary/elevation_mean.Rdata")){
  
  # load national and municipality base layer
  base_nat <- sf::read_sf("data/raw/geobr/base_nat_2015.gpkg")
  base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
  base_nat <- sf::st_transform(base_nat, crs = sf::st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  base_mun <- sf::st_transform(base_mun, crs = sf::st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  
  cat("downloading elevation data...")
  elevation_data <- elevatr::get_elev_raster(base_nat, z = 5)
  rr <- raster::crop(elevation_data, base_nat)
  cat("rasterToPolygons...")
  rp <- raster::rasterToPolygons(rr)
  rsf <- sf::st_as_sf(rp)
  colnames(rsf) <- c("value", "geometry")
  rm(rr, rp); gc()
  # plot(sf::st_centroid(rsf))
  
  # remove large ocean areas (< -500m) to reduce object size
  rsf <- rsf %>% dplyr::filter(value > -500)
  
  cat("aggregate to municipalities...")
  elev_dat <- sf::st_join(base_mun, rsf, join = st_intersects)
  colnames(elev_dat) <- c("code_mn", "name_mn", "cod_stt", "abbrv_s", "geometry", "value")
  rm(rsf); gc()
  
  elev_dat <- elev_dat %>% 
    sf::st_drop_geometry() %>%
    dplyr::group_by(code_mn) %>%
    dplyr::summarise(elevation = mean(value)) 
  
  # plot(dplyr::left_join(base_mun, elev_dat) %>% dplyr::select(elevation))
  
  save(elev_dat, file = "data/intermediary/elevation_mean.Rdata")
  cat("done. \n")
  
} else {
  load("data/intermediary/elevation_mean.Rdata")
}



# precipitation -----------------------------------------------------------

sf::sf_use_s2(FALSE) # turn off the s2 processing, otherwise error from intersecting geometries occurs from 2010 data

if(!file.exists("data/intermediary/cruTS_2000_2020.Rdata")){

  # download cruTS Dat.nc from https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/

  store <- list()
  years <- c(2000:2020)
  
  # Municipality maps available for: 2000 2001 2010 2013 2014 2015 2016 2017 2018 2019 2020
  years_mod <- c(2000, rep(2001, 9), rep(2010,3), 2013:2020)

  # load one example for crs
  temp <- cruts2raster("./data/raw/cruTS/cru_ts4.05.1991.2000.pre.dat.nc",
                       timeRange = c("1991-01-01","1992-01-01"),
                       type = "stack")

  cat("Processing cruTS...")
  for (yr in seq_along(years)){

    # Brazil base map
    base_mun <- sf::read_sf(paste0("data/raw/geobr/base_mun_", years_mod[yr], ".shp"))
    base_mun <- sf::st_transform(base_mun, crs = sf::st_crs(temp))

    cat("\n", years[yr], "...\n")
    if(years[yr] < 2001){
      yr_dat <- cruts2poly("./data/raw/cruTS/cru_ts4.05.1991.2000.pre.dat.nc",
                           poly = as(base_mun, "Spatial"),
                           timeRange = c(paste0(years[yr], "-01-01"), paste0(years[yr]+1, "-01-01")), na.rm = TRUE)
      yr_dat <- sf::st_as_sf(yr_dat) %>% dplyr::mutate(id = row_number()) %>%
        tidyr::gather(key = "month", value = "value",  -geometry, -id) %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(precip_average = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() # %>%
        # dplyr::mutate(precip_norm = (precip_average - mean(precip_average, na.rm = TRUE))/sd(precip_average, na.rm = TRUE))

    } else if(years[yr] < 2011) {
      yr_dat <- cruts2poly("./data/raw/cruTS/cru_ts4.05.2001.2010.pre.dat.nc",
                           poly = as(base_mun, "Spatial"),
                           timeRange = c(paste0(years[yr], "-01-01"), paste0(years[yr]+1, "-01-01")), na.rm = TRUE)
      
      yr_dat <- sf::st_as_sf(yr_dat) %>%
        dplyr::mutate(id = row_number()) %>%
        tidyr::gather(key = "month", value = "value",  -geometry, -id) %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(precip_average = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() # %>%
        # dplyr::mutate(precip_norm = (precip_average - mean(precip_average, na.rm = TRUE))/sd(precip_average, na.rm = TRUE))
      
    } else {
      yr_dat <- cruts2poly("./data/raw/cruTS/cru_ts4.05.2011.2020.pre.dat.nc",
                           poly = as(base_mun, "Spatial"),
                           timeRange = c(paste0(years[yr], "-01-01"), paste0(years[yr]+1, "-01-01")), na.rm = TRUE)
      yr_dat <- sf::st_as_sf(yr_dat) %>%
        dplyr::mutate(id = row_number()) %>%
        tidyr::gather(key = "month", value = "value",  -geometry, -id) %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(precip_average = mean(value, na.rm = TRUE)) %>%
        dplyr::ungroup() # %>%
        # dplyr::mutate(precip_norm = (precip_average - mean(precip_average, na.rm = TRUE))/sd(precip_average, na.rm = TRUE))
    }

    yr_dat <- sf::st_join(yr_dat, sf::st_centroid(base_mun) %>% dplyr::select(code_mn), join = st_nearest_feature) %>% dplyr::select(-id)
    yr_dat <- yr_dat %>% dplyr::filter(!is.na(code_mn))
    
    # take mean if multiple polygons assigned to same municipality
    # check <- yr_dat[duplicated(yr_dat$code_mn),]
    # check <- yr_dat %>% dplyr::filter(code_mn %in% unique(check$code_mn))
    yr_dat <- yr_dat %>% 
      sf::st_drop_geometry() %>%
      dplyr::group_by(code_mn) %>%
      dplyr::summarise(precip_average = mean(precip_average)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(precip_norm = (precip_average - mean(precip_average, na.rm = TRUE))/sd(precip_average, na.rm = TRUE))
    
    # store
    store[[years[yr]]] <- yr_dat

  }

  # tidy up and save
  store <- store[c(2000:2020)]

  for(yr in seq_along(years)){
    store[[yr]] <- store[[yr]]  %>% dplyr::mutate(year = years[yr])
  }

  cruTS_data <- do.call(rbind,store)
  summary(cruTS_data)

  save(cruTS_data, file = "data/intermediary/cruTS_2000_2020.Rdata")
  
  

} else {
  load("data/intermediary/cruTS_2000_2020.Rdata")
}

sf::sf_use_s2(TRUE)

# plot(cruTS_data %>% dplyr::filter(year == 2020) %>% dplyr::select(precip_norm))


