
library(dplyr)
library(tidyr)
library(terra)
library(sf)
library(stars)
library(elevatr)
library(exactextractr)
library(stringi)
library(cruts)
library(rvest)
library(R.utils)

if (!dir.exists("data/raw/elevatr")){dir.create("data/raw/elevatr", recursive = TRUE)}
if (!dir.exists("data/raw/cruTS")){dir.create("data/raw/cruTS", recursive = TRUE)}

# elevation ---------------------------------------------------------------

elevation_file <- "data/intermediary/elevation_mean.Rdata"
dir.create(dirname(elevation_file), recursive = TRUE, showWarnings = FALSE)
if(!file.exists(elevation_file)){
  
  # load national and municipality base layer
  base_nat <- sf::read_sf("data/raw/geobr/base_nat_2015.gpkg")
  base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
  base_nat <- sf::st_transform(base_nat, crs = sf::st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  base_mun <- sf::st_transform(base_mun, crs = sf::st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  
  cat("downloading elevation data...")
  elevation_data <- elevatr::get_elev_raster(base_nat, z = 5)
  cat("aggregating elevation to municipality level by mean...")
  elevation_mean <- exactextractr::exact_extract(elevation_data, base_mun, 'mean', progress = TRUE)
  
  elev_dat <- mutate(base_mun, elevation = elevation_mean)
  save(elev_dat, file = elevation_file)
  cat("done. \n")
  
} else {
  load(elevation_file)
}



# precipitation -----------------------------------------------------------

sf::sf_use_s2(FALSE) # turn off the s2 processing, otherwise error from intersecting geometries occurs from 2010 data

prec_file <- "data/intermediary/cruTS_2000_2020.Rdata"
dir.create(dirname(prec_file), recursive = TRUE, showWarnings = FALSE)
if(!file.exists(prec_file)){

  # download cruTS Dat.nc from https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/
  #Grab filenames from separate URL
  cru_url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/"
  helplinks <- read_html(cru_url) %>% html_nodes("a") %>% html_text(trim = T)
  helplinks <- helplinks[grepl(".nc.gz", helplinks)]
  helplinks <- helplinks[grepl("1991|2001|2011", helplinks)]
  for (f in helplinks) {
    cru_file <- paste0("./data/raw/cruTS/", f, sep = "")
    dir.create(dirname(cru_file), recursive = TRUE, showWarnings = FALSE)
    if(!file.exists(stri_replace_all(cru_file, fixed = ".gz", replacement = ""))){
      options(timeout = 500)
      download.file(paste(cru_url, f, sep = ""), cru_file)
      gunzip(cru_file)
      options(timeout = 60)
    }
  }

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
    base_mun <- sf::read_sf(paste0("data/raw/geobr/base_mun_", years_mod[yr], ".gpkg"))
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

    yr_dat <- sf::st_join(yr_dat, sf::st_centroid(base_mun) %>% dplyr::select(code_muni), join = st_nearest_feature) %>% dplyr::select(-id)
    yr_dat <- yr_dat %>% dplyr::filter(!is.na(code_muni))
    
    # take mean if multiple polygons assigned to same municipality
    # check <- yr_dat[duplicated(yr_dat$code_muni),]
    # check <- yr_dat %>% dplyr::filter(code_muni %in% unique(check$code_muni))
    yr_dat <- yr_dat %>% 
      sf::st_drop_geometry() %>%
      dplyr::group_by(code_muni) %>%
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

  save(cruTS_data, file = prec_file)
  
  

} else {
  load(prec_file)
}

sf::sf_use_s2(TRUE)

# plot(cruTS_data %>% dplyr::filter(year == 2020) %>% dplyr::select(precip_norm))


