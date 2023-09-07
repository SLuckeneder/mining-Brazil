
# NOTE: Users can move directly to the models (10_run_spatial_models.R and herein sourced 11_lndetPaceBarry.R and 12_run_*.R), 
# as 10_run_spatial_models.R will automatically download ready-to-use dataset from GitHub

window_yrs <- 5 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2000
to_yr <- 2015
knn_set <- 5 # set k for k-nearest neighbours weights matrix, main model uses k=5 

if (!dir.exists("data")){dir.create("data")}
if (!dir.exists("data/raw")){dir.create("data/raw")}
if (!dir.exists("data/intermediary")){dir.create("data/intermediary")}
if (!dir.exists("data/model_input")){dir.create("data/model_input")}
if (!dir.exists("data/W")){dir.create("data/W")}

# load data ---------------------------------------------------------------

source("R/01_base_maps.R") # spatial polygons for Brazil, states and municipalities

source("R/01_mapbiomas_v8.R") # MapBiomas Collection 8 land cover, merge land cover and change dataset with mining dataset

source("R/01_biophysical_data.R") # biophysical characteristics: Elevation, precipitation

source("R/01_ibge_population.R") # population in thousand

source("R/01_ibge_econ.R") # GVA in agriculture, GVA in industry, GVA in services, GVA in public sector, GVA total, GDP total, GDP per capita

source("R/01_firjan_data.R") # Education score

source("R/01_ibge_amazon.R") # Legal Amazon borders for Figure 1

source("R/01_worldbank_data.R") # WB GDP deflator and commodity prices for Figure 2

# prepare for merging the data --------------------------------------------

# # create matching IDs
mapbiomas_data <- mapbiomas_data %>% 
  dplyr::mutate(unid = paste0(geocode, "_", year))

cruTS_data <- cruTS_data %>% 
  dplyr::mutate(unid = paste0(code_muni, "_", year)) %>%
  dplyr::filter(code_muni %in% selected_mun)

elev_dat <- elev_dat %>%
  dplyr::mutate(code_muni = as.character(code_muni)) %>% 
  dplyr::filter(code_muni %in% selected_mun)

# merge data --------------------------------------------------------------

M <- ibge_pop %>% 
  dplyr::left_join(ibge_econ %>% dplyr::select(-year, -cod_municipio, -cod_municipio_long, -tax), by = "unid") %>%
  dplyr::left_join(mapbiomas_data %>% dplyr::select(-year, -geocode), by = "unid") %>%
  dplyr::left_join(cruTS_data %>% dplyr::select(-year, -code_muni), by = "unid") %>%
  dplyr::left_join(elev_dat, by = c("cod_municipio_long" = "code_muni"))

# add FIRJAN educ data, which has no 7th digit in ID (which is not a problem, because IDs are still unique)
M <- M %>% dplyr::mutate("code_muni_6" = as.numeric(substr(cod_municipio_long, 1, 6))) %>%
  dplyr::mutate(unid = paste(code_muni_6, year, sep = "_")) %>%
  dplyr::left_join(firjan_educ %>% dplyr::select(unid, educ), by = "unid")

# arrange and clean data ---------------------------------------------------

# arrange by year and municipality ID
M <- M %>% dplyr::arrange(year, cod_municipio_long) %>%
  dplyr::filter(year %in% c(from_yr:(to_yr+window_yrs))) %>%
  dplyr::select(-codigo_uf, -cod_municipio, -nome_munic, -unid, -cod_reg, -name_reg, -cod_uf, -name_uf, -code_muni_6) # %>%
  # dplyr::select(3, 5, 4, 1, 2, 35, 110, 6:34, 36:109)

# remove 1 municipality where there is no landcover data
check <- M %>% dplyr::filter(year >= 2005, is.na(Agriculture))
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# deal with negative values in GDP and GVA reporting, which may result as statistical artifacts from imputation: remove from sample
# negative values in GDP (1 municipality: Guamaré 2012) 
check <- M %>% dplyr::select(cod_municipio_long, name_munic, year, gdp_total, gdp_capita, gva_agri, gva_indu) %>%
  dplyr::filter(year >= 2005, gdp_total < 0)
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))
# negative values in GVA agriculture (1 municipality: Taquaral 2015)
check <- M %>% dplyr::select(cod_municipio_long, name_munic, year, gdp_total, gdp_capita, gva_agri, gva_indu) %>%
  dplyr::filter(year >= 2005, gva_agri < 0)
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))
# negative values in GVA industry (31 municipalities)
check <- M %>% dplyr::select(cod_municipio_long, name_munic, year, gdp_total, gdp_capita, gva_agri, gva_indu) %>%
  dplyr::filter(year >= 2005, gva_indu < 0)
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))
rm(check)

# missing precipitation data (42 municipalities)
check <- M %>%  dplyr::filter(year >= 2005, year <= to_yr, is.na(precip_average))
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# missing education data (141 municipalities)
check <- M %>%  dplyr::filter(year >= 2005, year <= to_yr, is.na(educ))
unique(check$cod_municipio_long)
M <- M %>% dplyr::filter(! cod_municipio_long %in% unique(check$cod_municipio_long))

# missing population data (27 municipalities)
check <- M %>%  dplyr::filter(year >= 2005, year <= to_yr, is.na(pop))
unique(check$cod_municipio_long)
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
M <- M %>% dplyr::select(3, 5, 4, 1, 35, 2, 117, 114:116, 113, 6:12, 118:122, 107:109, 13:33, 36:106, 110:112)

# save full data matrix ---------------------------------------------------

summary(M)
write.csv(M, file = paste0("data/full_data_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)



  

