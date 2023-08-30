

library(readxl)
library(dplyr)
library(tidyr)

if (!dir.exists("data/raw/firjan")){dir.create("data/raw/firjan")}
# add data from https://www.firjan.com.br/ifdm/downloads/ (1 June 2022)
firjan_url <- "https://www.firjan.com.br/data/files/5B/F3/48/87/F1B8461049FF6646A8A809C2/Evolu__o%20do%20IFDM%20Educa__o%20-%202005%20a%202016.xlsx"
download.file(firjan_url, destfile = "data/raw/firjan/firjan_educ_2005_2016.xlsx")

# load selected municipalities from base municipality layers
load("data/raw/geobr/selected_mun.Rdata")

firjan_file <- "data/intermediary/firjan_educ_2005_2016.Rdata"
if(!file.exists(firjan_file)){
  
  # read data ---------------------------------------------------------------
  
  firjan_educ <- readxl::read_excel("data/raw/firjan/firjan_educ_2005_2016.xlsx", skip = 2) %>%
    dplyr::select(c(1:4, seq(5, 27, 2))) %>%
    `colnames<-`(c(c("cod_municipio_long", "region", "codigo_uf", "nome_munic"), c(2005:2016))) %>%
    dplyr::filter(!is.na(cod_municipio_long))
  
  
  # tidy --------------------------------------------------------------------
  
  firjan_educ <- firjan_educ %>% 
    tidyr::gather(key = "year", value = "educ", -cod_municipio_long, -region, -codigo_uf, -nome_munic) %>%
    dplyr::filter(educ != "*")
  
  # filter to selected municipalities
  firjan_educ <- firjan_educ %>% 
    dplyr::mutate(unid = paste0(cod_municipio_long, "_", year)) %>%
    dplyr::filter(cod_municipio_long %in% substr(selected_mun, 1, 6)) %>%
    dplyr::mutate(educ = as.numeric(educ))
  
  save(firjan_educ, file = firjan_file)
  
} else {
  load(firjan_file)
}
