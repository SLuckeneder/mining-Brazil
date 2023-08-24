

library(readxl)
library(dplyr)
library(tidyr)

if (!dir.exists("data/raw/firjan")){dir.create("data/raw/firjan")}
# add data from https://www.firjan.com.br/ifdm/downloads/ (1 June 2022)

# load selected municipalities from base municipality layers
load("data/raw/geobr/selected_mun.Rdata")


if(!file.exists(paste0("data/intermediary/firjan_educ_2005_2016.Rdata"))){
  
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
  
  save(firjan_educ, file = "data/intermediary/firjan_educ_2005_2016.Rdata")
  
} else {
  load("data/intermediary/firjan_educ_2005_2016.Rdata")
}
