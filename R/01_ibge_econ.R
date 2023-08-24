
if (!dir.exists("data/raw/ibge")){dir.create("data/raw/ibge")}
# Add data:
# IBGE Gross Domestic Product of Municipalities
# https://www.ibge.gov.br/en/statistics/economic/national-accounts/19567-gross-domestic-product-of-municipalities.html?=&t=downloads (1 June 2022)

library(readxl)
library(dplyr)
library(tidyr)

# load selected municipalities from base municipality layers
load("data/raw/geobr/selected_mun.Rdata")

if(!file.exists(paste0("data/intermediary/ibge_econ.Rdata"))){
  
  
  # read 2002 - 2009 data
  ibge_econ_2002_2009 <- read_excel("data/raw/ibge/GDP-2002-2009.xls") %>%
    dplyr::select(c(1:8, 33:40))
  colnames(ibge_econ_2002_2009) <- c("year", "cod_reg", "name_reg", "cod_uf", "sigla_uf", "name_uf", "cod_municipio", "name_munic", 
                                    "gva_agri", "gva_indu", "gva_serv", "gva_publ", "gva_total", "tax", "gdp_total", # all in curren R$ 1.000
                                    "gdp_capita") # "current" R$
  
  # read 2010 - 2020 data
  ibge_econ_2010_2020 <- read_excel("data/raw/ibge/GDP-2010-2020.xls") %>%
    dplyr::select(c(1:8, 33:40))
  colnames(ibge_econ_2010_2020) <- c("year", "cod_reg", "name_reg", "cod_uf", "sigla_uf", "name_uf", "cod_municipio", "name_munic", 
                                    "gva_agri", "gva_indu", "gva_serv", "gva_publ", "gva_total", "tax", "gdp_total", # all in current R$ 1.000
                                    "gdp_capita") # "current" R$
  
  # merge
  ibge_econ <- dplyr::bind_rows(ibge_econ_2002_2009, ibge_econ_2010_2020) %>%
    dplyr::mutate(cod_municipio = substr(cod_municipio, 3, 7))
  rm(ibge_econ_2002_2009, ibge_econ_2010_2020)
  
  # harmonise municipality ID, transform to million Real, filter to selected municipalities
  ibge_econ <- ibge_econ %>%
    dplyr::mutate(cod_municipio_long = paste0(cod_uf, cod_municipio)) %>%
    dplyr::mutate(unid = paste0(cod_municipio_long, "_", year)) %>%
    dplyr::filter(cod_municipio_long %in% selected_mun) %>%
    dplyr::mutate(gva_agri = gva_agri / 1000,
                  gva_indu  = gva_indu  / 1000,
                  gva_serv  = gva_serv  / 1000,
                  gva_publ  = gva_publ  / 1000,
                  gva_total = gva_total / 1000,
                  tax =  tax / 1000,
                  gdp_total = gdp_total / 1000)
  
  save(ibge_econ, file = "data/intermediary/ibge_econ.Rdata")
  
  
} else {
  load("data/intermediary/ibge_econ.Rdata")
}






