

library(sf)
library(dplyr)
library(tidyr)

if (!dir.exists("data/raw/ibge/legal_amazon")){dir.create("data/raw/ibge/legal_amazon", recursive = TRUE)}

if(! file.exists("data/raw/ibge/legal_amazon/Amazonia_Legal_2020.gpkg")){
  
  # IBGE borders of the Legal Amazon
  # https://www.ibge.gov.br/en/geosciences/environmental-information/vegetation/17927-legal-amazon.html?=&t=acesso-ao-produto (31 August 2023)
  
  download.file("https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2022/Limites_Amazonia_Legal_2022_shp.zip", destfile = "data/raw/ibge/legal_amazon/legal_amazon.zip")
  unzip("data/raw/ibge/legal_amazon/legal_amazon.zip", exdir = "data/raw/ibge/legal_amazon")
  legal_amazon <- sf::read_sf("data/raw/ibge/legal_amazon/Limites_Amazonia_Legal_2022.shp")
  sf::st_write(legal_amazon, "data/raw/ibge/legal_amazon/Amazonia_Legal_2020.gpkg")
  
}

