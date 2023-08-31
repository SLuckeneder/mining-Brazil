
library(readxl)
library(dplyr)
library(tidyr)

if (!dir.exists("data/raw/worldbank")){dir.create("data/raw/worldbank")}


# GDP deflator (annual %) - Brazil
# https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG?locations=BR (31 August 2023)
if(! file.exists("data/raw/worldbank/wb_GDP_deflator.csv")){
  download.file("https://api.worldbank.org/v2/en/indicator/NY.GDP.DEFL.KD.ZG?downloadformat=csv", destfile = "data/raw/worldbank/wb_test.zip")
  unzip("data/raw/worldbank/wb_test.zip", exdir = "data/raw/worldbank/") %>% 
    file.rename(c("data/raw/worldbank/Metadata_Indicator.csv", 
                  "data/raw/worldbank/wb_GDP_deflator.csv", 
                  "data/raw/worldbank/Metadata_Country.csv"))
}

# Commodity prices
# https://www.worldbank.org/en/research/commodity-markets (31 August 2023)
if(! file.exists("data/raw/worldbank/wb_commodity_prices.xlsx")){
  download.file("https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx", 
                destfile = "data/raw/worldbank/wb_commodity_prices.xlsx")
}
