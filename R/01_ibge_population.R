
if (!dir.exists("data/raw/ibge")){dir.create("data/raw/ibge")}


if(!file.exists(paste0("data/intermediary/ibge_pop.Rdata"))){
  
  library(readxl)
  library(dplyr)
  library(tidyr)
  
  years <- c(2000:2006, 2008, 2009, 2011:2020)
  
  # download data:
  # IBGE population estimates
  # https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?edicao=17283&t=downloads (31 August 2023)
  for(yr in years){
    if(yr < 2015){
      if(yr < 2012){
        orig_file <- paste0("https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_", yr, "/UF_Municipio.zip")
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".zip")
        download.file(orig_file, destfile = dest_file)
      }
      if (yr == 2012){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2012/estimativa_2012_DOU_28_08_2012_xls.zip"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".zip")
        download.file(orig_file, destfile = dest_file)
      }
      if (yr == 2013){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2013/estimativa_2013_dou_xls.zip"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".zip")
        download.file(orig_file, destfile = dest_file)
      }
      if (yr == 2014){
        orig_file <- paste0("https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_", yr, "/estimativa_dou_", yr, "_xls.zip")
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".zip")
        download.file(orig_file, destfile = dest_file)
      }
      unzip(dest_file, exdir = "data/raw/ibge/") %>% file.rename(paste0("data/raw/ibge/POP-", yr, ".xls"))
      file.remove(dest_file)
    } else {
      if (yr == 2015){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2015/estimativa_dou_2015_20150915.xls"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".xls")
        download.file(orig_file, destfile = dest_file)
      }  
      if (yr == 2016){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2016/estimativa_dou_2016_20160913.xlsx"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".xlsx")
        download.file(orig_file, destfile = dest_file)
      }  
      if (yr == 2017){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2017/estimativa_dou_2017.xls"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".xls")
        download.file(orig_file, destfile = dest_file)
      }  
      if (yr == 2018){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2018/estimativa_dou_2018_20181019.xls"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".xls")
        download.file(orig_file, destfile = dest_file)
      }  
      if (yr == 2019){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2019/estimativa_dou_2019.xls"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".xls")
        download.file(orig_file, destfile = dest_file)
      }  
      if (yr == 2020){
        orig_file <- "https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2020/estimativa_dou_2020.xls"
        dest_file <- paste0("data/raw/ibge/POP-", yr, ".xls")
        download.file(orig_file, destfile = dest_file)
      }  
    }
  }
  
  # load selected municipalities from base municipality layers
  load("data/raw/geobr/selected_mun.Rdata")
  
# municipality ID concordance ---------------------------------------------
# we combine two datasets using different ID system, create concordance:

# read one "old" and one "new" file each (2000 and 2008)
old <- readxl::read_excel(paste0("data/raw/ibge/POP-2000.xls"), skip = 3, col_names = FALSE) %>%
  dplyr::select(1:5) %>%
  `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
  dplyr::filter(! is.na(pop)) %>%
  dplyr::mutate(ano = 2000)
new <- readxl::read_excel(paste0("data/raw/ibge/POP-2008.xls"), skip = 4) %>%
  dplyr::select(1:5) %>%
  `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
  dplyr::filter(! is.na(pop)) %>%
  dplyr::mutate(cod_municipio = cod_municipio_short) %>%
  dplyr::mutate(ano = 2008)

# match, ca. 1.5% municipalities get dropped due to changing municipality borders
conc <- dplyr::left_join(new %>% dplyr::select("codigo_uf", "cod_municipio", "nome_munic") %>% 
                           dplyr::mutate(unid = paste0(codigo_uf, nome_munic)), 
                         old %>% 
                           dplyr::mutate(unid = paste0(codigo_uf, nome_munic)) %>%
                           dplyr::select(-codigo_uf, -nome_munic, -pop, -ano), 
                         by = "unid") %>%
  dplyr::select(-unid) %>%
  dplyr::mutate(cod_municipio_short = as.character(cod_municipio_short),
                unique_mun_short = paste0(sigula_uf, cod_municipio_short)) %>%
  dplyr::mutate(cod_municipio_long = paste0(codigo_uf, cod_municipio))


conc <- conc %>% dplyr::filter(!is.na(cod_municipio_short))
rm(old, new)


# read and merge yearly data ----------------------------------------------
  
  # loop all years
  store_pop <- list()
  for (yr in seq_along(years)){
    
    if(years[yr] == 2000){
      ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xls"), skip = 3, col_names = FALSE) %>%
        dplyr::select(1:5) %>%
        `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
        dplyr::filter(! is.na(pop)) %>%
        dplyr::mutate(ano = years[yr],
                      unique_mun_short = paste0(sigula_uf, cod_municipio_short)) %>% 
        dplyr::left_join(conc %>% dplyr::select(unique_mun_short, cod_municipio), by = "unique_mun_short") %>%
        dplyr::filter(!is.na(cod_municipio))
    }
    if(years[yr] < 2007){
      ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xls"), skip = 4) %>%
        dplyr::select(1:5) %>%
        `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
        dplyr::filter(! is.na(pop)) %>%
        dplyr::mutate(ano = years[yr],
                      unique_mun_short = paste0(sigula_uf, cod_municipio_short)) %>% 
        dplyr::left_join(conc %>% dplyr::select(unique_mun_short, cod_municipio), by = "unique_mun_short") %>%
        dplyr::filter(!is.na(cod_municipio))
    } else {
    if(years[yr] < 2011){
      ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xls"), skip = 4) %>%
        dplyr::select(1:5) %>%
        `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
        dplyr::filter(! is.na(pop)) %>%
        dplyr::mutate(ano = years[yr])
    }
    if(years[yr] %in% c(2011:2013)){
        ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xls"), skip = 2) %>%
          dplyr::select(1:5) %>%
          `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
          dplyr::filter(! is.na(pop)) %>%
          dplyr::mutate(ano = years[yr])
      } 
      if(years[yr] %in% c(2014, 2015)){
        ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xls"), skip = 2, sheet = 2) %>%
          dplyr::select(1:5) %>%
          `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
          dplyr::filter(! is.na(pop)) %>%
          dplyr::mutate(ano = years[yr])
      } 
      if(years[yr] == 2016){
        ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xlsx"), skip = 2, sheet = 2) %>%
          dplyr::select(1:5) %>%
          `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
          dplyr::filter(! is.na(pop)) %>%
          dplyr::mutate(ano = years[yr])
      } 
      if(years[yr] %in% c(2017:2020)){
        ibge_pop <- readxl::read_excel(paste0("data/raw/ibge/POP-", years[yr], ".xls"), skip = 1, sheet = 2) %>%
          dplyr::select(1:5) %>%
          `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
          dplyr::filter(! is.na(pop)) %>%
          dplyr::mutate(ano = years[yr])

      } 
      
      if(class(ibge_pop$cod_municipio_short) == "numeric"){
        ibge_pop <- ibge_pop %>% dplyr::mutate(cod_municipio = sprintf("%05d", cod_municipio_short))
      } else{
        ibge_pop$cod_municipio <- ibge_pop$cod_municipio_short
      }
      
    }
    
    ibge_pop <- ibge_pop %>% dplyr::select("codigo_uf", "cod_municipio", "nome_munic", "ano", "pop")
    store_pop[[yr]] <- ibge_pop
    
  }
    
    
  ibge_pop <- do.call(rbind, store_pop) 
  
  # clean, harmonise municipality ID, transform to thousand persons, filter to selected municipalities
  ibge_pop <- ibge_pop %>%
    dplyr::filter(!is.na(codigo_uf)) %>%
    dplyr::mutate(pop = gsub("\\s*\\([^\\)]+\\)","", pop)) %>%
    dplyr::mutate(pop = gsub('\\D+','', pop)) %>%
    dplyr::rename(year = ano) %>%
    dplyr::mutate(cod_municipio_long = paste0(codigo_uf, cod_municipio)) %>%
    dplyr::mutate(unid = paste0(cod_municipio_long, "_", year)) %>%
    dplyr::filter(cod_municipio_long %in% selected_mun) %>%
    dplyr::mutate(pop = as.numeric(pop)) %>%
    dplyr::mutate(pop = pop / 1000) # thousand
  
  # interpolate missing 2007 and 2010 data
  temp <- ibge_pop %>% group_by(cod_municipio_long) %>% slice(1) %>%
    dplyr::mutate(year = 2007, pop = NA) %>%
    dplyr::mutate(unid = paste0(cod_municipio_long, "_", year))
  temp <- rbind(temp, temp %>% dplyr::mutate(year = 2010) %>%
                  dplyr::mutate(unid = paste0(cod_municipio_long, "_", year)))
  ibge_pop <- ibge_pop %>% dplyr::bind_rows(temp); rm(temp)
  ibge_pop <- ibge_pop %>% 
    dplyr::arrange(year) %>%
    dplyr::group_by(cod_municipio_long) %>%
    dplyr::mutate(pop = ifelse(year == 2007, (lag(pop, 1) +  lead(pop, 1)) / 2, pop)) %>%
    dplyr::mutate(pop = ifelse(year == 2010, (lag(pop, 1) +  lead(pop, 1)) / 2, pop))
  
  save(ibge_pop, file = "data/intermediary/ibge_pop.Rdata")
  
} else{
  load("data/intermediary/ibge_pop.Rdata")
}

