library(dplyr)
library(sf)
library(spdep)

# data --------------------------------------------------------------------

# spatial base layer
if(!file.exists(paste0("data/raw/geobr/base_mun_2015.gpkg"))){source("R/01_base_maps.R")}
base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")

# read full data and obtain list of municipalities covered (5,262 municipalities)
if(! file.exists("data/full_data_2000-2020_5y.csv")){source("R/00_compile_data.R")}
full_data <- read.csv2(file = paste0("data/full_data_2000-2020_5y.csv"), sep = ",", stringsAsFactors = FALSE) # read full data matrix
selected_mun <- unique(full_data$cod_municipio_long)

# weights matrices --------------------------------------------------------

# filter for selected municipalities
W_base <- base_mun %>%
  dplyr::filter(code_muni %in% selected_mun)

# remove islands
nb_q <- spdep::poly2nb(W_base, queen=TRUE)
summary(nb_q)
W_base <- W_base[-c(231, 3376),]

coords <- sf::st_centroid(sf::st_geometry(W_base), of_largest_polygon=TRUE)

nb_5nnb <- spdep::knearneigh(coords, k=5)
nb_5nnb <- spdep::knn2nb(nb_5nnb, sym=F)

png(file="figures/SI/figure_SI_W.png", width=1500, height=1500)
plot(sf::st_geometry(base_mun), border="grey", lwd=2)
plot(nb_5nnb, coords=st_coordinates(coords), add=TRUE, points=FALSE, lwd=0.6, col = "blue")
dev.off()
