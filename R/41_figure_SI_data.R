

library(dplyr)
library(sf)
library(ggplot2)
library(gridExtra)



# data --------------------------------------------------------------------

# spatial base layer
if(!file.exists(paste0("data/raw/geobr/base_mun_2015.gpkg"))){source("R/01_base_maps.R")}
base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
base_sta <- sf::read_sf("data/raw/geobr/base_sta_2015.gpkg")
base_nat <- sf::read_sf("data/raw/geobr/base_nat_2015.gpkg")

# read data
if(! file.exists("data/full_data_2000-2020_5y.csv")){source("R/00_compile_data.R")}
full_data <- read.csv2(file = paste0("data/full_data_2000-2020_5y.csv"), sep = ",", stringsAsFactors = FALSE) # read full data matrix
full_data <- full_data %>% dplyr::mutate_at(c(4:121), as.numeric)

# tidy data
maps_dat <- full_data %>%
  dplyr::select(cod_municipio_long, year,
                gdp_capita_growth, natural_forest_loss_ha_km2, natural_forest_loss,
                mining_industrial, mining_garimpo,
                share_Forest.Formation, gdp_capita_log) %>%
  dplyr::filter(year %in% c(2005, 2010, 2015)) %>%
  dplyr::mutate(mining_industrial = ifelse(mining_industrial > 0, 1, 0)) %>%
  dplyr::mutate(mining_garimpo = ifelse(mining_garimpo > 0, 1, 0))


# make spatial
maps_dat <- base_mun %>%
  dplyr::left_join(maps_dat, by = c("code_muni" = "cod_municipio_long"))


# map preparations --------------------------------------------------------

# zoom table
zoom_bbox <- tibble::tribble(
  ~region,                    ~x_lim,            ~y_lim,
  "Brazil",         c(-74.00, -34.00), c(-34.00, 6.00)
) %>%
  dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
                group = 1,
                geometry = lapply(geometry, sf::st_bbox),
                geometry = lapply(geometry, sf::st_as_sfc),
                geometry = lapply(geometry, sf::st_geometrycollection),
                geometry = sf::st_sfc(geometry)) %>%
  sf::st_sf() %>%
  sf::st_collection_extract()

lim <- zoom_bbox %>%
  dplyr::filter(region == "Brazil")

# map theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# GDP growth --------------------------------------------------------------

selected_var <- maps_dat %>% 
  sf::st_drop_geometry() %>%
  dplyr::filter(year == 2005) %>%
  dplyr::select(gdp_capita_growth) %>%
  dplyr::pull() 
quantile(x = selected_var, probs = seq(0, 1, 0.2), na.rm = T)
brks <- c(min(selected_var, na.rm = TRUE), 0, 5, 10, 15,  max(selected_var, na.rm = TRUE))
maps_dat <- maps_dat %>%
  dplyr::mutate(gdp_growth_d = cut(x = maps_dat$gdp_capita_growth, breaks = brks, include.lowest = FALSE, dig.lab = 5))

p_map_gdp_growth_2005 <- maps_dat %>% 
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = gdp_growth_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::cividis(5, direction = -1), "white"),
                             labels = c("< 0%", "0% to 5%", "5% to 10%", "10% to 15%", "> 15%"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "GDP per capita \ngrowth, 2005-2010") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_legend(reverse=T))

p_map_gdp_growth_2010 <- maps_dat %>% 
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = gdp_growth_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::cividis(5, direction = -1), "white"),
                             labels = c("< 0%", "0% to 5%", "5% to 10%", "10% to 15%", "> 15%"), 
                             na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "GDP per capita \ngrowth, 2010-2015") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_legend(reverse=T))

p_map_gdp_growth_2015 <- maps_dat %>% 
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot(aes(fill = gdp_growth_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::cividis(5, direction = -1), "white"),
                             labels = c("< 0%", "0% to 5%", "5% to 10%", "10% to 15%", "> 15%"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "GDP per capita \ngrowth, 2015-2020") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_legend(reverse=T))


# forest loss relative ----------------------------------------------------

maps_dat <- maps_dat %>%
  dplyr::mutate(forest_loss_relative_cut = ifelse(natural_forest_loss_ha_km2 > 2, 2, natural_forest_loss_ha_km2))

selected_var <- maps_dat %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year == 2005) %>%
  dplyr::select(forest_loss_relative_cut) %>%
  dplyr::pull()
mybreaks <- pretty(selected_var, 6)
if(max(mybreaks) > max(selected_var, na.rm = TRUE)){mybreaks <- mybreaks[-length(mybreaks)]}
mylabels <- mybreaks
mylabels[length(mylabels)] <- paste0("\u2265", mylabels[length(mylabels)])

p_map_forest_relative_2005 <- maps_dat %>% 
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = forest_loss_relative_cut)) +
  ggplot2::geom_sf(lwd = 0.1) +
  viridis::scale_fill_viridis(option = "viridis", direction = -1, breaks = mybreaks, labels = mylabels) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Forest loss 2005 \n(ha per km2 of \nmunicipality area)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_forest_relative_2010 <- maps_dat %>% 
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = forest_loss_relative_cut)) +
  ggplot2::geom_sf(lwd = 0.1) +
  viridis::scale_fill_viridis(option = "viridis", direction = -1, breaks = mybreaks, labels = mylabels) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Forest loss 2010 \n(ha per km2 of \nmunicipality area)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_forest_relative_2015 <- maps_dat %>% 
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot(aes(fill = forest_loss_relative_cut)) +
  ggplot2::geom_sf(lwd = 0.1) +
  viridis::scale_fill_viridis(option = "viridis", direction = -1, breaks = mybreaks, labels = mylabels) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Forest loss 2015 \n(ha per km2 of \nmunicipality area)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))


# forest loss absolute ----------------------------------------------------

selected_var <- maps_dat %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year == 2005) %>%
  dplyr::select(natural_forest_loss) %>%
  dplyr::pull()
quantile(x = selected_var, probs = seq(0, 1, 0.1), na.rm = T)
brks <- c(-1, 0, 50, 500, 5000, 25000,  max(selected_var, na.rm = TRUE))
maps_dat <- maps_dat %>%
  dplyr::mutate(forest_loss_absolute_d = cut(x = maps_dat$natural_forest_loss, breaks = brks, include.lowest = FALSE, dig.lab = 5))

p_map_forest_absolute_2005 <- maps_dat %>% 
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = forest_loss_absolute_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::rocket(6, direction = -1)),
                             labels = c("0", "(0, 50]", "(50, 500]", "(500, 5,000]", "(5,000, 25,000]", "> 25,000"), 
                             na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Forest loss 2005 (ha)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_legend(reverse=T))

p_map_forest_absolute_2010 <- maps_dat %>% 
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = forest_loss_absolute_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::rocket(6, direction = -1)),
                             labels = c("0", "(0, 50]", "(50, 500]", "(500, 5,000]", "(5,000, 25,000]", "> 25,000"), 
                             na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Forest loss 2010 (ha)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_legend(reverse=T))

p_map_forest_absolute_2015 <- maps_dat %>% 
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot(aes(fill = forest_loss_absolute_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::rocket(6, direction = -1)),
                             labels = c("0", "(0, 50]", "(50, 500]", "(500, 5,000]", "(5,000, 25,000]", "> 25,000"), 
                             na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Forest loss 2015 (ha)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_legend(reverse=T))


# mining industrial -------------------------------------------------------

p_map_industrial_mining_2005 <- maps_dat %>%
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = as.character(mining_industrial))) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c("#7fb3d5", "#154360"),
                             labels = c("FALSE", "TRUE"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Industrial mining \n2005") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_industrial_mining_2010 <- maps_dat %>%
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = as.character(mining_industrial))) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c("#7fb3d5", "#154360"),
                             labels = c("FALSE", "TRUE"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Industrial mining \n2010") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_industrial_mining_2015 <- maps_dat %>%
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot(aes(fill = as.character(mining_industrial))) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c("#7fb3d5", "#154360"),
                             labels = c("FALSE", "TRUE"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Industrial mining \n2015") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))


# mining garimpo ----------------------------------------------------------

p_map_garimpo_mining_2005 <- maps_dat %>%
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = as.character(mining_garimpo))) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c("#f59c39", "#6a1818"),
                             labels = c("FALSE", "TRUE"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Garimpo mining \n2005") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_garimpo_mining_2010 <- maps_dat %>%
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = as.character(mining_garimpo))) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c("#f59c39", "#6a1818"),
                             labels = c("FALSE", "TRUE"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Garimpo mining \n2010") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_garimpo_mining_2015 <- maps_dat %>%
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot(aes(fill = as.character(mining_garimpo))) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c("#f59c39", "#6a1818"),
                             labels = c("FALSE", "TRUE"), na.value = "white", na.translate = F) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Garimpo mining \n2015") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))


# share forest formation --------------------------------------------------

maps_dat <- maps_dat %>%
  dplyr::mutate(share_forest_formation_cut = ifelse(share_Forest.Formation > 0.4, 0.4, share_Forest.Formation)) %>%
  dplyr::mutate(share_forest_formation_cut = share_forest_formation_cut*100)

selected_var <- maps_dat %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year == 2005) %>%
  dplyr::select(share_forest_formation_cut) %>%
  dplyr::pull()
mybreaks <- pretty(selected_var, 4)
if(max(mybreaks) > max(selected_var, na.rm = TRUE)){mybreaks <- mybreaks[-length(mybreaks)]}
mylabels <- mybreaks
mylabels[length(mylabels)] <- paste0("\u2265", mylabels[length(mylabels)])

p_map_share_forest_2005 <- maps_dat %>%
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = share_forest_formation_cut)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_distiller(palette = "Greens", trans = "reverse",
                                breaks = mybreaks, labels = mylabels) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Share natural forest \nformation 2005 (%)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_colorbar(reverse = TRUE), size = guide_legend(reverse = TRUE))

p_map_share_forest_2010 <- maps_dat %>%
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = share_forest_formation_cut)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_distiller(palette = "Greens", trans = "reverse", guide = guide_legend(reverse = TRUE),
                                breaks = mybreaks, labels = mylabels) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Share natural forest \nformation 2010 (%)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_colorbar(reverse = TRUE), size = guide_legend(reverse = TRUE))

p_map_share_forest_2015 <- maps_dat %>%
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot(aes(fill = share_forest_formation_cut)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_distiller(palette = "Greens", trans = "reverse", guide = guide_legend(reverse = TRUE),
                                breaks = mybreaks, labels = mylabels) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Share natural forest \nformation 2015 (%)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21)) +
  ggplot2::guides(fill = guide_colorbar(reverse = TRUE), size = guide_legend(reverse = TRUE))


# GDP per capita ----------------------------------------------------------

p_map_gdp_capita_2005 <- maps_dat %>%
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = gdp_capita_log)) +
  ggplot2::geom_sf(lwd = 0.1) +
  viridis::scale_fill_viridis(option = "mako", direction = -1) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "GDP per capita 2005 \n(million BRL, current \nPPP, log)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_gdp_capita_2010 <- maps_dat %>%
  dplyr::filter(year == 2010) %>%
  ggplot2::ggplot(aes(fill = gdp_capita_log)) +
  ggplot2::geom_sf(lwd = 0.1) +
  viridis::scale_fill_viridis(option = "mako", direction = -1) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "GDP per capita 2010 \n(million BRL, current \nPPP, log)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))

p_map_gdp_capita_2015 <- maps_dat %>%
  dplyr::filter(year == 2005) %>%
  ggplot2::ggplot(aes(fill = gdp_capita_log)) +
  ggplot2::geom_sf(lwd = 0.1) +
  viridis::scale_fill_viridis(option = "mako", direction = -1) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "GDP per capita 2015 \n(million BRL, current \nPPP, log)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=17),
                 legend.title = element_text(size=21))



# merge -------------------------------------------------------------------

p_maps_A <- cowplot::plot_grid(p_map_industrial_mining_2005, p_map_industrial_mining_2010, p_map_industrial_mining_2015,
                               p_map_garimpo_mining_2005, p_map_garimpo_mining_2010, p_map_garimpo_mining_2015,
                               p_map_gdp_capita_2005, p_map_gdp_capita_2010, p_map_gdp_capita_2015,
                               p_map_gdp_growth_2005, p_map_gdp_growth_2010, p_map_gdp_growth_2015,
                               nrow = 4, 
                               labels = letters[c(1:12)], label_size = 16) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_data_A.png",
                plot = p_maps_A, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 600, height = 840, units = "mm")



p_maps_B <- cowplot::plot_grid(p_map_share_forest_2005, p_map_share_forest_2010, p_map_share_forest_2015, 
                               p_map_forest_absolute_2005, p_map_forest_absolute_2010, p_map_forest_absolute_2015,
                               p_map_forest_relative_2005, p_map_forest_relative_2010, p_map_forest_relative_2015,
                               nrow = 3, 
                               labels = letters[c(1:9)], label_size = 16) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_data_B.png",
                plot = p_maps_B, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 600, height = 630, units = "mm")




