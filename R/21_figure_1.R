
library(dplyr)
library(magrittr)
library(sf)
library(ggplot2)
library(gridExtra)
library(ggrepel)

if (!dir.exists("figures/main_text")){dir.create("figures/main_text", recursive = TRUE)}

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

# mining data
mining_area <- full_data %>% 
  dplyr::select(cod_municipio_long, year, mining) %>%
  dplyr::filter(year == 2020) %>%
  tidyr::spread(key = "year", value = "mining") %>%
  dplyr::rename("mining_2020" = `2020`) %>%
  dplyr::mutate(cod_municipio_long = as.character(cod_municipio_long))

mining_growth <- full_data %>%
  dplyr::select(cod_municipio_long, year, mining) %>%
  dplyr::filter(year %in% c(2005:2020)) %>%
  dplyr::arrange(year) %>%
  dplyr::group_by(cod_municipio_long) %>%
  dplyr::mutate(mining_growth = (mining - lag(mining)) / lag(mining) * 100) %>%
  dplyr::filter(is.finite(mining_growth)) %>%
  dplyr::summarise(mining_growth = mean(mining_growth)) %>%
  dplyr::mutate(cod_municipio_long = as.character(cod_municipio_long))

mining_data <- base_mun %>% 
  dplyr::mutate(code_muni = as.character(code_muni)) %>%
  dplyr::left_join(mining_area, by = c("code_muni" = "cod_municipio_long")) %>%
  dplyr::left_join(mining_growth, by = c("code_muni" = "cod_municipio_long")) 

# legal Amazon
if(! file.exists("data/raw/ibge/legal_amazon/Amazonia_Legal_2020.gpkg")){source("R/01_ibge_amazon.R")}
legal_amazon <- sf::read_sf("data/raw/ibge/legal_amazon/Amazonia_Legal_2020.gpkg") %>%
  sf::st_set_crs(sf::st_crs(mining_data))

# no of mining municipalities
full_data %>% dplyr::filter(mining > 0) %>%
  dplyr::group_by(cod_municipio_long) %>%
  dplyr::slice(1) %>% nrow()

# Minas Gerais
minas_gerais <- base_sta %>% dplyr::filter(abbrev_state == "MG")

# no of mining municipalities in MG
mining_data %>% dplyr::filter(abbrev_state == "MG") %>%
  dplyr::filter(mining_2020 > 0) %>%
  nrow()

# mining in the Legal Amazon
legal_amazon_mining <- mining_data %>% sf::st_intersection(legal_amazon) %>%
  dplyr::mutate(cat = "Legal Amazon")
sum(legal_amazon_mining$mining_2020, na.rm = T)
sum(mining_data$mining_2020, na.rm = T)
sum(legal_amazon_mining$mining_2020, na.rm = T) / sum(mining_data$mining_2020, na.rm = T)

# time series data by region (Amazon, Minas Gerais, Rest of Brazil) and mining type
mine_mun_panel <- full_data %>%
  dplyr::select(cod_municipio_long, sigla_uf, year,  mining_industrial, mining_garimpo) %>%
  dplyr::mutate(cod_municipio_long = as.character(cod_municipio_long)) %>%
  dplyr::left_join(legal_amazon_mining %>% dplyr::select(code_muni, cat), 
                   by = c("cod_municipio_long" = "code_muni"))
mine_mun_panel <- mine_mun_panel %>%
  dplyr::mutate(cat = ifelse(sigla_uf == "MG", "Minas Gerais", cat)) %>%
  dplyr::mutate(cat = ifelse(is.na(cat), "Rest of Brazil", cat)) %>%
  dplyr::select(-geom) %>%
  dplyr::rename("mining_region" = cat)
mine_mun_panel <- mine_mun_panel %>% 
  tidyr::gather(key = "mining_type", value = "mining_area", -cod_municipio_long, -sigla_uf, -year, -mining_region) %>%
  dplyr::mutate(mining_type = ifelse(mining_type == "mining_garimpo", "Garimpo mining", "Industrial mining"))



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


# map area ----------------------------------------------------------------

brks <- c(0, 10, 100, 1000, 10000, max(mining_data$mining_2020, na.rm = T))
mining_data <- mining_data %>%
  dplyr::mutate(mining_2020_d = cut(x = mining_data$mining_2020, breaks = brks, include.lowest = FALSE, dig.lab = 5)) %>%
  dplyr::mutate(mining_2020_d = ifelse(is.na(mining_2020_d), 0, as.character(mining_2020_d)))

p_map_2020 <- mining_data %>% 
  ggplot2::ggplot(aes(fill = mining_2020_d)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_manual(values = c(viridis::viridis(5, direction = -1), "white"), 
                             labels = c("0 to 10", "10 to 100", "100 to 1,000", "1,000 to 10,000", "10,000 to 41,327", "No mining")) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::geom_sf(data = legal_amazon , fill = NA, colour = "red", alpha = 0.5, lwd = 0.3) +
  ggplot2::geom_sf(data = minas_gerais , fill = NA, colour = "red", alpha = 0.5, lwd = 0.3) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::geom_curve(aes(
    xend = -49.903966184516804, x = -46,
    yend = 1.6296791814468312, y = 1.0998414027864398), 
    curvature = 0.2, size = 0.4, arrow = arrow(length = unit(0.005, "npc")), colour = "black") +
  ggplot2::geom_curve(aes(
    xend = -49.903966184516804, x = -46,
    yend = 1.6296791814468312, y = 1.0998414027864398), 
    curvature = 0.2, size = 0.2, arrow = arrow(length = unit(0.005, "npc")), colour = "red") +
  ggplot2::geom_curve(aes(
    xend = -45.27609797823044, x = -45.27609797823044,
    yend = -22.61750495298117, y = -26.800678103403406), 
    curvature = -0.1, size = 0.4, arrow = arrow(length = unit(0.005, "npc")), colour = "black") +
  ggplot2::geom_curve(aes(
    xend = -45.27609797823044, x = -45.27609797823044,
    yend = -22.61750495298117, y = -26.800678103403406), 
    curvature = -0.1, size = 0.2, arrow = arrow(length = unit(0.005, "npc")), colour = "red") +
  geom_text(aes(x = -46, y = 1.0998414027864398, label = "Legal Amazon"),
            nudge_x = 0.4, nudge_y = -0.3, hjust = 0, color = "black", size = 7) +
  geom_text(aes(x = -44.8, y = -26.800678103403406, label = "Minas Gerais"),
            nudge_x = 1, nudge_y = -0.7, hjust = 0.5, color = "black", size = 7) +
  ggplot2::labs(fill = "Mining area 2020 (ha)") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=19),
                 legend.title = element_text(size=21, face = "bold"))


# map growth --------------------------------------------------------------


brks <- c(min(mining_data$mining_growth, na.rm = T), -10, -5, 0, 5, 10, 100, max(mining_data$mining_growth, na.rm = T))
mining_data <- mining_data %>%
  dplyr::mutate(mining_growth_d = cut(x = mining_data$mining_growth, breaks = brks, include.lowest = FALSE, dig.lab = 5))

mining_data <- mining_data %>%
  dplyr::mutate(mining_growth_mod = ifelse(mining_growth > 10, 10, mining_growth),
                mining_growth_mod = ifelse(mining_growth_mod < -10, -10, mining_growth_mod))

p_map_growth <- mining_data %>% 
  ggplot2::ggplot(aes(fill = mining_growth_mod)) +
  ggplot2::geom_sf(lwd = 0.1) +
  ggplot2::scale_fill_gradient2(low = "#7fbc41", high = "#c51b7d", na.value = "white",
                                breaks = c(-10, -5, 0, 5, 10), labels = c("less than -10%", "-5%", "0%", "5%", "more than 10%")) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = c(0, 0)) +
  ggplot2::labs(fill = "Average annual change \nof mining area (2005-2020)\n") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=19),
                 legend.title = element_text(size=21, face = "bold"))


# TS by region ------------------------------------------------------------

p_ts_region <- mine_mun_panel %>% 
  dplyr::group_by(year, mining_region) %>%
  dplyr::summarise(mining_area_1000_ha = sum(mining_area) / 1000) %>%
  dplyr::filter(year %in% c(2005:2020)) %>%
  ggplot2::ggplot(aes(x = year, y = mining_area_1000_ha, fill = mining_region)) +
  ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits= c(0, 385), expand = c(0,0)) +
  ggplot2::scale_x_continuous(breaks=c(2005:2020), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(values = viridis::rocket(9)[c(8, 6, 4)]) + 
  ggplot2::labs(title = NULL, x = NULL, y = NULL) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
                 axis.ticks.x = element_blank(),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 16, face = "bold"),
                 legend.position = c(0.3, 0.8),
                 legend.direction = "vertical",
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank()) +
  ggplot2::guides(fill = guide_legend(title = "Mining area (1,000 ha)", title.position = "top", title.hjust = 0.5))


# TS by type --------------------------------------------------------------

p_ts_type <- mine_mun_panel %>% 
  dplyr::group_by(year, mining_type) %>%
  dplyr::summarise(mining_area_1000_ha = sum(mining_area) / 1000) %>%
  dplyr::filter(year %in% c(2005:2020)) %>%
  ggplot2::ggplot(aes(x = year, y = mining_area_1000_ha, fill = mining_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.8, color = "black") +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits= c(0, 385), expand = c(0,0)) +
  ggplot2::scale_x_continuous(breaks=c(2005:2020), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(values = viridis::mako(9)[c(8, 4)]) + 
  ggplot2::labs(title = NULL, x = NULL, y = NULL) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
                 axis.ticks.x = element_blank(),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 16, face = "bold"),
                 legend.position = c(0.3, 0.8),
                 legend.direction = "vertical",
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank()) +
  ggplot2::guides(fill = guide_legend(title = "Mining area (1,000 ha)", title.position = "top", title.hjust = 0.5))



# merge -------------------------------------------------------------------

p_maps <- cowplot::plot_grid(p_map_2020, p_map_growth, nrow = 1, rel_widths = c(1/2, 1/2), labels = c("a", "b"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
p_bars <- cowplot::plot_grid(p_ts_region, p_ts_type, nrow = 2, rel_heights = c(1/2, 1/2), labels = c("c", "d"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
p_mining <- cowplot::plot_grid(p_maps, p_bars, labels = c('', ''), rel_widths = c(3/4, 1/4), nrow = 1)

ggplot2::ggsave("figure_1.png",
                plot = p_mining, device = "png",
                path = paste0("./figures/main_text"),
                scale = 1, width = 600, height = 250, units = "mm")



