

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggsci)

# DONT FORGET TO MODIFY 40.R!

# data --------------------------------------------------------------------

# spatial base layer
if(!file.exists(paste0("data/raw/geobr/base_mun_2015.gpkg"))){source("R/01_base_maps.R")}
base_mun <- sf::read_sf("data/raw/geobr/base_mun_2015.gpkg")
base_sta <- sf::read_sf("data/raw/geobr/base_sta_2015.gpkg")
base_nat <- sf::read_sf("data/raw/geobr/base_nat_2015.gpkg")

# biome data for Brazil
biomes <- geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = TRUE) %>%
  dplyr::filter(name_biome != "Sistema Costeiro")

# mining data
if (! file.exists("data/raw/MapBiomas-v8/MapBiomas_col8_mining.xlsx")){
  download.file("https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/09/TABELA-MINERACAO-MAPBIOMAS-COL8.0.xlsx", 
                destfile = "data/raw/MapBiomas-v8/MapBiomas_col8_mining.xlsx")
}
mining_data_raw <- readxl::read_excel("data/raw/MapBiomas-v8/MapBiomas_col8_mining.xlsx", sheet = 2)

mining_data <- mining_data_raw %>%
  dplyr::select(GEOCODE, biome, level_1, 12:49) %>%
  tidyr::gather(key = "year", value = "value", -GEOCODE, -biome, -level_1) %>%
  dplyr::group_by(GEOCODE, biome, year, level_1) %>%
  dplyr::summarise(value = sum(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(value > 0) %>%
  tidyr::spread(key = "level_1", value = "value") %>%
  dplyr::rename(mining_industrial = `2. Industrial`,
                mining_garimpo = `1. Garimpo`) %>%
  dplyr::mutate(mining_industrial = ifelse(is.na(mining_industrial), 0, mining_industrial)) %>%
  dplyr::mutate(mining_garimpo = ifelse(is.na(mining_garimpo), 0, mining_garimpo))



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

# map ---------------------------------------------------------------------



p_map_biomes <- biomes %>% 
  ggplot2::ggplot(aes(fill = name_biome)) +
  ggplot2::geom_sf(lwd = 0) +
  ggplot2::scale_fill_manual(values = pal_jco()(10)[c(1:5, 7)]) +
  ggplot2::geom_sf(data = base_sta , fill = NA, lwd = 0.6) +
  ggplot2::coord_sf(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]], expand = 0) +
  ggplot2::labs(fill = "Biome") +
  theme_map() +
  ggplot2::theme(legend.position = c(0.17, 0.27),
                 legend.text=element_text(size=19),
                 legend.title = element_text(size=21, face = "bold"))

# bar charts --------------------------------------------------------------

p_dat <- mining_data %>% 
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) %>%
  dplyr::group_by(biome, year) %>% 
  dplyr::summarise(mining_garimpo = sum(mining_garimpo), 
                   mining_industrial = sum(mining_industrial)) %>%
  tidyr::gather(key = "type", value = "mining_ha", -biome, -year) %>%
  dplyr::mutate(type = ifelse(type == "mining_garimpo", "Garimpo mining", "Industrial mining"))

p_dat$biome <- factor(p_dat$biome, levels = rev(unique(p_dat$biome)))

p_bars_biomes <- p_dat %>% 
  ggplot2::ggplot(aes(x = biome, y = mining_ha / 1000, fill = type)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits= c(0, 175), expand = c(0,0)) +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(rows = vars(year)) +
  ggplot2::scale_fill_manual(values = viridis::mako(9)[c(8, 4)]) + 
  ggplot2::labs(title = NULL, x = NULL, y = "Mining area (1,000 ha)", fill = NULL) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.ticks.y = element_blank(),
                 axis.title.x = element_text(size = 14),
                 strip.text.y = element_text(size = 14),
                 legend.text = element_text(size = 16),
                 legend.position = c(0.73, 0.87),
                 legend.direction = "vertical",
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank()) +
  ggplot2::guides(fill = guide_legend(reverse=T))

# merge -------------------------------------------------------------------

p_biomes <- cowplot::plot_grid(p_map_biomes, p_bars_biomes, labels = c('a', 'b'), label_size = 18, rel_widths = c(1/2, 1/2), nrow = 1) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_biomes.png",
                plot = p_biomes, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 320, height = 200, units = "mm")


# # for thesis --------------------------------------------------------------
# 
# p_biomes <- cowplot::plot_grid(p_map_biomes, p_bars_biomes, labels = c('A', 'B'), label_size = 18, rel_widths = c(1/2, 1/2), nrow = 1) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("figure_SI_biomes.png",
#                 plot = p_biomes, device = "png",
#                 path = paste0("./figures/thesis"),
#                 scale = 1, width = 320, height = 200, units = "mm")

