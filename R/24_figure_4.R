
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

if (!dir.exists("figures/main_text")){dir.create("figures/main_text", recursive = TRUE)}


# Figure 4 -----------------------------------------------------------------

# read data
if(! file.exists("data/full_data_2000-2020_5y.csv")){source("R/00_compile_data.R")}
full_data <- read.csv2(file = paste0("data/full_data_2000-2020_5y.csv"), sep = ",", stringsAsFactors = FALSE) # read full data matrix
full_data <- full_data %>% dplyr::mutate_at(c(4:121), as.numeric)

# selected municipalities in balanced panel (5506 municipality ids)
if(!file.exists(paste0("data/raw/geobr/selected_mun.Rdata"))){source("R/01_base_maps.R")}
load("data/raw/geobr/selected_mun.Rdata")

# land cover and mining data
tidy_data <- full_data %>% 
  dplyr::filter(cod_municipio_long %in% selected_mun) %>%
  dplyr::select(cod_municipio_long, year, AREA_mun, natural_forest_loss, natural_forest_loss_ha_km2, mining) %>%
  dplyr::mutate(mining = ifelse(mining > 0 , TRUE, FALSE)) %>%
  dplyr::filter(year > 2000)

# look at total forest loss
p_dat <- tidy_data %>% 
  dplyr::group_by(year, mining) %>%
  dplyr::summarize(natural_forest_loss = sum(natural_forest_loss) / 100) 

# prepare data for Figure 3a: median forest loss rates for mining and non-mining municipalities (exclude municipalities with zero forest)
temp_median <- tidy_data %>% 
  dplyr::filter(natural_forest_loss > 0) %>%
  dplyr::group_by(year, mining) %>%
  dplyr::summarise(average_forest_loss_ha_km2 = median(natural_forest_loss_ha_km2))

p_dat$average_forest_loss_ha_km2 <- temp_median$average_forest_loss_ha_km2

p_dat <- p_dat %>% 
  dplyr::mutate(mining = ifelse(mining == TRUE, "Mining municipalities", "Non-mining municipalities")) %>%
  dplyr::mutate(date = as.Date(paste0(year, "-01-01")))

conc <- p_dat %>% 
  dplyr::group_by(date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(d = row_number()) %>%
  dplyr::select(date, d)

p_dat <- p_dat %>% dplyr::left_join(conc) %>% dplyr::mutate(d = as.numeric(d))

bounds <- p_dat %>% 
  dplyr::ungroup() %>%
  dplyr::select(date, mining, average_forest_loss_ha_km2) %>%
  dplyr::filter(!is.na(mining)) %>%
  tidyr::spread(key = "mining", value = "average_forest_loss_ha_km2") %>%
  dplyr::mutate(
    ymax = pmax(`Mining municipalities`, `Non-mining municipalities`),
    ymin = pmin(`Mining municipalities`, `Non-mining municipalities`),
    fill = `Mining municipalities` >= `Non-mining municipalities`
  )

intervals <- bounds %>%
  filter(ymax > ymin) %>%
  dplyr::left_join(conc)

other_intersections <- bounds %>%
  dplyr::left_join(conc) %>%
  transmute(
    x1 = d,       y1 = `Mining municipalities`,
    x2 = lead(d), y2 = lead(`Mining municipalities`),
    x3 = d,       y3 = `Non-mining municipalities`,
    x4 = lead(d), y4 = lead(`Non-mining municipalities`)
  ) %>%
  dplyr::filter(((y1 > y3) & (y2 < y4)) | ((y1 < y3) & (y2 > y4)))

other_intersections <- other_intersections %>%  # only rows where an intersection occurs between two dates
  mutate(
    e = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4),  # denominator
    u = x1 * y2 - y1 * x2,
    v = x3 * y4 - y3 * x4,
    d = (u * (x3 - x4) - v * (x1 - x2)) / e,
    y = (u * (y3 - y4) - v * (y1 - y2)) / e
  ) %>%
  dplyr::mutate(ymax = y, ymin = y) %>%
  dplyr::select(d, ymax, ymin)

ribbons <- bind_rows(
  intervals,
  # intersections,
  mutate(other_intersections, fill = TRUE),
  mutate(other_intersections, fill = FALSE)
) %>%
  arrange(date)

# plot
p_ts_relative <- p_dat %>% 
  dplyr::filter(date > "2002-12-31" & date < "2020-01-02") %>%
  ggplot2::ggplot(aes(x = d)) + 
  ggplot2::geom_line( aes(y = average_forest_loss_ha_km2, group = mining, colour = mining), size = 1) +
  ggplot2::geom_ribbon(data = ribbons, aes(ymin = ymin, ymax = ymax, fill = fill), alpha = 0.1) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(0, 0.34), expand = c(0, 0)) +
  scale_x_continuous(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20), limits = c(3, 20),
                     labels= c("2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020"), expand = c(0, 0)) +
  ggplot2::scale_color_manual(values = viridis::plasma(7)[c(2, 6)]) + 
  ggplot2::scale_fill_manual(values = viridis::plasma(7)[c(2, 6)], na.translate = F) +
  ggplot2::labs(title = NULL, x = NULL, y = NULL, colour = NULL) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14, vjust = 0),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 16, face = "bold"),
                 legend.position = c(0.5, 0.8),
                 legend.direction = "horizontal",
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 plot.margin = margin(0.5,1,0.5,0.7, "cm")) +
  ggplot2::guides(colour = guide_legend(title = expression(bold(Forest~loss~(ha/km^2))), title.position = "top", title.hjust = 0.5))

# compute differences between mining and non-mining for mention in manuscript text
period <- c(2003:2020)
p_dat %>% dplyr::filter(year %in% period, !is.na(mining)) %>%
  dplyr::group_by(mining) %>%
  dplyr::summarise(mean_forest_loss_ha_km2 = mean(average_forest_loss_ha_km2)) %>%
  tidyr::spread(key = "mining", value = "mean_forest_loss_ha_km2") %>%
    dplyr::mutate(difference_ha_km2 = `Mining municipalities` - `Non-mining municipalities`) %>%
    dplyr::mutate(difference_perc = (`Mining municipalities` - `Non-mining municipalities`) / `Non-mining municipalities` * 100)


# compute differences between mining and non-mining
period <- c(2003:2020)
temp <-  p_dat %>% dplyr::filter(year %in% period, !is.na(mining))  %>%
  dplyr::group_by(mining, year) %>%
  dplyr::summarise(mean_forest_loss_ha_km2 = mean(average_forest_loss_ha_km2)) %>%
  tidyr::spread(key = "mining", value = "mean_forest_loss_ha_km2") %>%
  dplyr::mutate(difference_ha_km2 = `Mining municipalities` - `Non-mining municipalities`) %>%
  dplyr::mutate(difference_perc = (`Mining municipalities` - `Non-mining municipalities`) / `Non-mining municipalities` * 100); temp
mean(temp$difference_perc); rm(temp, period)


# export ------------------------------------------------------------------

ggplot2::ggsave("figure_4.png",
                plot = p_ts_relative, device = "png",
                path = paste0("./figures/main_text"),
                scale = 1, width = 300, height = 100, units = "mm")


