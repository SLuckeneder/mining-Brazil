

library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

if (!dir.exists("figures/main_text")){dir.create("figures/main_text", recursive = TRUE)}

# Figure 2 ----------------------------------------------------------------

# read data
if(! file.exists("data/full_data_2000-2020_5y.csv")){source("R/00_compile_data.R")}
full_data <- read.csv2(file = paste0("data/full_data_2000-2020_5y.csv"), sep = ",", stringsAsFactors = FALSE) # read full data matrix
full_data <- full_data %>% dplyr::mutate_at(c(4:121), as.numeric)

# selected municipalities in balanced panel (5506 municipality ids)
if(!file.exists(paste0("data/raw/geobr/selected_mun.Rdata"))){source("R/01_base_maps.R")}
load("data/raw/geobr/selected_mun.Rdata")

# World Bank GDP deflator
if(! file.exists("data/raw/worldbank/wb_GDP_deflator.csv")){source("R/01_worldbank_data.R")}
GDP_deflator <- read.csv("data/raw/worldbank/wb_GDP_deflator.csv", skip = 4) %>%
  dplyr::filter(Country.Name == "Brazil") %>%
  dplyr::select(-c(1:4)) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::filter(!is.na(V1))
GDP_deflator <- GDP_deflator %>%
  dplyr::mutate(year = as.numeric(substr(rownames(GDP_deflator), 2, 5))) %>%
  dplyr::rename(gdp_deflator = V1) 

# World Bank commodity price index
if(! file.exists("data/raw/worldbank/wb_commodity_prices.xlsx")){source("R/01_worldbank_data.R")}
wb_commodity_prices <- readxl::read_excel("data/raw/worldbank/wb_commodity_prices.xlsx", sheet = "Monthly Indices", skip = 9) 
colnames(wb_commodity_prices)[1] <- "year_month"
wb_commodity_prices <- wb_commodity_prices%>%
  dplyr::mutate(year = substr(year_month, 1, 4),
                date = paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")) %>%
  dplyr::filter(date < "2020-01-02")  %>%
  dplyr::select(date, year, iMETMIN) %>%
  dplyr::rename("Metals and minerals price index (2010 = 100)" = iMETMIN) %>%
  tidyr::gather(key = "key", value = "value", -date, -year) %>% na.omit() %>%
  dplyr::mutate(date = as.Date(date))

# gdp and mining data
tidy_data <- full_data %>%
  dplyr::select(cod_municipio_long, year, gdp_capita, mining) %>%
  dplyr::left_join(GDP_deflator) %>%
  dplyr::filter(cod_municipio_long %in% selected_mun) %>%
  dplyr::arrange(year, cod_municipio_long) %>%
  dplyr::group_by(cod_municipio_long) %>%
  dplyr::mutate(gdp_capita_growth = (gdp_capita - lag(gdp_capita)) / lag(gdp_capita) * 100) %>%
  dplyr::mutate(gdp_capita_growth_real = gdp_capita_growth - gdp_deflator) %>%
  dplyr::mutate(mining = ifelse(mining > 0 , TRUE, FALSE))

# Prepare data for plotting: compute median
p_dat <- tidy_data %>%
  dplyr::mutate(date = paste0(year, "-01-01")) %>%
  dplyr::group_by(date, mining) %>%
  dplyr::summarize(gdp_capita_growth_real = median(gdp_capita_growth_real)) %>%
  dplyr::mutate(date = as.Date(date))

# Add World Bank commodity price index
p_dat <- p_dat %>% dplyr::right_join(wb_commodity_prices, by = "date") %>%
  dplyr::rename(price_index = value)

p_dat <- p_dat %>% 
  dplyr::mutate(mining = ifelse(mining == TRUE, "Mining municipalities (right axis)", "Non-mining municipalities (right axis)"))

conc <- p_dat %>% 
  dplyr::group_by(date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(d = row_number()) %>%
  dplyr::select(date, d)

p_dat <- p_dat %>% dplyr::left_join(conc) %>% dplyr::mutate(d = as.numeric(d))

bounds <- p_dat %>% 
  dplyr::ungroup() %>%
  dplyr::select(date, mining, gdp_capita_growth_real) %>%
  dplyr::filter(!is.na(mining)) %>%
  tidyr::spread(key = "mining", value = "gdp_capita_growth_real") %>%
  dplyr::mutate(
    ymax = pmax(`Mining municipalities (right axis)`, `Non-mining municipalities (right axis)`),
    ymin = pmin(`Mining municipalities (right axis)`, `Non-mining municipalities (right axis)`),
    fill = `Mining municipalities (right axis)` >= `Non-mining municipalities (right axis)`
  )

intervals <- bounds %>%
  filter(ymax > ymin) %>%
  dplyr::left_join(conc)

other_intersections <- bounds %>%
  dplyr::left_join(conc) %>%
  transmute(
    x1 = d,       y1 = `Mining municipalities (right axis)`,
    x2 = lead(d), y2 = lead(`Mining municipalities (right axis)`),
    x3 = d,       y3 = `Non-mining municipalities (right axis)`,
    x4 = lead(d), y4 = lead(`Non-mining municipalities (right axis)`)
  ) %>%
  filter(((y1 > y3) & (y2 < y4)) | ((y1 < y3) & (y2 > y4)))

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
  mutate(other_intersections, fill = TRUE),
  mutate(other_intersections, fill = FALSE)
) %>%
  arrange(date)

p_gdp <- p_dat %>%
  dplyr::filter(date > "2002-12-31" & date < "2020-01-02") %>%
  ggplot2::ggplot(aes(x = d)) +
  ggplot2::geom_hline(yintercept=0, color = "gray", linetype = 2, size = 1) +
  ggplot2::geom_line(aes(y = gdp_capita_growth_real*10, group = mining, colour = mining), size = 1) +
  ggplot2::geom_line(aes(y = price_index, colour = key), size = 1) +
  ggplot2::geom_ribbon(data = ribbons, aes(ymin = ymin*10, ymax = ymax*10, fill = fill), alpha = 0.1) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(6), limits = c(-50, 130), expand = c(0, 0),
    name = "Metals and minerals price index",
    sec.axis = sec_axis(~./10, name="GDP/capita growth (annual %, real BRL)")) +
  scale_x_continuous(breaks=c(529, 553, 577, 601, 625, 649, 673, 697, 721), labels=c("2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020"), limits = c(516,721), expand = c(0, 0)) +
  ggplot2::scale_color_manual(values = c("black", viridis::plasma(7)[c(2, 6)]), na.translate = F) +
  ggplot2::scale_fill_manual(values = viridis::plasma(7)[c(6, 2)], na.translate = F) +
  ggplot2::labs(title = NULL, x = NULL, color = NULL) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14, vjust = 0),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 legend.position = c(0.22, 0.15),
                 legend.background = element_rect(fill='transparent'),
                 plot.margin = margin(unit(c(5.5, 12, 5.5, 5.5), "points")))


# compute differences between mining and non-mining for mention in manuscript text
period <- c(2004:2011)
p_dat %>% dplyr::filter(year %in% period, !is.na(mining)) %>%
  dplyr::group_by(mining) %>%
  dplyr::summarise(mean_growth = mean(gdp_capita_growth_real)) %>%
  tidyr::spread(key = "mining", value = "mean_growth") %>%
  dplyr::mutate(difference_perc_pts = `Mining municipalities (right axis)` - `Non-mining municipalities (right axis)`)
period <- c(2004)
p_dat %>% dplyr::filter(year %in% period, !is.na(mining)) %>%
  tidyr::spread(key = "mining", value = "gdp_capita_growth_real") %>%
  dplyr::mutate(difference_perc_pts = `Mining municipalities (right axis)` - `Non-mining municipalities (right axis)`)
period <- c(2010)
p_dat %>% dplyr::filter(year %in% period, !is.na(mining)) %>%
  tidyr::spread(key = "mining", value = "gdp_capita_growth_real") %>%
  dplyr::mutate(difference_perc_pts = `Mining municipalities (right axis)` - `Non-mining municipalities (right axis)`)
period <- c(2013:2016)
p_dat %>% dplyr::filter(year %in% period, !is.na(mining)) %>%
  dplyr::group_by(mining) %>%
  dplyr::summarise(mean_growth = mean(gdp_capita_growth_real)) %>%
  tidyr::spread(key = "mining", value = "mean_growth") %>%
  dplyr::mutate(difference_perc_pts = `Mining municipalities (right axis)` - `Non-mining municipalities (right axis)`)



# export ------------------------------------------------------------------

ggplot2::ggsave("figure_2.png",
                plot = p_gdp, device = "png",
                path = paste0("./figures/main_text"),
                scale = 1, width = 300, height = 100, units = "mm")

