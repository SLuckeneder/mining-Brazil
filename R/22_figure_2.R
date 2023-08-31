

library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

if (!dir.exists("figures/main_text")){dir.create("figures/main_text", recursive = TRUE)}

# Figure 2a ---------------------------------------------------------------

# mining data
load("data/intermediary/mapbiomas_mine_data.Rdata")

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

# gdp data
load("data/intermediary/ibge_econ.Rdata")
# load("data/intermediary/ibge_pop.Rdata")
ibge_econ_tidy <- ibge_econ %>%
  dplyr::left_join(GDP_deflator) %>%
  dplyr::filter(cod_municipio_long %in% selected_mun) %>%
  dplyr::select(cod_municipio_long, year, unid, gdp_capita, gdp_deflator) %>%
  dplyr::arrange(year, cod_municipio_long) %>%
  dplyr::group_by(cod_municipio_long) %>%
  dplyr::mutate(gdp_capita_growth = (gdp_capita - lag(gdp_capita)) / lag(gdp_capita) * 100) %>%
  dplyr::mutate(gdp_capita_growth_real = gdp_capita_growth - gdp_deflator) 

# add mining
ibge_econ_mining <- ibge_econ_tidy %>%
  dplyr::left_join(mine_mun_panel %>%
                     dplyr::mutate(unid = paste(code_mn, year, sep = "_")) %>%
                     dplyr::select(unid, mining) %>%
                     dplyr::rename(mining_area = mining),
                   by = "unid") %>%
  dplyr::mutate(mining = ifelse(mining_area > 0 , TRUE, FALSE))

# Prepare data for plotting: compute median
p_dat <- ibge_econ_mining %>%
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



# Figure 2b industrial mining impact estimates ----------------------------

# some preparations for all impact charts
variables_industrial_yearly <- c("mining_industrial_2005", "mining_industrial_2006", "mining_industrial_2007", "mining_industrial_2008", "mining_industrial_2009", "mining_industrial_2010", 
               "mining_industrial_2011", "mining_industrial_2012", "mining_industrial_2013", "mining_industrial_2014", "mining_industrial_2015")
variables_industrial_pooled <- c("mining_industrial_x_pre_2010", "mining_industrial_x_since_2010")
variables_garimpo_yearly <- c("mining_garimpo_2005", "mining_garimpo_2006", "mining_garimpo_2007", "mining_garimpo_2008", "mining_garimpo_2009", "mining_garimpo_2010", 
                                 "mining_garimpo_2011", "mining_garimpo_2012", "mining_garimpo_2013", "mining_garimpo_2014", "mining_garimpo_2015")
variables_garimpo_pooled <- c("mining_garimpo_x_pre_2010", "mining_garimpo_x_since_2010")
impacts_gdp_yearly <- list.files("data/impact_estimates/summaries/", pattern = "gdp_yearly")
impacts_gdp_pooled <- list.files("data/impact_estimates/summaries/", pattern = "gdp_pooled")

### yearly estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/", impacts_gdp_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Impact of industrial mining on GDP per capita growth") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Direct impact", "Indirect impact")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Direct impact Industrial", "Direct impact Garimpo", "Indirect impact Industrial", "Indirect impact Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Impact estimate") +
  ggplot2::scale_y_continuous(limits = c(-3.9, 5.1), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.15, 0.2),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/", impacts_gdp_pooled))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_pooled) %>%
  dplyr::mutate(freq = "divide") %>%
  dplyr::mutate(unit = "Pooled effects") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable %in% c("mining_industrial_x_pre_2010", "mining_garimpo_x_pre_2010"), "Pre 2010", "Since 2010"),
                impact = ifelse(impact == "Direct", "Direct impact", "Indirect impact")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Direct impact Industrial", "Direct impact Garimpo", "Indirect impact Industrial", "Indirect impact Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-3.9, 5.1), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 legend.title =  element_blank(),
                 legend.position = "none",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14),
                 plot.margin = unit(c(5.5, 5.5, 5.5, 0), "pt")) +
  ggplot2::labs(caption = " ")

### merge
p_gdp_industrial <- cowplot::plot_grid(p_gdp_year, p_gdp_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))


# Figure 2c industrial mining impact estimates ----------------------------

### yearly estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/", impacts_gdp_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Impact of garimpo mining on GDP per capita growth") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Direct impact", "Indirect impact")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Direct impact Industrial", "Direct impact Garimpo", "Indirect impact Industrial", "Indirect impact Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Impact estimate") +
  ggplot2::scale_y_continuous(limits = c(-3.9, 4.7), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.8, 0.2),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/", impacts_gdp_pooled))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_pooled) %>%
  dplyr::mutate(freq = "divide") %>%
  dplyr::mutate(unit = "Pooled effects") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable %in% c("mining_industrial_x_pre_2010", "mining_garimpo_x_pre_2010"), "Pre 2010", "Since 2010"),
                impact = ifelse(impact == "Direct", "Direct impact", "Indirect impact")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Direct impact Industrial", "Direct impact Garimpo", "Indirect impact Industrial", "Indirect impact Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-3.9, 4.7), expand = c(0, 0)) +  
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 legend.title =  element_blank(),
                 legend.position = "none",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14),
                 plot.margin = unit(c(5.5, 5.5, 5.5, 0), "pt")) +
  ggplot2::labs(caption = " ")


### merge
p_gdp_garimpo <- cowplot::plot_grid(p_gdp_year, p_gdp_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))


# merge -------------------------------------------------------------------

p_merge <- cowplot::plot_grid(p_gdp, p_gdp_industrial, p_gdp_garimpo, nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("a", "b", "c"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_2.png",
                plot = p_merge, device = "png",
                path = paste0("./figures/main_text"),
                scale = 1, width = 300, height = 300, units = "mm")

