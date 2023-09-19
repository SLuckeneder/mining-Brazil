
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

if (!dir.exists("figures/SI")){dir.create("figures/SI", recursive = TRUE)}


# some preparations for all impact charts
variables_industrial_yearly <- c("mining_industrial_2005", "mining_industrial_2006", "mining_industrial_2007", "mining_industrial_2008", "mining_industrial_2009", "mining_industrial_2010", 
                                 "mining_industrial_2011", "mining_industrial_2012", "mining_industrial_2013", "mining_industrial_2014", "mining_industrial_2015", "mining_industrial_2016")
variables_industrial_pooled <- c("mining_industrial_x_pre_2010", "mining_industrial_x_since_2010")
variables_garimpo_yearly <- c("mining_garimpo_2005", "mining_garimpo_2006", "mining_garimpo_2007", "mining_garimpo_2008", "mining_garimpo_2009", "mining_garimpo_2010", 
                              "mining_garimpo_2011", "mining_garimpo_2012", "mining_garimpo_2013", "mining_garimpo_2014", "mining_garimpo_2015", "mining_garimpo_2016")
variables_garimpo_pooled <- c("mining_garimpo_x_pre_2010", "mining_garimpo_x_since_2010")
variables_yearly <- c(variables_industrial_yearly, variables_garimpo_yearly)
variables_pooled <- c(variables_industrial_pooled, variables_garimpo_pooled)


# F2 3-year growth windows ------------------------------------------------

impacts_yearly <- list.files("data/impact_estimates/summaries/SI/", pattern = "gdp_yearly_2005-2016_3y")
impacts_pooled <- list.files("data/impact_estimates/summaries/SI/", pattern = "gdp_pooled_2005-2016_3y")

### yearly estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/SI/", impacts_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Impact of industrial and garimpo mining on GDP per capita growth (3-year window)") 

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

p_gdp3y_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Type)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.4, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "GDP impact estimate") +
  ggplot2::scale_y_continuous(limits = c(-4.9, 6.2), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(5)[c(1, 3)]) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.82, 0.88),
                 legend.direction = "horizontal",
                 legend.spacing.y = unit(-11, "pt"),
                 legend.title =  element_blank(),   
                 legend.background = element_rect(fill='transparent'),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")



### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/SI/", impacts_pooled))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_pooled) %>%
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

p_gdp3y_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Type)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-4.9, 6.2), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(5)[c(1, 3)]) +
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




# F2 7-year growth windows ------------------------------------------------

impacts_yearly <- list.files("data/impact_estimates/summaries/SI/", pattern = "gdp_yearly_2005-2013_7y")
impacts_pooled <- list.files("data/impact_estimates/summaries/SI/", pattern = "gdp_pooled_2005-2013_7y")

### yearly estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/SI/", impacts_yearly))

p_dat_temp <- data.frame("Variable" = c("mining_industrial_2014", "mining_industrial_2015", "mining_industrial_2016"))
p_dat <- dplyr::bind_rows(p_dat, p_dat_temp); rm(p_dat_temp)

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Impact of industrial and garimpo mining on GDP per capita growth (7-year window)") 

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

p_gdp7y_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Type)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.4, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "GDP impact estimate") +
  ggplot2::scale_y_continuous(limits = c(-1.9, 2.5), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(5)[c(1, 3)]) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.87, 0.76),
                 legend.direction = "vertical",
                 legend.spacing.y = unit(-3, "pt"),
                 legend.title =  element_blank(),   
                 legend.background = element_rect(fill='transparent'),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")



### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/impact_estimates/summaries/SI/", impacts_pooled))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_pooled) %>%
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

p_gdp7y_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Type)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-1.9, 2.5), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(5)[c(1, 3)]) +
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


# merge -------------------------------------------------------------------

### year and pre/post 2010 divide
p_gdp3y <- cowplot::plot_grid(p_gdp3y_year, p_gdp3y_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
p_gdp7y <- cowplot::plot_grid(p_gdp7y_year, p_gdp7y_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

p_merge <- cowplot::plot_grid(p_gdp3y, p_gdp7y, nrow = 2, rel_heights = c(1/2, 1/2), labels = c("a", "b"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_growth_windows.png",
                plot = p_merge, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 350, height = 250, units = "mm")
