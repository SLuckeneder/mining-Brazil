
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

if (!dir.exists("figures/SI")){dir.create("figures/SI", recursive = TRUE)}


# some preparations for all impact charts
variables_industrial_yearly <- c("mining_industrial_2005", "mining_industrial_2006", "mining_industrial_2007", "mining_industrial_2008", "mining_industrial_2009", "mining_industrial_2010", 
                                 "mining_industrial_2011", "mining_industrial_2012", "mining_industrial_2013", "mining_industrial_2014", "mining_industrial_2015")
variables_industrial_pooled <- c("mining_industrial_x_pre_2010", "mining_industrial_x_since_2010")
variables_garimpo_yearly <- c("mining_garimpo_2005", "mining_garimpo_2006", "mining_garimpo_2007", "mining_garimpo_2008", "mining_garimpo_2009", "mining_garimpo_2010", 
                              "mining_garimpo_2011", "mining_garimpo_2012", "mining_garimpo_2013", "mining_garimpo_2014", "mining_garimpo_2015")
variables_garimpo_pooled <- c("mining_garimpo_x_pre_2010", "mining_garimpo_x_since_2010")



# S5 hectares industrial --------------------------------------------------

### GDP industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "gdp_yearly_hectares_industrial_baseline_")
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "gdp_pooled_hectares_industrial_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of industrial mining (thousand ha) on GDP per capita growth") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "GDP effect estimate") +
  ggplot2::scale_y_continuous(limits = c(-3.7, 7.6), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.2, 0.2),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_pooled))

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
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-3.7, 7.6), expand = c(0, 0)) +
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



### relative forest loss industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_yearly_hectares_industrial_baseline_")
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_pooled_hectares_industrial_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of industrial mining (ha/municipality area km2) on forest loss (ha/km2)") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Forest loss effect estimate") +
  ggplot2::scale_y_continuous(limits = c(-0.21, 0.51), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.6, 0.8),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_pooled))

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
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.21, 0.51), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) +
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
p_relative_industrial <- cowplot::plot_grid(p_deforestation_hect_year, p_deforestation_hect_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))



### absolute forest loss industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_yearly_hectares_industrial_baseline_")
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_pooled_hectares_industrial_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of industrial mining (ha) on forest loss (ha)") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_abs_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Forest loss effect estimate") +
  ggplot2::scale_y_continuous(limits = c(-0.4, 3.3), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.7, 0.8),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_pooled))

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
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_abs_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.4, 3.3), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
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
p_absolute_industrial <- cowplot::plot_grid(p_deforestation_hect_abs_year, p_deforestation_hect_abs_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

p_merge_industrial <- cowplot::plot_grid(p_gdp_industrial, p_relative_industrial, p_absolute_industrial, 
                                         nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("a", "b", "c"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_hectares_industrial.png",
                plot = p_merge_industrial, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 300, height = 300, units = "mm")

# # for thesis
# 
# p_absolute_industrial <- cowplot::plot_grid(p_deforestation_hect_abs_year, p_deforestation_hect_abs_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# 
# p_merge_industrial <- cowplot::plot_grid(p_gdp_industrial, p_relative_industrial, p_absolute_industrial, 
#                                          nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("A", "B", "C"), label_size = 18) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("figure_SI_hectares_industrial.png",
#                 plot = p_merge_industrial, device = "png",
#                 path = paste0("./figures/thesis"),
#                 scale = 1, width = 300, height = 300, units = "mm")






# F1 garimpo mining -------------------------------------------------------

### GDP garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "gdp_yearly_hectares_garimpo_baseline_")
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "gdp_pooled_hectares_garimpo_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of garimpo mining (thousand ha) on GDP per capita growth") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "GDP effect estimate") +
  ggplot2::scale_y_continuous(limits = c(-1.5, 3.3), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.75, 0.87),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_pooled))

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
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_gdp_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-1.5, 3.3), expand = c(0, 0)) +
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



### relative forest loss garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_yearly_hectares_garimpo_baseline_")
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_pooled_hectares_garimpo_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of garimpo mining (ha/municipality area km2) on forest loss (ha/km2)") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Forest loss effect estimate") +
  ggplot2::scale_y_continuous(limits = c(-0.3, 0.6), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.7, 0.8),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_pooled))

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
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) * 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) * 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 * 1000, ymax= CI.97.5 * 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.3, 0.6), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) +
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
p_relative_garimpo <- cowplot::plot_grid(p_deforestation_hect_year, p_deforestation_hect_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))



### absolute forest loss garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_yearly_hectares_garimpo_baseline_")
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_pooled_hectares_garimpo_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_yearly))


p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of garimpo mining (ha) on forest loss (ha)") 

p_dat <- p_dat %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringr::str_sub(Variable,-4,-1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_abs_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Forest loss effect estimate") +
  ggplot2::scale_y_continuous(limits = c(-0.6, 2.6), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.7, 0.8),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_pooled))

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
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect")) %>%
  dplyr::mutate(gr = paste(impact, Type))

p_dat$Type <- factor(p_dat$Type, levels = c("Industrial", "Garimpo"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities Industrial", "Effect in mining municipalities Garimpo", "Spillover effect Industrial", "Spillover effect Garimpo"))

colnames(p_dat) <- c("Variable", "Pool", "Unit", "Impact", "CI.1", "CI.2.5", "CI.5", "CI.95", "CI.97.5", "CI.99", "Mean", "Type", "gr")

p_deforestation_hect_abs_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.6, 2.6), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
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
p_absolute_garimpo <- cowplot::plot_grid(p_deforestation_hect_abs_year, p_deforestation_hect_abs_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

p_merge_garimpo <- cowplot::plot_grid(p_gdp_garimpo, p_relative_garimpo, p_absolute_garimpo, 
                                      nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("a", "b", "c"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_hectares_garimpo.png",
                plot = p_merge_garimpo, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 300, height = 300, units = "mm")




