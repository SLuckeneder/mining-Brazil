
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

if (!dir.exists("figures/main_text")){dir.create("figures/main_text", recursive = TRUE)}

# some preparations for all impact charts
variables_industrial_yearly <- c("mining_industrial_2005", "mining_industrial_2006", "mining_industrial_2007", "mining_industrial_2008", "mining_industrial_2009", "mining_industrial_2010", 
                                 "mining_industrial_2011", "mining_industrial_2012", "mining_industrial_2013", "mining_industrial_2014", "mining_industrial_2015")
variables_industrial_pooled <- c("mining_industrial_x_pre_2010", "mining_industrial_x_since_2010")
variables_garimpo_yearly <- c("mining_garimpo_2005", "mining_garimpo_2006", "mining_garimpo_2007", "mining_garimpo_2008", "mining_garimpo_2009", "mining_garimpo_2010", 
                              "mining_garimpo_2011", "mining_garimpo_2012", "mining_garimpo_2013", "mining_garimpo_2014", "mining_garimpo_2015")
variables_garimpo_pooled <- c("mining_garimpo_x_pre_2010", "mining_garimpo_x_since_2010")

# industrial mining impact estimates (relative) ---------------------------

impacts_def_rel_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_yearly_industrial_baseline_")
impacts_def_rel_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_pooled_industrial_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_rel_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of industrial mining on forest loss (ha/km2)") 

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

p_relative_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_y_continuous(limits = c(-0.21, 0.15), expand = c(0, 0), breaks = scales::pretty_breaks(4)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.55, 0.85),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_rel_pooled))

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

p_relative_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.21, 0.15), expand = c(0, 0), breaks = scales::pretty_breaks(4)) +
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
p_relative_industrial <- cowplot::plot_grid(p_relative_year, p_relative_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))


# garimpo mining impact estimates (relative) -----------------------------------------

impacts_def_rel_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_yearly_garimpo_baseline_")
impacts_def_rel_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_pooled_garimpo_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_rel_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of garimpo mining on forest loss (ha/km2)") 

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

p_relative_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_y_continuous(limits = c(-0.31, 0.6), expand = c(0, 0), breaks = scales::pretty_breaks(6)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.55, 0.85),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_rel_pooled))

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

p_relative_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5, ymax= CI.97.5), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.31, 0.6), expand = c(0, 0), breaks = scales::pretty_breaks(6)) +
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
p_relative_garimpo <- cowplot::plot_grid(p_relative_year, p_relative_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))


# industrial mining impact estimates (absolute) ---------------------------

impacts_def_abs_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_yearly_industrial_baseline_")
impacts_def_abs_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_pooled_industrial_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_industrial_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of industrial mining on forest loss (thousand ha)") 

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

p_absolute_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean) / 1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5 / 1000, ymax= CI.97.5 / 1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_y_continuous(limits = c(-0.45, 0.45), breaks = scales::pretty_breaks(5), expand = c(0, 0)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.5, 0.85),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_pooled))

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

p_absolute_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)/1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean)/1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5/1000, ymax= CI.97.5/1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.45, 0.45), breaks = scales::pretty_breaks(5), expand = c(0, 0)) +
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
p_absolute_industrial <- cowplot::plot_grid(p_absolute_year, p_absolute_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))


# garimpo mining impact estimates (absolute) -----------------------------------------

impacts_def_abs_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_yearly_garimpo_baseline_")
impacts_def_abs_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_pooled_garimpo_baseline_")

### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of garimpo mining on forest loss (thousand ha)") 

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

p_absolute_year <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)/1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean)/1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5/1000, ymax= CI.97.5/1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_y_continuous(limits = c(-0.5, 2.9), expand = c(0, 0), breaks = scales::pretty_breaks(5)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.7, 0.8),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")



### pre/post 2010 estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_pooled))

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

p_absolute_divide <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)/1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean)/1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5/1000, ymax= CI.97.5/1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(limits = c(-0.5, 2.9), expand = c(0, 0), breaks = scales::pretty_breaks(5)) +
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
p_absolute_garimpo <- cowplot::plot_grid(p_absolute_year, p_absolute_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

# for presentation
### yearly estimates
p_dat <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_yearly))

p_dat <- p_dat %>% dplyr::filter(Variable %in% variables_garimpo_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "Effect of garimpo mining on forest loss (thousand ha)") 

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

p_absolute_year_pres <- p_dat %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::geom_point(aes(y = as.numeric(Mean)/1000), position=position_dodge(.5), size = 3) +
  ggplot2::geom_line(aes(y = as.numeric(Mean)/1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5/1000, ymax= CI.97.5/1000), width=.2, 
                         position=position_dodge(.5), size = 1) +
  ggplot2::facet_wrap(Unit~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_y_continuous(limits = c(-0.5, 2.9), expand = c(0, 0), breaks = scales::pretty_breaks(5)) +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.8, 0.8),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")
p_absolute_garimpo_pres <- cowplot::plot_grid(p_absolute_year_pres, p_absolute_divide, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

# merge -------------------------------------------------------------------

p_merge <- cowplot::plot_grid(p_relative_industrial, p_relative_garimpo, p_absolute_industrial, p_absolute_garimpo, 
                              nrow = 4, rel_heights = c(1/4, 1/4, 1/4, 1/4), labels = c("a", "b", "c", "d"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_5.png",
                plot = p_merge, device = "png",
                path = paste0("./figures/main_text"),
                scale = 1, width = 300, height = 400, units = "mm")


# # for presentation --------------------------------------------------------
# 
# if (!dir.exists("figures/presentations")){dir.create("figures/presentations", recursive = TRUE)}
# 
# ggplot2::ggsave("figure_5d.png",
#                 plot = p_absolute_garimpo_pres, device = "png",
#                 path = paste0("./figures/presentations"),
#                 scale = 1, width = 300, height = 100, units = "mm")
# 
# 
# # for thesis --------------------------------------------------------------
# 
# p_merge <- cowplot::plot_grid(p_relative_industrial, p_relative_garimpo, p_absolute_industrial, p_absolute_garimpo, 
#                               nrow = 4, rel_heights = c(1/4, 1/4, 1/4, 1/4), labels = c("A", "B", "C", "D"), label_size = 20) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("figure_5.png",
#                 plot = p_merge, device = "png",
#                 path = paste0("./figures/thesis"),
#                 scale = 1, width = 300, height = 400, units = "mm")
# 
# # for defensio ------------------------------------------------------------
# 
# p_merge <- cowplot::plot_grid(p_relative_industrial, p_relative_garimpo,
#                               nrow = 2, label_size = 20) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("paper-2_results-2.pdf",
#                 plot = p_merge, device = "pdf",
#                 path = paste0("./figures/presentations"),
#                 scale = 1, width = 300, height = 200, units = "mm")
# 
# p_merge <- cowplot::plot_grid(p_absolute_industrial, p_absolute_garimpo, 
#                               nrow = 2, label_size = 20) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("paper-2_results-3.pdf",
#                 plot = p_merge, device = "pdf",
#                 path = paste0("./figures/presentations"),
#                 scale = 1, width = 300, height = 200, units = "mm")



