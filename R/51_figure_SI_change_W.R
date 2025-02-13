
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


# growth impacts ----------------------------------------------------------


# W4 industrial
impacts_gdp_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_yearly_industrial_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_gdp_yearly))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")

# W7 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_yearly_industrial_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")

# W10 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_yearly_industrial_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")

p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover k = 4", "Spillover k = 7", "Spillover k = 10"))

p_gdp_year_industrial <- p_dat %>%
  dplyr::filter(Type == "Industrial") %>%
  dplyr::mutate(Model = "Effect of industrial mining on GDP per capita growth") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-3.1, 5.3), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.7, 0.85),
                 legend.spacing.y = unit(-11, "pt"),
                 legend.direction = "horizontal",
                 legend.title =  element_blank(),   
                 legend.background = element_rect(fill='transparent'),
                 legend.key.width = unit(1,"cm"),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


# W4 garimpo
impacts_gdp_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_yearly_garimpo_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_gdp_yearly))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")

# W7 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_yearly_garimpo_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")

# W10 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_yearly_garimpo_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")

p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_gdp_year_garimpo <- p_dat %>%
  dplyr::filter(Type == "Garimpo") %>%
  dplyr::mutate(Model = "Effect of garimpo mining on GDP per capita growth") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-4, 5), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_color_manual(name = NULL, values =  viridis::viridis(10)[c(1, 8)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.33, 0.12),
                 legend.spacing.y = unit(-11, "pt"),
                 legend.direction = "horizontal",
                 legend.title =  element_blank(),   
                 legend.key.width = unit(1,"cm"),
                 legend.background = element_rect(fill='transparent'),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")



### pre/post 2010 estimates industrial
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_pooled_industrial_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "pooled") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")



impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_pooled_industrial_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")


impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_pooled_industrial_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")


p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_gdp_divide_industrial <- p_dat %>%
  dplyr::filter(Type == "Industrial") %>%
  dplyr::mutate(Model = "Pooled effects") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-3.1, 5.3), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
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

### pre/post 2010 estimates garimpo
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_pooled_garimpo_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "pooled") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")



impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_pooled_garimpo_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")


impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "gdp_pooled_garimpo_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "GDP per capita ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")


p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_gdp_divide_garimpo <- p_dat %>%
  dplyr::filter(Type == "Garimpo") %>%
  dplyr::mutate(Model = "Pooled effects") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-4, 5), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
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



# forest loss impacts relative ----------------------------------------------

# W4 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_yearly_industrial_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")

# W7 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_yearly_industrial_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")

# W10 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_yearly_industrial_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")

p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_rel_year_industrial <- p_dat %>%
  dplyr::filter(Type == "Industrial") %>%
  dplyr::mutate(Model = "Effect of industrial mining on forest loss (ha/km2)") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.4, 0.3), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position =c(0.5, 0.85),
                 legend.spacing.y = unit(-11, "pt"),
                 legend.direction = "horizontal",
                 legend.title =  element_blank(),   
                 legend.background = element_rect(fill='transparent'),
                 legend.key.width = unit(1,"cm"),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


# W4 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_yearly_garimpo_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")

# W7 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_yearly_garimpo_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")

# W10 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_yearly_garimpo_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")

p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_rel_year_garimpo <- p_dat %>%
  dplyr::filter(Type == "Garimpo") %>%
  dplyr::mutate(Model = "Effect of garimpo mining on forest loss (ha/km2)") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.55, 0.8), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_color_manual(name = NULL, values = viridis::inferno(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.63, 0.82),
                 legend.spacing.y = unit(-11, "pt"),
                 legend.direction = "horizontal",
                 legend.title =  element_blank(),   
                 legend.key.width = unit(1,"cm"),
                 legend.background = element_rect(fill='transparent'),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")



### pre/post 2010 estimates industrial
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_pooled_industrial_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "pooled") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")



impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_pooled_industrial_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")


impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_pooled_industrial_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")


p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_rel_divide_industrial <- p_dat %>%
  dplyr::filter(Type == "Industrial") %>%
  dplyr::mutate(Model = "Pooled effects") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.4, 0.3), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
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


### pre/post 2010 estimates garimpo
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_pooled_garimpo_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "pooled") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")



impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_pooled_garimpo_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")


impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "rel_pooled_garimpo_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")


p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_rel_divide_garimpo <- p_dat %>%
  dplyr::filter(Type == "Garimpo") %>%
  dplyr::mutate(Model = "Pooled effects") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10)), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10), group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4, ymax= CI.97.5.W4), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7, ymax= CI.97.5.W7), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10, ymax= CI.97.5.W10), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.55, 0.8), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
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



# forest loss impacts absolute --------------------------------------------


# W4 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_yearly_industrial_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")

# W7 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_yearly_industrial_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")

# W10 industrial
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_yearly_industrial_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")

p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_abs_year_industrial <- p_dat %>%
  dplyr::filter(Type == "Industrial") %>%
  dplyr::mutate(Model = "Effect of industrial mining on forest loss (thousand ha)") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4 / 1000, ymax= CI.97.5.W4 / 1000), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7 / 1000, ymax= CI.97.5.W7 / 1000), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10 / 1000, ymax= CI.97.5.W10 / 1000), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.8, 0.8), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position =c(0.6, 0.8),
                 legend.spacing.y = unit(-11, "pt"),
                 legend.direction = "horizontal",
                 legend.title =  element_blank(),   
                 legend.background = element_rect(fill='transparent'),
                 legend.key.width = unit(1,"cm"),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")


# W4 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_yearly_garimpo_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")

# W7 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_yearly_garimpo_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")

# W10 garimpo
impacts_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_yearly_garimpo_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_yearly))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_yearly) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "relative area (ha/km2) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = stringi::stri_sub(Variable, -4, -1),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")

p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_abs_year_garimpo <- p_dat %>%
  dplyr::filter(Type == "Garimpo") %>%
  dplyr::mutate(Model = "Effect of garimpo mining on forest loss (thousand ha)") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4 / 1000, ymax= CI.97.5.W4 / 1000), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7 / 1000, ymax= CI.97.5.W7 / 1000), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10 / 1000, ymax= CI.97.5.W10 / 1000), width=.4,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.5, 4.5), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = "Effect estimate and 95% CI") +
  ggplot2::scale_color_manual(name = NULL, values = viridis::mako(8)[c(1, 5)]) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.65, 0.8),
                 legend.spacing.y = unit(-11, "pt"),
                 legend.direction = "horizontal",
                 legend.title =  element_blank(),   
                 legend.key.width = unit(1,"cm"),
                 legend.background = element_rect(fill='transparent'),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"),
                 plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt")) +
  ggplot2::labs(caption = " ")



### pre/post 2010 estimates industrial
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_pooled_industrial_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "pooled") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "absolute area (thousand ha) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")



impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_pooled_industrial_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "absolute area (thousand ha) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")


impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_pooled_industrial_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "absolute area (thousand ha) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")


p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_abs_divide_industrial <- p_dat %>%
  dplyr::filter(Type == "Industrial") %>%
  dplyr::mutate(Model = "Pooled effects") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4 / 1000, ymax= CI.97.5.W4 / 1000), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7 / 1000, ymax= CI.97.5.W7 / 1000), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10 / 1000, ymax= CI.97.5.W10 / 1000), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.8, 0.8), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
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


### pre/post 2010 estimates garimpo
impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_pooled_garimpo_baseline_2005-2015_5y_W4")
p_dat_W4 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W4 <- p_dat_W4 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "pooled") %>%
  dplyr::mutate(unit = "k = 4") %>%
  dplyr::mutate(model = "absolute area (thousand ha) ~ mining binary") 

p_dat_W4 <- p_dat_W4 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W4) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W4", "CI.2.5.W4", "CI.5.W4", "CI.95.W4", "CI.97.5.W4", "CI.99.W4", "Mean.W4", "Type")



impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_pooled_garimpo_baseline_2005-2015_5y_W7")
p_dat_W7 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W7 <- p_dat_W7 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 7") %>%
  dplyr::mutate(model = "absolute area (thousand ha) ~ mining binary") 

p_dat_W7 <- p_dat_W7 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = as.character(Variable),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W7) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W7", "CI.2.5.W7", "CI.5.W7", "CI.95.W7", "CI.97.5.W7", "CI.99.W7", "Mean.W7", "Type")


impacts_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/SI/", pattern = "abs_pooled_garimpo_baseline_2005-2015_5y_W10")
p_dat_W10 <- read.csv(file = paste0("data/cem/sdm_impact_estimates/summaries/SI/", impacts_pooled))

p_dat_W10 <- p_dat_W10 %>% dplyr::filter(Variable %in% variables_pooled) %>%
  dplyr::mutate(freq = "yearly") %>%
  dplyr::mutate(unit = "k = 10") %>%
  dplyr::mutate(model = "absolute area (thousand ha) ~ mining binary") 

p_dat_W10 <- p_dat_W10 %>% tidyr::gather(key = "CI", value = "value", -Variable, -freq, -unit, -model) %>%
  tidyr::separate(CI, c("impact", "CI"), "\\.") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  tidyr::spread(key = "CI", value = "value") %>%
  dplyr::mutate(Type = substr(Variable, 8, 11),
                Type = ifelse(Type == "gari", "Garimpo", "Industrial"),
                Variable = ifelse(Variable == "mining_industrial_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_pre_2010", "Pre 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_industrial_x_since_2010", "Since 2010", as.character(Variable)),
                Variable = ifelse(Variable == "mining_garimpo_x_since_2010", "Since 2010", as.character(Variable)),
                impact = ifelse(impact == "Direct", "Effect in mining municipalities", "Spillover effect"))

colnames(p_dat_W10) <- c("Variable", "Pool", "Unit", "Model", "Impact", "CI.1.W10", "CI.2.5.W10", "CI.5.W10", "CI.95.W10", "CI.97.5.W10", "CI.99.W10", "Mean.W10", "Type")


p_dat <- dplyr::bind_rows(p_dat_W4, p_dat_W7, p_dat_W10) %>%
  dplyr::mutate(gr = paste(Impact, Unit))

p_dat$Unit <- factor(p_dat$Unit, levels = c("k = 4", "k = 7", "k = 10"))
p_dat$gr <- factor(p_dat$gr, levels = c("Effect in mining municipalities k = 4", "Effect in mining municipalities k = 7", "Effect in mining municipalities k = 10",
                                        "Spillover effect k = 4", "Spillover effect k = 7", "Spillover effect k = 10"))

p_def_abs_divide_garimpo <- p_dat %>%
  dplyr::filter(Type == "Garimpo") %>%
  dplyr::mutate(Model = "Pooled effects") %>%
  ggplot2::ggplot(aes(x = Variable, color = Impact, linetype = Unit)) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W4) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W4) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W7) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W7) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  ggplot2::geom_point(aes(y = as.numeric(Mean.W10) / 1000), position=position_dodge(.5), size = 2) +
  ggplot2::geom_line(aes(y = as.numeric(Mean.W10) / 1000, group = gr), position=position_dodge(.5), size = 1) +
  
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W4 / 1000, ymax= CI.97.5.W4 / 1000), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W7 / 1000, ymax= CI.97.5.W7 / 1000), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::geom_errorbar(aes(ymin= CI.2.5.W10 / 1000, ymax= CI.97.5.W10 / 1000), width=.2,
                         position=position_dodge(.5), size = 1) +
  ggplot2::scale_y_continuous(limits = c(-0.5, 4.5), expand = c(0, 0)) +
  ggplot2::geom_hline(yintercept=0) +
  ggplot2::facet_wrap(Model~., scales = "free_y") +
  ggplot2::labs(x = NULL, y = NULL) +
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



# merge -------------------------------------------------------------------

### year and pre/post 2010 divide
p_gdp_industrial <- cowplot::plot_grid(p_gdp_year_industrial, p_gdp_divide_industrial, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
p_gdp_garimpo <- cowplot::plot_grid(p_gdp_year_garimpo, p_gdp_divide_garimpo, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

p_def_rel_industrial <- cowplot::plot_grid(p_def_rel_year_industrial, p_def_rel_divide_industrial, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
p_def_rel_garimpo <- cowplot::plot_grid(p_def_rel_year_garimpo, p_def_rel_divide_garimpo, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))

p_def_abs_industrial <- cowplot::plot_grid(p_def_abs_year_industrial, p_def_abs_divide_industrial, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
p_def_abs_garimpo <- cowplot::plot_grid(p_def_abs_year_garimpo, p_def_abs_divide_garimpo, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))


p_merge_industrial <- cowplot::plot_grid(p_gdp_industrial, p_def_rel_industrial, p_def_abs_industrial, nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("a", "b", "c"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_change_W_industrial.png",
                plot = p_merge_industrial, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 300, height = 300, units = "mm")

p_merge_garimpo <- cowplot::plot_grid(p_gdp_garimpo, p_def_rel_garimpo, p_def_abs_garimpo, nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("a", "b", "c"), label_size = 18) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_change_W_garimpo.png",
                plot = p_merge_garimpo, device = "png",
                path = paste0("./figures/SI"),
                scale = 1, width = 300, height = 300, units = "mm")


# for thesis --------------------------------------------------------------


# ### year and pre/post 2010 divide
# p_gdp_industrial <- cowplot::plot_grid(p_gdp_year_industrial, p_gdp_divide_industrial, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# p_gdp_garimpo <- cowplot::plot_grid(p_gdp_year_garimpo, p_gdp_divide_garimpo, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# 
# p_def_rel_industrial <- cowplot::plot_grid(p_def_rel_year_industrial, p_def_rel_divide_industrial, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# p_def_rel_garimpo <- cowplot::plot_grid(p_def_rel_year_garimpo, p_def_rel_divide_garimpo, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# 
# p_def_abs_industrial <- cowplot::plot_grid(p_def_abs_year_industrial, p_def_abs_divide_industrial, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# p_def_abs_garimpo <- cowplot::plot_grid(p_def_abs_year_garimpo, p_def_abs_divide_garimpo, align = "h", nrow = 1, rel_widths = c(3/4, 1/4))
# 
# 
# p_merge_industrial <- cowplot::plot_grid(p_gdp_industrial, p_def_rel_industrial, p_def_abs_industrial, nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("A", "B", "C"), label_size = 18) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("figure_SI_change_W_industrial.png",
#                 plot = p_merge_industrial, device = "png",
#                 path = paste0("./figures/thesis"),
#                 scale = 1, width = 300, height = 300, units = "mm")
# 
# p_merge_garimpo <- cowplot::plot_grid(p_gdp_garimpo, p_def_rel_garimpo, p_def_abs_garimpo, nrow = 3, rel_heights = c(1/3, 1/3, 1/3), labels = c("A", "B", "C"), label_size = 18) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("figure_SI_change_W_garimpo.png",
#                 plot = p_merge_garimpo, device = "png",
#                 path = paste0("./figures/thesis"),
#                 scale = 1, width = 300, height = 300, units = "mm")

