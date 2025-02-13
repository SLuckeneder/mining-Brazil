

library(dplyr)
library(xtable)

# count observations (need to recall full data matrix and filter for 2005-2015)
# read matched data
M_matched_industrial <- read.csv(paste0("data/cem/cem_results/cem_baseline_data_industrial_2005-2020_5y.csv"))
M_matched_industrial <- M_matched_industrial %>% dplyr::filter(year %in% c(2005:2015))
observations <- data.frame("Variables" = "Observations", "2.5\\%" = nrow(M_matched_industrial), "PM" = NA, "97.5\\%" = NA, " 2.5\\%" = NA, " PM" = NA, " 97.5\\%" = NA)
colnames(observations) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# build concordance for variable names
raw_name <- c("mining_industrial_2005","mining_industrial_2006","mining_industrial_2007","mining_industrial_2008","mining_industrial_2009","mining_industrial_2010",
              "mining_industrial_2011","mining_industrial_2012","mining_industrial_2013","mining_industrial_2014","mining_industrial_2015",
              "mining_industrial_x_pre_2010","mining_industrial_x_since_2010",
              "mining_garimpo_2005","mining_garimpo_2006","mining_garimpo_2007","mining_garimpo_2008","mining_garimpo_2009","mining_garimpo_2010",
              "mining_garimpo_2011","mining_garimpo_2012","mining_garimpo_2013","mining_garimpo_2014","mining_garimpo_2015",
              "mining_garimpo_x_pre_2010","mining_garimpo_x_since_2010",
              "mav_Agriculture_Forest_Plantation_log","mav_Agriculture_Grassland_log","mav_Agriculture_Pasture_log","mav_Natural_Forest_Agriculture_log",
              "mav_Natural_Forest_Forest_Plantation_log","mav_Natural_Forest_Grassland_log","mav_Natural_Forest_Pasture_log","mav_Forest_Formation_Agriculture_log",
              "mav_Forest_Formation_Forest_Plantation_log","mav_Forest_Formation_Grassland_log","mav_Forest_Formation_Pasture_log","mav_Forest_Plantation_Agriculture_log","mav_Forest_Plantation_Grassland_log",
              "mav_Forest_Plantation_Pasture_log","mav_Grassland_Agriculture_log","mav_Grassland_Forest_Plantation_log","mav_Grassland_Pasture_log","mav_Pasture_Agriculture_log",
              "mav_Pasture_Forest_Plantation_log","mav_Pasture_Grassland_log",
              "share_Agriculture","share_Natural.Forest","share_Forest.Formation","share_Forest.Plantation","share_Grassland","share_Pasture",
              "gdp_capita_log","educ","pop_growth","pop_dens","gva_agri_log","gva_indu_log","gva_serv_log", "gdp_capita_growth", "precip_norm","elevation",
              "Rho:", "Observations")
long_name <- c("Industrial mining 2005", "Industrial mining 2006", "Industrial mining 2007", "Industrial mining 2008", "Industrial mining 2009", "Industrial mining 2010", 
               "Industrial mining 2011", "Industrial mining 2012", "Industrial mining 2013", "Industrial mining 2014", "Industrial mining 2015", 
               "Industrial mining $\\times$ pre 2010", "Industrial mining $\\times$ since 2010", 
               "Garimpo mining 2005", "Garimpo mining 2006", "Garimpo mining 2007", "Garimpo mining 2008", "Garimpo mining 2009", "Garimpo mining 2010", 
               "Garimpo mining 2011", "Garimpo mining 2012", "Garimpo mining 2013", "Garimpo mining 2014", "Garimpo mining 2015", 
               "Garimpo mining $\\times$ pre 2010", "Garimpo mining $\\times$ since 2010",
               "LUC$^{Agriculture, Forest\ Plantation}$","LUC$^{Agriculture, Grassland}$","LUC$^{Agriculture, Pasture}$", "LUC$^{Natural\ Forest, Agriculture}$","LUC$^{Natural\ Forest, Forest\ Plantation}$",
               "LUC$^{Natural\ Forest, Grassland}$","LUC$^{Natural\ Forest, Pasture}$", "LUC$^{Natural\ Forest, Agriculture}$",
               "LUC$^{Natural\ Forest, Forest\ Plantation}$","LUC$^{Natural\ Forest, Grassland}$","LUC$^{Natural\ Forest, Pasture}$", "LUC$^{Forest\ Plantation, Agriculture}$","LUC$^{Forest\ Plantation, Grassland}$",
               "LUC$^{Forest\ Plantation, Pasture}$","LUC$^{Grassland, Agriculture}$","LUC$^{Grassland, Forest\ Plantation}$","LUC$^{Grassland, Pasture}$",
               "LUC$^{Pasture, Agriculture}$","LUC$^{Pasture, Forest\ Plantation}$","LUC$^{Pasture, Grassland}$",
               "Initial agriculture","Initial natural forest","Initial natural forest","Initial forest plantation","Initial grassland","Initial pasture","Initial income",
               "Human capital","Population growth","Population density","GVA agriculture","GVA industry","GVA services", "GPD growth", "Precipitation","Elevation",
               "$\\rho$", "Observations")
concordance <- data.frame(raw_name, long_name)

# E1 economic growth ------------------------------------------------------

impacts_gdp_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "gdp_yearly_industrial_baseline_")
impacts_gdp_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "gdp_pooled_industrial_baseline_")

### yearly estimates

# read output Excel
impacts_raw <- read.delim(file.path("data/cem/sdm_impact_estimates/summaries/", impacts_gdp_yearly),  sep = ",") 
rho <- impacts_raw %>% dplyr::filter(Variable == "Rho:")
impacts_raw <- impacts_raw %>%
  dplyr::filter(!is.na( Indirect.1.)) 

# tidy
impacts <- impacts_raw %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>% 
  dplyr::filter(Variable != "Dependent variable:") %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(impacts) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# add rho
rho <- rho %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(rho) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")
impacts <- dplyr::bind_rows(impacts, rho)

# add observations
impacts <- dplyr::bind_rows(impacts, observations)

# variable concordance
concordance_sub <- concordance %>% dplyr::filter(raw_name %in% impacts$Variable)
impacts <- concordance_sub %>% 
  dplyr::left_join(impacts, by = c("raw_name" = "Variables")) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename(Variables = long_name)

# latex
print(xtable::xtable(impacts, 
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3), align = "llrrrrrr",
                     caption = 
                       "\\textbf{Average direct and indirect impact estimates on economic growth with yearly mining effects.} 
                     Panel-structure (2005-2020) spatial Durbin model including time fixed effects. 
                     Dependent variable is 5-year average annual GDP per capita growth rate. 
                     Estimates printed in bold type are statistically different from zero based on the 95 percent posterior credible interval. 
                     PM denotes posterior mean. Time-specific intercepts were excluded for more concise summary tables."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)



### pooled estimates

# read output Excel
impacts_raw <- read.delim(file.path("data/cem/sdm_impact_estimates/summaries/", impacts_gdp_pooled),  sep = ",") 
rho <- impacts_raw %>% dplyr::filter(Variable == "Rho:")
impacts_raw <- impacts_raw %>%
  dplyr::filter(!is.na( Indirect.1.)) 

# tidy
impacts <- impacts_raw %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>% 
  dplyr::filter(Variable != "Dependent variable:") %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(impacts) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# add rho
rho <- rho %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(rho) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")
impacts <- dplyr::bind_rows(impacts, rho)

# add observations
impacts <- dplyr::bind_rows(impacts, observations)

# variable concordance
concordance_sub <- concordance %>% dplyr::filter(raw_name %in% impacts$Variable)
impacts <- concordance_sub %>% 
  dplyr::left_join(impacts, by = c("raw_name" = "Variables")) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename(Variables = long_name)

# latex
print(xtable::xtable(impacts, 
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3), align = "llrrrrrr",
                     caption = 
                       "\\textbf{Average direct and indirect impact estimates on economic growth with pooled mining effects.} 
                     Panel-structure (2005-2020) spatial Durbin model including time fixed effects. 
                     Dependent variable is 5-year average annual GDP per capita growth rate. 
                     Estimates printed in bold type are statistically different from zero based on the 95 percent posterior credible interval. 
                     PM denotes posterior mean. Time-specific intercepts were excluded for more concise summary tables."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)



# E2 Forest loss ----------------------------------------------------------

impacts_def_rel_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_yearly_industrial_baseline_")
impacts_def_rel_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "rel_pooled_industrial_baseline_")
impacts_def_abs_yearly <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_yearly_industrial_baseline_")
impacts_def_abs_pooled <- list.files("data/cem/sdm_impact_estimates/summaries/", pattern = "abs_pooled_industrial_baseline_")

### yearly estimates relative

# read output Excel
impacts_raw <- read.delim(file.path("data/cem/sdm_impact_estimates/summaries/", impacts_def_rel_yearly),  sep = ",") 
rho <- impacts_raw %>% dplyr::filter(Variable == "Rho:")
impacts_raw <- impacts_raw %>%
  dplyr::filter(!is.na( Indirect.1.)) 

# tidy
impacts <- impacts_raw %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>% 
  dplyr::filter(Variable != "Dependent variable:") %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(impacts) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# add rho
rho <- rho %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(rho) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")
impacts <- dplyr::bind_rows(impacts, rho)

# add observations
impacts <- dplyr::bind_rows(impacts, observations)

# variable concordance
concordance_sub <- concordance %>% dplyr::filter(raw_name %in% impacts$Variable)
impacts <- concordance_sub %>% 
  dplyr::left_join(impacts, by = c("raw_name" = "Variables")) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename(Variables = long_name)

# latex
print(xtable::xtable(impacts, 
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3), align = "llrrrrrr",
                     caption = 
                       "\\textbf{Average direct and indirect impact estimates on forest loss (relative) with yearly mining effects.} 
                     Panel-structure (2005-2020) spatial Durbin model including time fixed effects. 
                     Dependent variable is annual forest loss in ha per $\\text{km}^2$. 
                     Estimates printed in bold type are statistically different from zero based on the 95 percent posterior credible interval. 
                     PM denotes posterior mean. Time-specific intercepts were excluded for more concise summary tables."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)



### pooled estimates relative

# read output Excel
impacts_raw <- read.delim(file.path("data/cem/sdm_impact_estimates/summaries/", impacts_def_rel_pooled),  sep = ",") 
rho <- impacts_raw %>% dplyr::filter(Variable == "Rho:")
impacts_raw <- impacts_raw %>%
  dplyr::filter(!is.na( Indirect.1.)) 

# tidy
impacts <- impacts_raw %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>% 
  dplyr::filter(Variable != "Dependent variable:") %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(impacts) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# add rho
rho <- rho %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(rho) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")
impacts <- dplyr::bind_rows(impacts, rho)

# add observations
impacts <- dplyr::bind_rows(impacts, observations)

# variable concordance
concordance_sub <- concordance %>% dplyr::filter(raw_name %in% impacts$Variable)
impacts <- concordance_sub %>% 
  dplyr::left_join(impacts, by = c("raw_name" = "Variables")) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename(Variables = long_name)

# latex
print(xtable::xtable(impacts, 
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3), align = "llrrrrrr",
                     caption = 
                       "\\textbf{Average direct and indirect impact estimates on forest loss (relative) with pooled mining effects.} 
                     Panel-structure (2005-2020) spatial Durbin model including time fixed effects. 
                     Dependent variable is annual forest loss in ha per $\\text{km}^2$. 
                     Estimates printed in bold type are statistically different from zero based on the 95 percent posterior credible interval. 
                     PM denotes posterior mean. Time-specific intercepts were excluded for more concise summary tables."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)



### yearly estimates absolute

# read output Excel
impacts_raw <- read.delim(file.path("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_yearly),  sep = ",") 
rho <- impacts_raw %>% dplyr::filter(Variable == "Rho:")
impacts_raw <- impacts_raw %>%
  dplyr::filter(!is.na( Indirect.1.)) 

# tidy
impacts <- impacts_raw %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>% 
  dplyr::filter(Variable != "Dependent variable:") %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(impacts) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# add rho
rho <- rho %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(rho) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")
impacts <- dplyr::bind_rows(impacts, rho)

# add observations
impacts <- dplyr::bind_rows(impacts, observations)

# variable concordance
concordance_sub <- concordance %>% dplyr::filter(raw_name %in% impacts$Variable)
impacts <- concordance_sub %>% 
  dplyr::left_join(impacts, by = c("raw_name" = "Variables")) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename(Variables = long_name)

# latex
print(xtable::xtable(impacts, 
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3), align = "llrrrrrr",
                     caption = 
                       "\\textbf{Average direct and indirect impact estimates on forest loss (absolute) with yearly mining effects.} 
                     Panel-structure (2005-2020) spatial Durbin model including time fixed effects. 
                     Dependent variable is annual forest loss in absolute ha. 
                     Estimates printed in bold type are statistically different from zero based on the 95 percent posterior credible interval. 
                     PM denotes posterior mean. Time-specific intercepts were excluded for more concise summary tables."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)



### pooled estimates absolute

# read output Excel
impacts_raw <- read.delim(file.path("data/cem/sdm_impact_estimates/summaries/", impacts_def_abs_pooled),  sep = ",") 
rho <- impacts_raw %>% dplyr::filter(Variable == "Rho:")
impacts_raw <- impacts_raw %>%
  dplyr::filter(!is.na( Indirect.1.)) 

# tidy
impacts <- impacts_raw %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>% 
  dplyr::filter(Variable != "Dependent variable:") %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(impacts) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")

# add rho
rho <- rho %>% 
  dplyr::select(c(1, 3, 5, 7, 10, 12, 14)) %>%
  dplyr::mutate(Direct.2.5.  = as.numeric(as.character(Direct.2.5. )))
colnames(rho) <- c("Variables", "2.5\\%", "PM", "97.5\\%", " 2.5\\%", " PM", " 97.5\\%")
impacts <- dplyr::bind_rows(impacts, rho)

# add observations
impacts <- dplyr::bind_rows(impacts, observations)

# variable concordance
concordance_sub <- concordance %>% dplyr::filter(raw_name %in% impacts$Variable)
impacts <- concordance_sub %>% 
  dplyr::left_join(impacts, by = c("raw_name" = "Variables")) %>%
  dplyr::select(-raw_name) %>%
  dplyr::rename(Variables = long_name)

# latex
print(xtable::xtable(impacts, 
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3), align = "llrrrrrr",
                     caption = 
                       "\\textbf{Average direct and indirect impact estimates on forest loss (absolute) with pooled mining effects.} 
                     Panel-structure (2005-2020) spatial Durbin model including time fixed effects. 
                     Dependent variable is annual forest loss in absolute ha. 
                     Estimates printed in bold type are statistically different from zero based on the 95 percent posterior credible interval. 
                     PM denotes posterior mean. Time-specific intercepts were excluded for more concise summary tables."), 
      include.rownames=FALSE, size = "scriptsize",
      sanitize.text.function = identity)




