
library(dplyr)
library(cem)
library(fastDummies)

if (!dir.exists("data/cem")){dir.create("data/cem")}
if (!dir.exists("data/cem/cem_results")){dir.create("data/cem/cem_results")}
if (!dir.exists("figures/SI")){dir.create("figures/SI", recursive = TRUE)}

# load data for the main model specifications -----------------------------

window_yrs <- 5 # set window for calculation of average annual growth rates, main model uses 5 years
from_yr <- 2005 # set start year of panel, main model uses 2005
to_yr <- 2015 # set end year of panel minus window_yrs, main model uses 2020-5 = 2015
if(! file.exists("data/full_data_2000-2020_5y.csv")){source("R/00_compile_data.R")}
M <- read.csv2(file = "data/full_data_2000-2020_5y.csv", sep = ",", stringsAsFactors = FALSE) # read full data matrix
M <- M %>% dplyr::mutate_at(c(4:121), as.numeric)

# filter to selected years (e.g. 2005-2015 covers data up to 2020 due to 5-year growth windows)
M <- M %>% dplyr::filter(year %in% c(from_yr:to_yr))

# filter to balanced panel
check <- M %>% dplyr::group_by(cod_municipio_long) %>% dplyr::summarise(n = n()) %>%
  dplyr::filter(n == length(unique(M$year)))
M <- M %>% dplyr::filter(cod_municipio_long %in% check$cod_municipio_long)

# select all variables used in the modelling + others interesting for matching (cod_municipio_long, municipality area, absolute forest area)
M <- M %>%
  dplyr::select(gdp_capita_growth, 
                natural_forest_loss_ha_km2, natural_forest_loss,
                gdp_capita_log, pop_growth, pop_dens, educ,
                gva_agri_log, gva_indu_log, gva_serv_log,
                share_Agriculture, `share_Forest.Formation`, `share_Forest.Plantation`, share_Grassland, share_Pasture,
                mav_Agriculture_Forest_Plantation_log, mav_Agriculture_Grassland_log, mav_Agriculture_Pasture_log,
                mav_Forest_Formation_Agriculture_log, mav_Forest_Formation_Forest_Plantation_log, mav_Forest_Formation_Grassland_log, mav_Forest_Formation_Pasture_log,
                mav_Forest_Plantation_Agriculture_log, mav_Forest_Plantation_Grassland_log, mav_Forest_Plantation_Pasture_log,
                mav_Grassland_Agriculture_log, mav_Grassland_Forest_Plantation_log, mav_Grassland_Pasture_log,
                mav_Pasture_Agriculture_log, mav_Pasture_Forest_Plantation_log, mav_Pasture_Grassland_log,
                precip_norm, elevation, AREA_mun, Forest.Formation,
                year, cod_municipio_long, 
                mining_industrial, mining_garimpo, mining) %>%
  dplyr::mutate(mining_treatment = ifelse(mining > 0, TRUE, FALSE),
                mining_treatment_industrial = ifelse(mining_industrial > 0, TRUE, FALSE), 
                mining_treatment_garimpo = ifelse(mining_garimpo > 0, TRUE, FALSE))

# prepare selection of matching variables
# tried a range of alternative models, which did not work so well
# Our selection, inspired by Giljum et al. (2022) and Lo et al. (2024):
match_on_baseline <- c("pop_dens", "share_Forest.Formation", "precip_norm", "elevation", "AREA_mun") # baseline

drop_but <- function(x, but) {
  names(x)[!grepl(paste0(but, collapse = "|"), names(x))]
}


# baseline ----------------------------------------------------------------

# perform matching for industrial mining
out_cem_industrial <- cem::cem(treatment = "mining_treatment_industrial", data = M,
                               drop = drop_but(M, match_on_baseline),
                               keep.all = TRUE)

M$w <- out_cem_industrial[["w"]]
M_matched_industrial <- M %>% dplyr::filter(w > 0) %>% dplyr::select(-w)

# perform matching for garimpo mining
out_cem_garimpo <- cem::cem(treatment = "mining_treatment_garimpo", data = M,
                            drop = drop_but(M, match_on_baseline),
                            keep.all = TRUE)

M$w <- out_cem_garimpo[["w"]]
M_matched_garimpo <- M %>% dplyr::filter(w > 0) %>% dplyr::select(-w)

# store matching results
write.csv(M_matched_industrial, file = paste0("data/cem/cem_results/cem_baseline_data_industrial_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)
write.csv(M_matched_garimpo, file = paste0("data/cem/cem_results/cem_baseline_data_garimpo_", from_yr, "-", to_yr+window_yrs, "_", window_yrs, "y.csv"), row.names = FALSE)



# checking balances -------------------------------------------------------
# (Zhang et al 2019)

library("grid")
library("cobalt")
library("ggplot2")
library("viridis")

tbl_match <- M %>% dplyr::select(out_cem_industrial$vars)

### Standardised mean difference
p_smd_industrial <- cobalt::love.plot(bal.tab(out_cem_industrial, data = tbl_match, m.threshold=0.1), 
                  stat = "mean.diffs", abs = F, colors = c("darkorchid2", "skyblue2"), shapes = c(17, 15), 
                  size = 5, title = "Industrial mining", position = "none") + 
  ggplot2::xlab(NULL)

p_smd_garimpo <- cobalt::love.plot(bal.tab(out_cem_garimpo, data = tbl_match, m.threshold=0.1), 
                  stat = "mean.diffs", abs = F, colors = c("darkorchid2", "skyblue2"), shapes = c(17, 15), 
                  size = 5, title = "Garimpo mining", position = "none") + 
  ggplot2::xlab(NULL)

p_merge <- cowplot::plot_grid(p_smd_industrial, p_smd_garimpo, nrow = 1, labels = c("a", "b"), label_size = 14) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure_SI_balances_smd.pdf",
                plot = p_merge, device = "pdf",
                path = paste0("./figures/SI"),
                scale = 1, width = 200, height = 80, units = "mm")




# ### Density function showing distributions before and after matching for share forest
# 
# p_forest_industrial <- cobalt::bal.plot(out_cem_industrial, data = tbl_match, var.name = 'share_Forest.Formation', which = 'both', 
#                  sample.names = c("Unadjusted", "Adjusted"),
#                  colors = c(viridis(3)[1], viridis(3)[3])) + 
#   ggplot2::labs(title = "Industrial mining - initial forest cover", x = NULL) +
#   ggplot2::scale_x_continuous(n.breaks = 3) +
#   ggplot2::theme(legend.position = c(0.85, 0.85),
#                  axis.title.y = element_text(hjust = 1))
# 
# p_forest_garimpo <- cobalt::bal.plot(out_cem_garimpo, data = tbl_match, var.name = 'share_Forest.Formation', which = 'both', 
#                                         sample.names = c("Unadjusted", "Adjusted"),
#                                         colors = c(viridis(3)[1], viridis(3)[3])) + 
#   ggplot2::labs(title = "Garimpo mining - initial forest cover", x = NULL) +
#   ggplot2::scale_x_continuous(n.breaks = 3) +
#   ggplot2::theme(legend.position = c(0.85, 0.85),
#                  axis.title.y = element_text(hjust = 1))
# 
# p_merge <- cowplot::plot_grid(p_forest_industrial, p_forest_garimpo, nrow = 2, labels = c("a", "b"), label_size = 18) +
#   theme(plot.background = element_rect(fill = "white", colour = NA))
# 
# ggplot2::ggsave("figure_SI_balances_forest.png",
#                 plot = p_merge, device = "png",
#                 path = paste0("./figures/SI"),
#                 scale = 1, width = 300, height = 200, units = "mm")




