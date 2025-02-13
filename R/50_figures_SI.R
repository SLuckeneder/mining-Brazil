
if (!dir.exists("figures")){dir.create("figures")}
if (!dir.exists("figures/SI")){dir.create("figures/SI")}

# Figure S1 - biomes and mining area
source("R/51_figure_SI_biomes.R")

# Figures S2-S3 - data
source("R/51_figure_SI_data.R")

# Figure S4 - spatial weights matrix
source("R/51_figure_SI_W.R")

# Figures S5-S6 - mining area in hectares instead of binary indicator
source("R/51_figure_SI_hectares_matching.R")

# Figure S7 - alternative economic growth windows
source("R/51_figure_SI_growth_windows.R")

# Figures S8-S9 - alternative spatial weights matrices
source("R/51_figure_SI_change_W.R")

# Figure S11 - SMD: see R/11.run_cem.R

