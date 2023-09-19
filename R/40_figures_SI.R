
if (!dir.exists("figures")){dir.create("figures")}
if (!dir.exists("figures/SI")){dir.create("figures/SI")}

# Figures B - data
source("R/41_figure_SI_data.R")

# Figure C - spatial weights matrix

# Figures F1 - mining area in hectares instead of binary indicator
source("R/41_figure_SI_hectares.R")

# Figure F2 - alternative economic growth windows
source("R/41_figure_SI_growth_windows.R")

# Figures F3 - alternative spatial weights matrices
source("R/41_figure_SI_change_W.R")

