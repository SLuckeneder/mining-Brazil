

if (!dir.exists("data/cem")){dir.create("data/cem")}
if (!dir.exists("data/cem/cem_results")){dir.create("data/cem/cem_results")}
if (!dir.exists("figures/SI")){dir.create("figures/SI", recursive = TRUE)}

# match data
source("R/11_run_cem.R")

# match data
source("R/12_run_cem_SI.R")

# summary tables comparing sample balance
source("R/13_matching_balances.R")

# figures
source("R/95_figures_matching.R")





