# Brazilian mining impacts - A Bayesian Spatial Durbin Model

This repository contains the R scripts and data to reproduce results from "Forest loss and uncertain economic gains from industrial and garimpo mining in Brazilian municipalities". 

## Usage

All scripts related to data compilation (`00_compile_data.R` and herein sourced `01_*.R`) automatically download and process raw data (see respective `01_*.R` scripts). Alternatively, users can move directly to the matching (`10_matching.R` and herein sourced `11_run_cem.R`, etc.), followed by the modelling (`20_run_spatial_models.R` and herein sourced `21_lndetPaceBarry.R` and `2*_run_*.R`), as ready-to-use dataset required for running the scripts is also provided in this GitHub repository.

`10_matching.R` performs coarsened exact matching (CEM) separately for industrial and garimpo mining municipalities using the R package by Iacus, King and Porro (2009).

`20_run_spatial_models.R` runs 12 Bayesian Spatial Durbin Models estimating municipality-level mining impacts on per capita GDP, relative and absolute forest loss for yearly and pooled (pre-2010 and post-2010) effects, using the datasets resulting from the matching for industrial and garimpo mining, respectively. MCMC draws of estimated parameters are stored as Rdata in `./data/intermediary/MCMC_draws_matching`. Related direct, indirect and total impact estimates of the spatial models are stored in `./data/cem/sdm_impact_estimates/draws` and summarised as csv files in `./data/cem/sdm_impact_estimates/summaries`. `21_lndetPaceBarry.R` provides a function to approximate the log-determinant of positive-definite spatial projection matrices of the form $(I - \rho W)$ as proposed by Barry and Pace (1999) and `23_geweke_statistics.R` shows convergence of the samplers using diagnostics proposed by Geweke (1992).

`30_figures_main.R` (sourcing `31_figure_1.R`, `32_figure_2.R`, `33_figure_3.R`, `34_figure_4.R` and `35_figure_5.R`) generates the respective main text figures, saved as PNG files in `./figures/main_text`.

`40_run_models_SI` runs models presented in the supplementary materials.

`50_figures_SI.R` generates the respective figures for the supplementary materials, saved as PNG files in `./figures/SI`.

`60_tables_SI_latex_industrial.R` and `60_tables_SI_latex_garimpo.R` generate LaTeX code for the tables shown in the supplementary materials.

## References

Barry, R. P., and Pace, R. K. (1999): Monte Carlo estimates of the log determinant of large sparse matrices. Linear Algebra and its applications, 289(1-3), 41-54. 

Geweke, J. (1992). Evaluating the accuracy of sampling-based approaches to the calculation of posterior moments. In J. M. Bernardo, J. O. Berger, A. P. Dawid and A. F. M. Smith (Eds.), Bayesian Statistics (Vol. 4, pp. 169–193). Oxford: Clarendon Press

Iacus, S., King, G., and Porro, G. (2009): cem: Software for coarsened exact matching, Journal of Statistical Software 30, 1–27.
