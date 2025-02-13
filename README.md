# Brazilian mining impacts - A Bayesian Spatial Durbin Model

This repository contains the R scripts and data to reproduce results from "Transient economic benefit and persistent forest loss: regional impacts of mining in Brazil". 

## Usage

All scripts related to data compilation (`00_compile_data.R` and herein sourced `01_*.R`) automatically download and process raw data (see respective `01_*.R` scripts). Alternatively, users can move directly to the models (`10_run_spatial_models.R` and herein sourced `11_lndetPaceBarry.R` and `12_run_*.R`), as ready-to-use dataset required for running the scripts is also provided in this GitHub repository.

`10_run_spatial_models.R` runs 6 Bayesian Spatial Durbin Models estimating municipality-level mining impacts on per capita GDP, relative and absolute forest loss for yearly and pooled (pre-2010 and post-2010) effects. MCMC draws of estimated parameters are stored as Rdata in `./data/intermediary/MCMC_draws`. Related direct, indirect and total impact estimates of the spatial models are stored in `./data/impact_estimates/draws` and summarised as csv files in `./data/impact_estimates/summaries`. `11_lndetPaceBarry.R` provides a function to approximate the log-determinant of positive-definite spatial projection matrices of the form $(I - \rho W)$ as proposed by Barry and Pace (1999) and `13_geweke_statistics.R` shows convergence of the samplers using diagnostics proposed by Geweke (1992).

`20_figures_main.R` (sourcing `21_figure_1.R`, `22_figure_2.R`, `23_figure_3.R`, `24_figure_4.R` and `25_figure_5.R`) generates the respective main text figures, saved as PNG files in `./figures/main_text`.

`30_run_models_SI` runs models presented in the supplementary information.

`40_figures_SI.R` generates the respective figures for the supplementary information, saved as PNG files in `./figures/SI`.

`50_tables_SI_latex.R` generates LaTeX code for the tables shown in the supplementary information.

## References

Barry, R. P., and Pace, R. K. (1999): Monte Carlo estimates of the log determinant of large sparse matrices. Linear Algebra and its applications, 289(1-3), 41-54. 

Geweke, J. (1992). Evaluating the accuracy of sampling-based approaches to the calculation of posterior moments. In J. M. Bernardo, J. O. Berger, A. P. Dawid and A. F. M. Smith (Eds.), Bayesian Statistics (Vol. 4, pp. 169–193). Oxford: Clarendon Press
