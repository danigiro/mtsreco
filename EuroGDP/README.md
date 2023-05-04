# European Area GDP from Output, Income and Expenditure sides

This Github repository contains scripts and plots related to the forecasting reconciliation of the European Area GDP from Output, Income and Expenditure sides. Below is a brief description of the contents of each folder:

*script* folder:

 - `agg_gdp.R`: derives the linear combination matrix for the entire system.
 - `data_exporter.R`: exports data.
 - `base.R`: calculates the base forecasts.
 - `reco_point.R`: calculates the point-wise reconciled forecasts.
 - `reco_prob_boot.R`: calculates non-parametric joint bootstrap probabilistic forecasts.
 - `reco_prob_gaussian.R`: calculates Gaussian probabilistic forecasts.
 - `prob_scores.R`: calculates CRPS, ES, and VS for probabilistic forecasts.

*PLOT* folder:

 - `agg_mat_plot.R`: generates figures for the linear combination matrix.
 - `mcb.R`: This folder contains figures related to the Multiple Comparison with the Best Nemenyii test.
 - `results_point.R`: generates figures for point forecast accuracy indexes.
 - `results_boot.R`: generates figures for non-parametric joint bootstrap probabilistic forecast accuracy indexes.
 - `results_gauss.R`: generates figures for Gaussian probabilistic forecast accuracy indexes.
 