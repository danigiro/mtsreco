# Australian GDP from Income and Expenditure sides

This Github repository contains scripts and plots related to the forecasting reconciliation of the Australian GDP from Income and Expenditure sides. Below is a brief description of the contents of each folder:

*script* folder:

 - `lin_comb_mat.R`: derives the linear combination matrix for the entire system.
 - `base.R`: calculates the base forecasts Income OR Expenditure.
 - `bas_ause.R`: calculates the base forecasts, Income AND Expenditure.
 - `reco_point.R`: calculates the point-wise reconciled forecasts Income OR Expenditure OR (Income AND Expenditure).
 - `reco_prob_boot.R`: calculates non-parametric joint bootstrap probabilistic forecasts Income OR Expenditure OR (Income AND Expenditure).
 - `reco_prob_gaussian.R`: calculates Gaussian probabilistic forecasts Income OR Expenditure OR (Income AND Expenditure).
 - `prob_scores.R`: calculates CRPS, ES, and VS for probabilistic forecasts.
 - `makefile.R`: workflow for the R script.

*plot* folder:

 - `agg_mat_plot.R`: generates figures for the linear combination matrix.
 - `mcb.R`: generates figures related to the Multiple Comparison with the Best Nemenyii test.
 - `results_point.R`: generates figures for point forecast accuracy indexes.
 - `results_boot.R`: generates figures for non-parametric joint bootstrap probabilistic forecast accuracy indexes.
 - `results_gauss.R`: generates figures for Gaussian probabilistic forecast accuracy indexes.
 