# GammaGammaCov 0.1.0

* Initial CRAN release.
* Added `GGcov_MLE()` for maximum likelihood estimation of the gamma-gamma spend model with time-varying, static, or no covariates (Fader and Hardie, 2024).
* Added `GGcov_expected_spend()` to compute the posterior conditional expectation of future transaction spend given a customer's transaction history and future covariate values.
* Added `GGcov_plot_mixing()` to plot the gamma mixing distribution of the baseline spending rate across customers.
* Added `GGcov_plot_avg_spend()` to compare observed versus model-implied distributions of customer average spend per transaction.
* Included simulated transaction datasets `spend_data` (1,000 customers) and `spend_data_sample` (300 customers), along with an introductory vignette.
