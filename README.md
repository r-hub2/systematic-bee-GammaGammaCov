# GammaGammaCov

[![License: Apache 2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)

**GammaGammaCov** implements the gamma-gamma model of spend per transaction with both time-varying and time-invariant covariates, following the methodology of Fader and Hardie (2024).

## Installation

Install the released version from CRAN:

```r
install.packages("GammaGammaCov")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("kaloklee/GammaGammaCov")
```

------------------------------------------------------------------------

## Quick Start

### 1. Time-Varying Covariates (`spend_data_sample`)

Data are in **transaction (long) format**: one row per transaction, with the customer identifier, the amount spent, and the covariate values in effect at the time of that transaction.

Here we demonstrate estimation using `spend_data_sample`, a small simulated dataset included for rapid experimentation and testing:

```r
library(GammaGammaCov)

# Load the quick-start sample dataset
spend_data_sample <- GammaGammaCov::spend_data_sample

# Estimate the spend model with dynamic covariates
fit <- GGcov_MLE(
  Price ~ discount + gender + channel,
  data    = spend_data_sample,
  subject = "Id"
)

# Print parameter estimates, standard errors, and Wald confidence intervals
print(fit)

# Plot the distribution of the baseline spending rate across customers
GGcov_plot_mixing(fit)
```

### 2. Expected Spend on a Future Transaction

`GGcov_expected_spend()` returns the posterior conditional expectation of the monetary value of a future transaction, given the customer's history and the covariate values of that transaction (assumed known). Here we predict each customer's next transaction at a 10% discount:

```r
# Each customer's gender and channel, taken from their last observed row
last_rows <- spend_data_sample[!duplicated(spend_data_sample$Id, fromLast = TRUE), ]
future <- last_rows[, c("Id", "discount", "gender", "channel")]
future$discount <- 0.1

GGcov_expected_spend(fit, newdata = future, history = spend_data_sample)
```

------------------------------------------------------------------------

## Vignettes

For a more comprehensive demonstration of the package's functionality, browse the [introductory vignette](https://kaloklee.github.io/GammaGammaCov/articles/introduction_to_ggcov_vignette.html) online, or build it locally:

```r
remotes::install_github("kaloklee/GammaGammaCov", build_vignettes = TRUE)
vignette("introduction_to_ggcov_vignette", package = "GammaGammaCov")
```

------------------------------------------------------------------------

## References

* Fader, P. S. & Hardie, B. G. S. (2013). *The Gamma-Gamma Model of Monetary Value* (Research Note No. 025). [Research Note](https://brucehardie.com/notes/025/).
* Fader, P. S. & Hardie, B. G. S. (2024). *Incorporating Covariates in the Gamma-Gamma Spend Model* (Research Note No. 045). [Research Note](https://brucehardie.com/notes/045/).

------------------------------------------------------------------------

## License & Contact

Licensed under the [Apache License 2.0](LICENSE).  
Questions, issues, or contributions: Ka Lok Lee ([kaloklee@gmail.com](mailto:kaloklee@gmail.com)).
