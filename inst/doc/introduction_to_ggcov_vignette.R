## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5,
  warning  = FALSE,
  message  = FALSE
)

## ----load-varying-------------------------------------------------------------
library(GammaGammaCov)

# Load the quick-start sample dataset
spend_data_sample <- GammaGammaCov::spend_data_sample

# Inspect the transaction data structure
head(spend_data_sample, 10)

## ----fit-varying--------------------------------------------------------------
fit_varying <- GGcov_MLE(
  Price ~ discount + gender + channel,
  data    = spend_data_sample,
  subject = "Id"
)

# View parameter estimates, standard errors, and Wald confidence intervals
print(fit_varying)

## ----plot-mixing--------------------------------------------------------------
GGcov_plot_mixing(fit_varying)

## ----avg-spend-varying--------------------------------------------------------
GGcov_plot_avg_spend(fit_varying, spend_data_sample)

## ----avg-spend-holdout--------------------------------------------------------
set.seed(2024)
ids       <- unique(spend_data_sample$Id)
train_ids <- sample(ids, length(ids) / 2)

train <- spend_data_sample[spend_data_sample$Id %in% train_ids, ]
test  <- spend_data_sample[!spend_data_sample$Id %in% train_ids, ]

fit_train <- GGcov_MLE(
  Price ~ discount + gender + channel,
  data    = train,
  subject = "Id"
)

GGcov_plot_avg_spend(fit_train, test)

## ----expected-spend-varying---------------------------------------------------
last_rows <- spend_data_sample[!duplicated(spend_data_sample$Id, fromLast = TRUE), ]
future <- last_rows[, c("Id", "discount", "gender", "channel")]
future$discount <- 0.1

pred_varying <- GGcov_expected_spend(fit_varying,
                                     newdata = future,
                                     history = spend_data_sample)
head(pred_varying)

## ----expected-spend-scenarios-------------------------------------------------
spend_0  <- GGcov_expected_spend(fit_varying, transform(future, discount = 0),
                                 spend_data_sample)$expected_spend
spend_30 <- GGcov_expected_spend(fit_varying, transform(future, discount = 0.3),
                                 spend_data_sample)$expected_spend

head(data.frame(Id = future$Id, no_discount = spend_0, discount_30 = spend_30))

## ----fit-static---------------------------------------------------------------
fit_static <- GGcov_MLE(
  Price ~ gender + channel,
  data    = spend_data_sample,
  subject = "Id",
  level   = 0.99
)

print(fit_static)

## ----avg-spend-static---------------------------------------------------------
GGcov_plot_avg_spend(fit_static, spend_data_sample)

## ----expected-spend-static----------------------------------------------------
customers <- spend_data_sample[!duplicated(spend_data_sample$Id),
                               c("Id", "gender", "channel")]

pred_static <- GGcov_expected_spend(fit_static,
                                    newdata = customers,
                                    history = spend_data_sample)
head(pred_static)

## ----fit-null-----------------------------------------------------------------
fit_null <- GGcov_MLE(Price ~ 1, data = spend_data_sample, subject = "Id")
print(fit_null)

## ----lr-test------------------------------------------------------------------
lr <- 2 * (fit_null$value - fit_varying$value)
df <- length(fit_varying$par) - length(fit_null$par)
pchisq(lr, df = df, lower.tail = FALSE)

## ----avg-spend-null-----------------------------------------------------------
GGcov_plot_avg_spend(fit_null, spend_data_sample)

## ----expected-spend-null------------------------------------------------------
customer_ids <- data.frame(Id = unique(spend_data_sample$Id))

pred_null <- GGcov_expected_spend(fit_null,
                                  newdata = customer_ids,
                                  history = spend_data_sample)
head(pred_null)

## ----expected-spend-shrinkage-------------------------------------------------
observed <- aggregate(Price ~ Id, data = spend_data_sample, FUN = mean)
names(observed)[2] <- "observed_mean"

comparison <- merge(pred_null, observed, by = "Id")
head(comparison[order(comparison$n_trans), ])

## ----session-info-------------------------------------------------------------
sessionInfo()

