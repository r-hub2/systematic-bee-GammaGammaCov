test_that("GGcov_plot_mixing returns mixing summary and validates inputs", {
  fit <- GGcov_MLE(Price ~ 1, data = spend_data_sample, subject = "Id")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- GGcov_plot_mixing(fit, n = 100)
  expect_named(res, c("mean", "median"))
  expect_equal(res$mean, unname(fit$par[2] / fit$par[3]))
  expect_true(res$median > 0)

  expect_error(
    GGcov_plot_mixing(list()),
    "'fit' must be a model of class \"GGcov\""
  )
  expect_error(
    GGcov_plot_mixing(fit, coverage = 1),
    "'coverage' must be a single number strictly between 0 and 1"
  )
})

test_that("GGcov_plot_avg_spend works for time-varying and static models", {
  fit_var <- GGcov_MLE(
    Price ~ discount + gender + channel,
    data    = spend_data_sample,
    subject = "Id"
  )
  fit_static <- GGcov_MLE(
    Price ~ gender + channel,
    data    = spend_data_sample,
    subject = "Id"
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  res_var <- GGcov_plot_avg_spend(fit_var, spend_data_sample, n = 50)
  expect_named(res_var, c("grid", "observed", "fitted", "n_customers", "n_time_varying"))
  expect_length(res_var$grid, 50L)
  expect_length(res_var$fitted, 50L)
  expect_equal(res_var$n_customers, length(unique(spend_data_sample$Id)))
  expect_gt(res_var$n_time_varying, 0L)

  res_stat <- GGcov_plot_avg_spend(fit_static, spend_data_sample, max_customers = 100, n = 50)
  expect_equal(res_stat$n_time_varying, 0L)

  expect_error(
    GGcov_plot_avg_spend(list(), spend_data_sample),
    "'fit' must be a model of class \"GGcov\""
  )
  expect_error(
    GGcov_plot_avg_spend(fit_var, as.list(spend_data_sample)),
    "'data' must be a data frame"
  )
  expect_error(
    GGcov_plot_avg_spend(fit_var, data.frame(Id = 1)),
    "Column\\(s\\) not found in 'data': Price"
  )
  bad_df <- spend_data_sample[1:10, ]
  bad_df$Price[1] <- 0
  expect_error(
    GGcov_plot_avg_spend(fit_var, bad_df),
    "Spend column 'Price' must be numeric, strictly positive"
  )
})
