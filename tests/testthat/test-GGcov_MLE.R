test_that("GGcov_MLE fits models with time-varying, static, and no covariates", {
  fit_full <- GGcov_MLE(
    Price ~ discount + gender + channel,
    data    = spend_data_sample,
    subject = "Id"
  )
  expect_s3_class(fit_full, "GGcov")
  expect_equal(fit_full$convergence, 0L)
  expect_named(
    fit_full$par,
    c("p (shape)", "q (mixture shape)", "gamma (mixture rate)",
      "discount", "gendermale", "channelweb", "channelapp")
  )
  expect_true(all(fit_full$par[1:3] > 0))
  expect_true(all(is.finite(fit_full$par_stderr)))
  expect_true(all(fit_full$par_lower < fit_full$par))
  expect_true(all(fit_full$par < fit_full$par_upper))
  expect_equal(fit_full$spend_name, "Price")
  expect_equal(fit_full$subject_name, "Id")

  # Static covariates model
  fit_static <- GGcov_MLE(
    Price ~ gender + channel,
    data    = spend_data_sample,
    subject = "Id",
    level   = 0.99
  )
  expect_s3_class(fit_static, "GGcov")
  expect_equal(fit_static$convergence, 0L)
  expect_equal(fit_static$level, 0.99)
  expect_length(fit_static$par, 6L)

  # Null model (~ 1)
  fit_null <- GGcov_MLE(
    Price ~ 1,
    data    = spend_data_sample,
    subject = "Id"
  )
  expect_s3_class(fit_null, "GGcov")
  expect_equal(fit_null$convergence, 0L)
  expect_named(
    fit_null$par,
    c("p (shape)", "q (mixture shape)", "gamma (mixture rate)")
  )
  expect_null(fit_null$terms)
  expect_null(fit_null$xlevels)

  # Nested models: negative log-likelihood decreases as covariates are added
  expect_gt(fit_null$value, fit_static$value)
  expect_gt(fit_static$value, fit_full$value)

  # Print method returns object invisibly and prints summary
  expect_output(res <- print(fit_null, digits = 3), "Gamma-gamma model")
  expect_identical(res, fit_null)
})

test_that("GGcov_MLE is invariant to row order", {
  sub_ids <- unique(spend_data_sample$Id)[1:60]
  df1 <- spend_data_sample[spend_data_sample$Id %in% sub_ids, ]
  set.seed(42)
  df2 <- df1[sample.int(nrow(df1)), ]

  fit1 <- GGcov_MLE(Price ~ discount + gender, data = df1, subject = "Id")
  fit2 <- GGcov_MLE(Price ~ discount + gender, data = df2, subject = "Id")

  expect_equal(fit1$par, fit2$par, tolerance = 1e-5)
  expect_equal(fit1$value, fit2$value, tolerance = 1e-6)
})

test_that("GGcov_LL guards against extreme parameter values", {
  df <- data.frame(id = c(1, 1, 2, 2), z = c(10, 12, 8, 15))
  expect_equal(GGcov_LL(c(25, 1, 1), df), Inf)
  expect_equal(GGcov_LL(c(1, -25, 1), df), Inf)
  expect_true(is.finite(GGcov_LL(c(0, 1, 2), df)))
})

test_that("GGcov_MLE validates inputs properly", {
  expect_error(
    GGcov_MLE(Price ~ discount, data = spend_data_sample, subject = "Id", level = 1.1),
    "'level' must be a single number strictly between 0 and 1"
  )
  expect_error(
    GGcov_MLE(Price ~ discount, data = as.list(spend_data_sample), subject = "Id"),
    "'data' must be a data frame"
  )
  expect_error(
    GGcov_MLE(Price ~ discount, data = spend_data_sample, subject = c("Id", "Price")),
    "'subject' must be a single character string"
  )
  expect_error(
    GGcov_MLE(Price ~ discount, data = spend_data_sample, subject = "MissingId"),
    "Subject column 'MissingId' not found"
  )
  expect_error(
    GGcov_MLE(log(Price) ~ discount, data = spend_data_sample, subject = "Id"),
    "left-hand side of the formula must be a single variable name"
  )
  expect_error(
    GGcov_MLE(Price ~ missing_cov, data = spend_data_sample, subject = "Id"),
    "Column\\(s\\) not found in 'data': missing_cov"
  )
  expect_error(
    GGcov_MLE(Price ~ discount * gender, data = spend_data_sample, subject = "Id"),
    "right-hand side of the formula must contain only plain variable names"
  )

  bad_na <- spend_data_sample[1:20, ]
  bad_na$discount[3] <- NA
  expect_error(
    GGcov_MLE(Price ~ discount, data = bad_na, subject = "Id"),
    "NA values found in column\\(s\\): discount"
  )

  bad_price <- spend_data_sample[1:20, ]
  bad_price$Price[1] <- 0
  expect_error(
    GGcov_MLE(Price ~ discount, data = bad_price, subject = "Id"),
    "Outcome column 'Price' must be numeric and strictly positive"
  )

  const_cov <- spend_data_sample[1:50, ]
  const_cov$discount <- 0.2
  expect_error(
    GGcov_MLE(Price ~ discount, data = const_cov, subject = "Id"),
    "Covariate\\(s\\) with no variation: discount"
  )
})
