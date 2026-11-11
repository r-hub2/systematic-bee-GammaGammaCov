test_that("GGcov_expected_spend computes posterior and prior expectations", {
  fit_var <- GGcov_MLE(
    Price ~ discount + gender + channel,
    data    = spend_data_sample,
    subject = "Id"
  )

  last_rows <- spend_data_sample[!duplicated(spend_data_sample$Id, fromLast = TRUE), ]
  future_0  <- transform(last_rows[, c("Id", "discount", "gender", "channel")], discount = 0)
  future_30 <- transform(future_0, discount = 0.3)

  pred_0  <- GGcov_expected_spend(fit_var, newdata = future_0,  history = spend_data_sample)
  pred_30 <- GGcov_expected_spend(fit_var, newdata = future_30, history = spend_data_sample)

  expect_s3_class(pred_0, "data.frame")
  expect_named(pred_0, c("Id", "n_trans", "expected_spend"))
  expect_equal(nrow(pred_0), nrow(future_0))
  expect_true(all(pred_0$n_trans >= 1))
  expect_true(all(pred_0$expected_spend > 0))

  # Ratio of predictions under discount = 0.3 vs 0 is exp(0.3 * beta_discount)
  beta_disc <- unname(fit_var$par["discount"])
  expect_equal(
    pred_30$expected_spend / pred_0$expected_spend,
    rep(exp(0.3 * beta_disc), nrow(pred_0)),
    tolerance = 1e-10
  )

  # Prior expectation when history = NULL (x = 0, B = 0)
  pred_prior <- GGcov_expected_spend(fit_var, newdata = future_0[1:5, ], history = NULL)
  expect_equal(pred_prior$n_trans, rep(0, 5))
  expect_true(all(pred_prior$expected_spend > 0))

  # Customer absent from history receives prior expectation
  new_cust <- future_0[1, ]
  new_cust$Id <- -999
  pred_new <- GGcov_expected_spend(fit_var, newdata = new_cust, history = spend_data_sample)
  expect_equal(pred_new$n_trans, 0)
  expect_equal(pred_new$expected_spend, pred_prior$expected_spend[1])
})

test_that("GGcov_expected_spend works for null model and warns when p*x + q <= 1", {
  fit_null <- GGcov_MLE(Price ~ 1, data = spend_data_sample, subject = "Id")
  ids <- data.frame(Id = unique(spend_data_sample$Id)[1:10])

  pred_null <- GGcov_expected_spend(fit_null, newdata = ids, history = spend_data_sample)
  expect_equal(nrow(pred_null), 10L)
  expect_true(all(pred_null$expected_spend > 0))

  # Prior expectation for null model equals p * gamma / (q - 1)
  p   <- unname(fit_null$par[1])
  q   <- unname(fit_null$par[2])
  gam <- unname(fit_null$par[3])
  pred_null_prior <- GGcov_expected_spend(fit_null, newdata = ids[1, , drop = FALSE])
  expect_equal(pred_null_prior$expected_spend, p * gam / (q - 1))

  # Artificially set q <= 1 with no history to trigger NA and warning
  fit_low_q <- fit_null
  fit_low_q$par[2] <- 0.5
  expect_warning(
    res_na <- GGcov_expected_spend(fit_low_q, newdata = ids[1, , drop = FALSE], history = NULL),
    "p \\* x \\+ q <= 1"
  )
  expect_true(is.na(res_na$expected_spend))
})

test_that("GGcov_expected_spend validates inputs", {
  fit <- GGcov_MLE(Price ~ discount, data = spend_data_sample, subject = "Id")
  nd  <- spend_data_sample[1:5, c("Id", "discount")]

  expect_error(
    GGcov_expected_spend(list(), newdata = nd),
    "'fit' must be a model of class \"GGcov\""
  )
  expect_error(
    GGcov_expected_spend(fit, newdata = as.list(nd)),
    "'newdata' must be a data frame"
  )
  expect_error(
    GGcov_expected_spend(fit, newdata = data.frame(discount = 0.1)),
    "Subject column 'Id' not found in 'newdata'"
  )
  expect_error(
    GGcov_expected_spend(fit, newdata = nd, history = list()),
    "'history' must be a data frame"
  )
  expect_error(
    GGcov_expected_spend(fit, newdata = nd, history = data.frame(Id = 1)),
    "Column\\(s\\) not found in 'history': Price"
  )
  bad_hist <- spend_data_sample[1:10, ]
  bad_hist$Price[1] <- -1
  expect_error(
    GGcov_expected_spend(fit, newdata = nd, history = bad_hist),
    "Spend column 'Price' in 'history' must be numeric, strictly positive"
  )
})
