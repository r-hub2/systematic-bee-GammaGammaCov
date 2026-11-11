#' Fit the Equation (6) Model by Maximum Likelihood
#'
#' Fits the model in which, for each subject, the outcomes z_1, ..., z_x are
#' gamma distributed with shape p and rate nu0 * exp(-w_i beta'), and nu0 has a
#' gamma(q, gamma) distribution that is integrated out (equation 6). Covariates
#' are centered and scaled internally for numerical stability, and the results
#' are converted back to the original covariate scale.
#'
#' @param fo Formula \code{z ~ w1 + w2 + ...}: the outcome on the left, plain
#'   covariate names on the right. Transformations such as \code{log(x)} or
#'   \code{I(x^2)} and interactions are rejected with an error; create
#'   transformed variables as new columns in \code{data} first. Do not include
#'   an intercept: gamma plays that role. Use \code{z ~ 1} to fit the null model
#'   with no covariates (p, q and gamma only); it is nested in any model with
#'   covariates (all coefficients equal to 0), so \code{2 * (null$value -
#'   full$value)} is a likelihood-ratio statistic with one degree of freedom
#'   per coefficient.
#' @param data Data frame in long format, one row per subject per period. Row
#'   order does not matter. Covariates may be time-varying or static: a static
#'   covariate is handled by repeating its value on each of the subject's rows.
#' @param subject Character string naming the subject ID column in \code{data}.
#' @param level Confidence level for the Wald intervals, strictly between 0 and
#'   1. Default 0.95.
#'
#' @return A list of class \code{"GGcov"} containing the \code{optim}
#'   results with additional elements. All estimates, standard errors and
#'   bounds are on the original covariate scale (only \code{hessian} is on the
#'   internal parameterization):
#'   \item{par}{Estimates: p (shape), q (mixture shape: the shape of the gamma
#'     mixing distribution of nu0), gamma (mixture rate: its rate), then the
#'     covariate coefficients.}
#'   \item{par_stderr}{Standard errors. For p, q and gamma these use the delta
#'     method, e.g. SE(p) is approximately p * SE(log p). \code{NA} with a
#'     warning if the Hessian is not positive definite.}
#'   \item{par_lower, par_upper}{Wald limits at \code{level}. For p, q and gamma
#'     the interval is built on the log scale and exponentiated.}
#'   \item{level}{The confidence level used.}
#'   \item{value}{Negative log-likelihood at the optimum.}
#'   \item{convergence}{optim convergence code (0 = success; any other code
#'     triggers a warning).}
#'   \item{hessian}{Hessian on the internal parameterization (log p, log q,
#'     log gamma, standardized coefficients).}
#'   \item{center, scale}{Covariate means and standard deviations used
#'     internally.}
#'   \item{terms, xlevels}{Covariate terms and factor levels, for coding new
#'     data.}
#'   \item{spend_name, subject_name}{Names of the outcome and subject columns
#'     used in the fit, so \code{\link{GGcov_expected_spend}} can find them in
#'     new data.}
#'
#' @examples
#' fit <- GGcov_MLE(
#'   Price ~ discount + gender + channel,
#'   data    = spend_data_sample,
#'   subject = "Id"
#' )
#' print(fit)
#'
#' # Null model without covariates
#' fit_null <- GGcov_MLE(Price ~ 1, data = spend_data_sample, subject = "Id")
#' print(fit_null)
#'
#' @export
#' @importFrom stats optim as.formula model.matrix model.frame terms qnorm
#' @importFrom stats .getXlevels
GGcov_MLE <- function(fo, data, subject, level = 0.95) {

  # --- Input validation -------------------------------------------------------
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      level <= 0 || level >= 1)
    stop("'level' must be a single number strictly between 0 and 1.",
         call. = FALSE)
  if (!is.data.frame(data))
    stop("'data' must be a data frame.", call. = FALSE)
  if (!is.character(subject) || length(subject) != 1L)
    stop("'subject' must be a single character string.", call. = FALSE)
  if (!subject %in% names(data))
    stop("Subject column '", subject, "' not found in 'data'.", call. = FALSE)

  fo <- as.formula(fo)
  if (length(fo) != 3L || !is.name(fo[[2L]]))
    stop("The left-hand side of the formula must be a single variable name.",
         call. = FALSE)

  vars      <- all.vars(fo)
  z_name    <- vars[1]
  cov_names <- vars[-1]

  # No covariates (z ~ 1) fits the null model.
  null_model <- length(cov_names) == 0L

  missing_cols <- setdiff(c(z_name, cov_names), names(data))
  if (length(missing_cols) > 0L)
    stop("Column(s) not found in 'data': ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)

  # Every right-hand-side term must be a plain variable (the formula is only
  # used to read off names, so transformations would be silently ignored).
  term_labels <- gsub("`", "", attr(terms(fo), "term.labels"), fixed = TRUE)
  if (!setequal(term_labels, cov_names))
    stop("The right-hand side of the formula must contain only plain variable ",
         "names (no transformations or interactions). Offending term(s): ",
         paste(setdiff(union(term_labels, cov_names),
                       intersect(term_labels, cov_names)), collapse = ", "),
         ". Create transformed variables as new columns in 'data' first.",
         call. = FALSE)

  na_cols <- c(subject, z_name, cov_names)
  na_cols <- na_cols[vapply(na_cols,
                            function(v) anyNA(data[[v]]), logical(1L))]
  if (length(na_cols) > 0L)
    stop("NA values found in column(s): ",
         paste(na_cols, collapse = ", "),
         ". Remove or impute missing values before fitting.", call. = FALSE)

  if (!is.numeric(data[[z_name]]) || any(data[[z_name]] <= 0))
    stop("Outcome column '", z_name, "' must be numeric and strictly positive.",
         call. = FALSE)
  # ---------------------------------------------------------------------------

  # Design matrix for the covariates only. model.matrix() expands factors and
  # adds an intercept column, which is dropped below (gamma plays that role).
  if (null_model) {
    # Nothing to code or standardize: no covariate columns at all.
    Xs      <- matrix(numeric(0), nrow = nrow(data), ncol = 0L)
    ctr     <- numeric(0)
    scl     <- numeric(0)
    tt      <- NULL
    mf_cov  <- NULL
    xlevels <- NULL
  } else {
    df_temp <- data[, cov_names, drop = FALSE]
    X       <- model.matrix(~ ., data = df_temp)
    mf_cov  <- model.frame(~ ., data = df_temp)
    tt      <- attr(mf_cov, "terms")
    xlevels <- .getXlevels(tt, mf_cov)

    # Standardize. Exact reparameterization: scaling rescales the coefficients,
    # centering is absorbed by gamma. Both are undone below.
    Xs  <- scale(X[, -1, drop = FALSE])
    ctr <- attr(Xs, "scaled:center")
    scl <- attr(Xs, "scaled:scale")
    no_var <- is.na(scl) | scl <= sqrt(.Machine$double.eps) * pmax(1, abs(ctr))
    if (any(no_var))
      stop("Covariate(s) with no variation: ",
           paste(names(scl)[no_var], collapse = ", "),
           ". Remove them before fitting.", call. = FALSE)
  }

  # Data frame in the layout GGcov_LL() expects: id, z, standardized W.
  model_data <- data.frame(id = data[[subject]],
                           z  = data[[z_name]])
  if (!null_model) {
    model_data <- cbind(model_data, as.data.frame(Xs))
  }

  solution <- GGcov_optim(model_data)

  if (solution$convergence != 0)
    warning("optim() did not converge (convergence code ", solution$convergence,
            "); estimates and standard errors may be unreliable.", call. = FALSE)

  par_names  <- c("p (shape)", "q (mixture shape)", "gamma (mixture rate)",
                  colnames(Xs))
  hess_names <- c("log(p)", "log(q)", "log(gamma)", colnames(Xs))
  dimnames(solution$hessian) <- list(hess_names, hess_names)

  # Covariance of the internal-scale estimates: inverse Hessian, used only
  # when the Hessian is finite and positive definite.
  hess_ok <- all(is.finite(solution$hessian)) &&
    min(eigen(solution$hessian, symmetric = TRUE, only.values = TRUE)$values) > 0
  hess_inv <- if (hess_ok) {
    tryCatch(solve(solution$hessian), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(hess_inv))
    warning("The Hessian at the solution is not positive definite (or could ",
            "not be inverted), so standard errors and confidence intervals ",
            "are NA.", call. = FALSE)

  # --- Reporting-scale results ------------------------------------------------
  # Computed from the untouched internal-scale estimates in solution$par
  # (log p, log q, log gamma', standardized coefficients beta') and the inverse
  # Hessian; the result vectors are assembled once at the end.
  z_crit <- qnorm(1 - (1 - level) / 2)

  se_internal <- if (is.null(hess_inv)) {
    rep(NA_real_, length(solution$par))
  } else {
    sqrt(diag(hess_inv))
  }

  # 1. Coefficients in original units: scaling by scl changes them, centering
  #    does not.
  b_idx <- seq_along(solution$par)[-(1:3)]
  b_est <- solution$par[b_idx] / scl
  b_se  <- se_internal[b_idx]  / scl
  b_lwr <- b_est - z_crit * b_se
  b_upr <- b_est + z_crit * b_se

  # 2. p and q: log-scale intervals, delta-method SEs.
  p_log <- solution$par[1]; p_log_se <- se_internal[1]
  p_est <- exp(p_log)
  p_se  <- p_est * p_log_se
  p_lwr <- exp(p_log - z_crit * p_log_se)
  p_upr <- exp(p_log + z_crit * p_log_se)

  q_log <- solution$par[2]; q_log_se <- se_internal[2]
  q_est <- exp(q_log)
  q_se  <- q_est * q_log_se
  q_lwr <- exp(q_log - z_crit * q_log_se)
  q_upr <- exp(q_log + z_crit * q_log_se)

  # 3. gamma on the original covariate scale. With w' = (w - ctr) / scl the
  #    linear predictor is eta' = eta - sum((ctr / scl) * beta'), so nu0 in the
  #    original model equals nu0' * exp(sum((ctr / scl) * beta')), which gives
  #    log(gamma) = log(gamma') - sum((ctr / scl) * beta'). This is a linear
  #    combination of the fitted parameters, so its variance is exactly
  #    a' %*% vcov %*% a. The interval is built on the log scale.
  a_vec         <- c(0, 0, 1, -ctr / scl)
  log_gamma     <- sum(a_vec * solution$par)
  se_log_gamma  <- if (is.null(hess_inv)) {
    NA_real_
  } else {
    sqrt(drop(t(a_vec) %*% hess_inv %*% a_vec))
  }
  g_est <- exp(log_gamma)
  g_se  <- g_est * se_log_gamma
  g_lwr <- exp(log_gamma - z_crit * se_log_gamma)
  g_upr <- exp(log_gamma + z_crit * se_log_gamma)

  solution$par        <- c(p_est, q_est, g_est, b_est)
  solution$par_stderr <- c(p_se,  q_se,  g_se,  b_se)
  solution$par_lower  <- c(p_lwr, q_lwr, g_lwr, b_lwr)
  solution$par_upper  <- c(p_upr, q_upr, g_upr, b_upr)
  names(solution$par)        <- par_names
  names(solution$par_stderr) <- par_names
  names(solution$par_lower)  <- par_names
  names(solution$par_upper)  <- par_names

  solution$center  <- ctr
  solution$scale   <- scl
  solution$level   <- level
  solution$terms   <- tt
  solution$xlevels <- xlevels
  solution$spend_name   <- z_name     # outcome column, for GGcov_expected_spend()
  solution$subject_name <- subject    # subject column, likewise
  class(solution)  <- c("GGcov", "list")
  solution
}

#' Internal Optimization for the Equation (6) Model
#'
#' BFGS on the log scale for p, q and gamma, so an unconstrained method is
#' appropriate.
#'
#' @param model_data Data frame prepared by \code{GGcov_MLE}: id, z, then
#'   the standardized covariate columns.
#' @return The unmodified list returned by \code{\link[stats]{optim}} (internal
#'   scale: log p, log q, log gamma, standardized coefficients), including the
#'   Hessian of the negative log-likelihood.
#' @keywords internal
GGcov_optim <- function(model_data) {

  # Parameters: log p, log q, log gamma, plus one coefficient per covariate
  # (model_data has id and z plus the covariate columns).
  ncov <- ncol(model_data) - 2L

  # Starting values on the data scale: p = 1, q = 2 and gamma = mean(z), which
  # matches the mean of z when p = 1 and q = 2 (the mean of z is roughly
  # p * gamma / (q - 1)). The coefficients start at 0 (no covariate effect).
  # Starting at the scale of the data keeps the first BFGS steps from jumping
  # to extreme parameter values. "NaNs produced" warnings from extreme trial
  # values during the search are muffled.
  start <- c(log(1), log(2), log(mean(model_data$z)), rep(0, ncov))

  withCallingHandlers(
    optim(par     = start,
          fn      = GGcov_LL,
          data_df = model_data,
          method  = "BFGS",
          control = list(maxit = 1000),
          hessian = TRUE),
    warning = function(w) {
      if (grepl("NaNs produced", conditionMessage(w)))
        invokeRestart("muffleWarning")
    }
  )
}
