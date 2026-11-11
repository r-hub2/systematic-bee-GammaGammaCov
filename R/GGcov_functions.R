#' Negative Log-Likelihood for Equation (6)
#'
#' @param par c(log p, log q, log gamma, beta), optimizer scale.
#' @param data_df Data frame: column 1 = id, column 2 = z (outcome, > 0),
#'   columns 3+ = covariates W (none for the null model, in which case
#'   \code{par} is just c(log p, log q, log gamma)). One row per subject per
#'   period.
#' @return Scalar negative log-likelihood.
GGcov_LL <- function(par, data_df) {
  p    <- exp(par[1])
  q    <- exp(par[2])
  gam  <- exp(par[3])
  beta <- par[-(1:3)]

  # Guard: for absurdly large p, q or gamma (|log| > 20, i.e. above about 5e8)
  # the terms of the log-likelihood are so large that floating-point
  # cancellation can make the computed value look spuriously good (even 0).
  # Returning Inf makes the optimizer back off from such regions.
  if (any(abs(par[1:3]) > 20)) return(Inf)

  id <- data_df[[1]]
  z  <- data_df[[2]]
  W  <- as.matrix(data_df[, -(1:2), drop = FALSE])

  # w_i beta'. With no covariate columns (the null model) this is 0, so
  # A(x) = 1 and B(x) = sum of z.
  eta <- if (ncol(W) == 0L) numeric(length(z)) else c(W %*% beta)

  # Per-id sums (reorder = FALSE keeps first-appearance order for all of them)
  x     <- c(rowsum(rep(1, length(z)), id, reorder = FALSE))  # rows per id
  Slogz <- c(rowsum(log(z),            id, reorder = FALSE))  # sum log z_i
  Seta  <- c(rowsum(eta,               id, reorder = FALSE))  # sum w_i beta'
  B     <- c(rowsum(z * exp(-eta),     id, reorder = FALSE))  # B(x)

  # log of eq (6), per id:
  # (p-1) sum log z - p sum(w beta') - x lgamma(p)
  #   + lgamma(px + q) - lgamma(q) + q log(gamma) - (px + q) log(gamma + B)
  ll <- (p - 1) * Slogz - p * Seta - x * lgamma(p) +
    lgamma(p * x + q) - lgamma(q) + q * log(gam) -
    (p * x + q) * log(gam + B)

  -sum(ll)
}
