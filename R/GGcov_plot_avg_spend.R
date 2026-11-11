#' Plot Observed versus Fitted Average Spend
#'
#' Compares the observed distribution of each customer's average spend per
#' transaction, \eqn{\bar{z}}, with the distribution implied by a fitted
#' gamma-gamma model, as in Figure 3 of Fader and Hardie (2013).
#'
#' For a customer with \eqn{x} transactions, the fitted density of
#' \eqn{\bar{z}} is the average over customers of
#' \deqn{f(\bar{z}) = \frac{1}{c} \,
#'   \frac{\Gamma(k + q)}{\Gamma(k)\Gamma(q)} \,
#'   \frac{\gamma^q (\bar{z}/c)^{k-1}}{(\gamma + \bar{z}/c)^{k+q}},}
#' where \eqn{a_1 = \sum_j e^{w_j\beta'}}, \eqn{a_2 = \sum_j e^{2 w_j\beta'}},
#' \eqn{k = p a_1^2 / a_2} and \eqn{c = a_2 / (x a_1)}.
#'
#' \itemize{
#'   \item If the customer's covariate effect is the same on every
#'     transaction (no covariates, or static covariates), then
#'     \eqn{k = p x} and the density is exact.
#'   \item If the covariates change across a customer's transactions, the
#'     total spend is a sum of gamma variables with different rates, which has
#'     no closed form. The total is then approximated by a gamma distribution
#'     with the same mean and variance (given \eqn{\nu_0}), the
#'     Satterthwaite-Welch approximation, which gives the expression above. The plot says so when this approximation is used.
#' }
#'
#' @param fit A fitted model of class \code{"GGcov"} from
#'   \code{\link{GGcov_MLE}}.
#' @param data The transaction data frame used to fit the model (subject,
#'   spend and covariate columns).
#' @param max_customers Maximum number of customers used to draw the fitted
#'   curve. If there are more customers, a random subset is used for the
#'   curve (the observed density always uses all customers).
#' @param n Number of points used to draw the curves.
#' @param xlab,ylab,main Axis labels and main title.
#' @return Invisibly, a list with the plotted \code{grid}, the \code{observed}
#'   and \code{fitted} densities, the number of customers (\code{n_customers})
#'   and the number whose covariates vary over time (\code{n_time_varying}).
#' @references Fader, P. S., & Hardie, B. G. S. (2013). \emph{The Gamma-Gamma
#'   Model of Monetary Value} (Research Note No. 025).
#'   \url{https://brucehardie.com/notes/025/}
#'   
#'   Satterthwaite, F. E. (1946). An approximate distribution of estimates of
#'   variance components. \emph{Biometrics Bulletin}, 2(6), 110-114.
#'   
#'   Welch, B. L. (1947). The generalization of "Student's" problem when
#'   several different population variances are involved. \emph{Biometrika},
#'   34(1/2), 28-35.
#'   
#'   Moschopoulos, P. G. (1985). The distribution of the sum of independent
#'   gamma random variables. \emph{Annals of the Institute of Statistical
#'   Mathematics}, 37, 541-544.
#' @examples
#' fit <- GGcov_MLE(
#'   Price ~ discount + gender + channel,
#'   data    = spend_data_sample,
#'   subject = "Id"
#' )
#' GGcov_plot_avg_spend(fit, spend_data_sample)
#' @export
#' @importFrom graphics plot lines legend mtext title
#' @importFrom stats density quantile model.frame model.matrix na.fail approx
#'   delete.response
GGcov_plot_avg_spend <- function(fit, data, max_customers = 5000, n = 400,
                                 xlab = "Average spend per transaction",
                                 ylab = "Density",
                                 main = "Observed versus fitted average spend") {

  if (!inherits(fit, "GGcov"))
    stop("'fit' must be a model of class \"GGcov\" from GGcov_MLE().",
         call. = FALSE)
  if (!is.data.frame(data))
    stop("'data' must be a data frame.", call. = FALSE)

  subject <- fit$subject_name
  spend   <- fit$spend_name
  miss    <- setdiff(c(subject, spend), names(data))
  if (length(miss) > 0L)
    stop("Column(s) not found in 'data': ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  z <- data[[spend]]
  if (!is.numeric(z) || anyNA(z) || any(z <= 0))
    stop("Spend column '", spend,
         "' must be numeric, strictly positive and have no NA.", call. = FALSE)

  p    <- unname(fit$par[1])
  q    <- unname(fit$par[2])
  gam  <- unname(fit$par[3])
  beta <- fit$par[-(1:3)]

  # Linear predictor w * beta' for each transaction (0 without covariates)
  if (length(beta) == 0L) {
    eta <- numeric(nrow(data))
  } else {
    tt <- delete.response(fit$terms)
    mf <- model.frame(tt, data, xlev = fit$xlevels, na.action = na.fail)
    X  <- model.matrix(tt, mf)[, -1, drop = FALSE]
    if (!identical(colnames(X), names(beta)))
      stop("Covariate columns of 'data' do not match the fitted model.",
           call. = FALSE)
    eta <- c(X %*% beta)
  }

  # Per-customer sums: x, total spend, a1 = sum exp(eta), a2 = sum exp(2 eta)
  sums <- rowsum(cbind(1, z, exp(eta), exp(2 * eta)), data[[subject]])
  x    <- sums[, 1]
  zbar <- sums[, 2] / x
  a1   <- sums[, 3]
  a2   <- sums[, 4]

  k     <- p * a1^2 / a2                    # effective shape (= p x if exact)
  cc    <- a2 / (x * a1)                    # scale of the average
  n_var <- sum(a1^2 / (x * a2) < 1 - 1e-8)  # customers with varying covariates

  # x-axis range and grid
  xmax <- unname(quantile(zbar, 0.99))
  grid <- seq(xmax * 1e-3, xmax, length.out = n)

  # Observed density of the average spend
  obs <- density(zbar, from = 0, to = xmax)

  # Fitted density: average over customers of the density given (x, a1, a2)
  use <- if (length(zbar) > max_customers)
    sample.int(length(zbar), max_customers) else seq_along(zbar)
  kk <- k[use]; ccc <- cc[use]
  lc <- lgamma(kk + q) - lgamma(kk) - lgamma(q) + q * log(gam) - log(ccc)
  fitted <- vapply(grid, function(zz) {
    y <- zz / ccc
    mean(exp(lc + (kk - 1) * log(y) - (kk + q) * log(gam + y)))
  }, numeric(1))

  plot(obs$x, obs$y, type = "l", lwd = 2, col = "grey30",
       xlab = xlab, ylab = ylab, main = "",
       ylim = c(0, max(obs$y, fitted) * 1.05))
  lines(grid, fitted, lwd = 2, col = "darkblue")
  title(main = main, line = 2.2)
  legend("topright", legend = c("Observed (kernel density)", "Fitted model"),
         col = c("grey30", "darkblue"), lwd = 2, bty = "n")

  if (n_var > 0L)
    mtext(sprintf(paste0("Fitted density is a gamma approximation for %d of ",
                         "%d customers with time-varying covariates"),
                  n_var, length(zbar)),
          side = 3, line = 0.6, cex = 0.85, col = "grey30")

  invisible(list(grid = grid, observed = approx(obs$x, obs$y, grid)$y,
                 fitted = fitted, n_customers = length(zbar),
                 n_time_varying = n_var))
}
