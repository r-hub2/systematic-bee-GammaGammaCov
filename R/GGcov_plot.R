#' Plot the Mixing Distribution of a Fitted Gamma-Gamma Model
#'
#' Plots the gamma mixing distribution of nu0 implied by a fitted model: shape
#' q (mixture shape) and rate gamma (mixture rate). The mean (dashed) and
#' median (dotted) are marked with vertical lines and shown in a second title.
#'
#' With covariates in the model, nu0 is the rate for a subject whose covariates
#' are all 0 on the original scale (the fitted coefficients are on the original
#' scale), so the plotted distribution describes that baseline.
#'
#' @param fit A fitted model of class \code{"GGcov"} from
#'   \code{\link{GGcov_MLE}}.
#' @param coverage Central probability mass of the mixing distribution to show
#'   on the x-axis. Default 0.999.
#' @param n Number of points used to draw the curve. Default 400.
#' @param xlab,ylab,main Axis labels and title.
#' @param ... Further arguments passed to \code{\link[graphics]{curve}}.
#' @return Invisibly returns a list with the \code{mean} (q / gamma) and
#'   \code{median} of the mixing distribution.
#' @examples
#' fit <- GGcov_MLE(
#'   Price ~ discount + gender + channel,
#'   data    = spend_data_sample,
#'   subject = "Id"
#' )
#' GGcov_plot_mixing(fit)
#' @export
#' @importFrom stats dgamma qgamma
#' @importFrom graphics curve abline mtext
GGcov_plot_mixing <- function(fit, coverage = 0.999, n = 400,
                              xlab = expression(nu[0]),
                              ylab = "Density",
                              main = "Mixing distribution", ...) {
  if (!inherits(fit, "GGcov"))
    stop("'fit' must be a model of class \"GGcov\" from GGcov_MLE().",
         call. = FALSE)
  if (!is.numeric(coverage) || length(coverage) != 1L || is.na(coverage) ||
      coverage <= 0 || coverage >= 1)
    stop("'coverage' must be a single number strictly between 0 and 1.",
         call. = FALSE)

  q   <- unname(fit$par[2])   # mixture shape
  gam <- unname(fit$par[3])   # mixture rate

  # Density of the gamma mixing distribution of nu0.
  dmix <- function(x) dgamma(x, shape = q, rate = gam)

  # Closed-form mean and median.
  mean_nu   <- q / gam
  median_nu <- qgamma(0.5, shape = q, rate = gam)

  # Plot between the tail quantiles so the curve is well scaled and never
  # evaluated at 0 (the density is infinite there when q < 1).
  tail <- (1 - coverage) / 2
  curve(dmix,
        from = qgamma(tail,     shape = q, rate = gam),
        to   = qgamma(1 - tail, shape = q, rate = gam),
        n = n, lwd = 2, col = "darkblue",
        main = main, xlab = xlab, ylab = ylab, ...)
  abline(v = mean_nu,   col = "red3",       lwd = 2, lty = 2)
  abline(v = median_nu, col = "darkorange", lwd = 2, lty = 3)

  # Second title: mean and median (line styles match the vertical lines).
  mtext(sprintf("Mean (dashed) = %.4g   |   Median (dotted) = %.4g",
                mean_nu, median_nu),
        side = 3, line = 0.4, cex = 0.9)

  invisible(list(mean = mean_nu, median = median_nu))
}
