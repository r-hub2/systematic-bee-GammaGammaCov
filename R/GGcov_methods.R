#' Print a Fitted Equation (6) Model
#'
#' @param x A fitted model of class \code{"GGcov"} from
#'   \code{\link{GGcov_MLE}}.
#' @param digits Number of significant digits to print. Default 4.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @examples
#' fit <- GGcov_MLE(Price ~ 1, data = spend_data_sample, subject = "Id")
#' print(fit, digits = 3)
#' @export
print.GGcov <- function(x, digits = 4, ...) {
  lvl <- format(100 * x$level, trim = TRUE)
  tab <- cbind(Estimate     = x$par,
               `Std. Error` = x$par_stderr,
               Lower        = x$par_lower,
               Upper        = x$par_upper)
  colnames(tab)[3:4] <- paste0(c("Lower ", "Upper "), lvl, "%")

  cat("Gamma-gamma model \n\n")
  print(signif(tab, digits))
  cat("\nNegative log-likelihood: ", format(x$value, digits = digits + 4), "\n",
      sep = "")
  cat("Convergence code: ", x$convergence, "\n", sep = "")
  invisible(x)
}
