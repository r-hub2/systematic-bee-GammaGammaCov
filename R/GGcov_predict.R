#' Expected Spend per Transaction
#'
#' Computes the expected spend on future transactions from a fitted
#' gamma-gamma model, given each customer's observed history. For a customer
#' with x past transactions (spends z_1, ..., z_x and covariates w_1, ..., w_x)
#' and a future transaction with covariates w_k, the expected spend is
#'
#' \deqn{E(Z_k \mid \cdot) = \frac{p \, (\gamma + B(x))}{p x + q - 1}
#'   e^{w_k \beta'}, \qquad B(x) = \sum_{i=1}^{x} z_i e^{-w_i \beta'},}
#'
#' which requires \eqn{p x + q > 1} (Fader and Hardie, 2024). One formula
#' covers all cases:
#' \itemize{
#'   \item time-varying covariates: each past and future transaction has its
#'     own covariate values;
#'   \item static covariates: repeat each customer's values on every row, and
#'     B(x) reduces to the total spend times \eqn{e^{-w\beta'}};
#'   \item no past transactions (x = 0, or a customer absent from
#'     \code{history}): B(x) = 0, giving the prior expectation
#'     \eqn{p \gamma e^{w_k\beta'} / (q - 1)}, which requires q > 1;
#'   \item a model without covariates: \eqn{e^{w_k\beta'} = 1}.
#' }
#'
#' @param fit A fitted model of class \code{"GGcov"} from
#'   \code{\link{GGcov_MLE}}.
#' @param newdata Data frame with one row per future transaction to predict:
#'   the subject column and the covariates (original, unstandardized values;
#'   factors are coded as in the fit) in effect for that transaction.
#' @param history Optional data frame of observed transactions, with the
#'   subject column, the spend column and the covariates, laid out like the
#'   data used for \code{\link{GGcov_MLE}}. If \code{NULL}, every customer is
#'   treated as having no past transactions.
#' @return A data frame with one row per row of \code{newdata}: the subject
#'   identifier, \code{n_trans} (number of past transactions used) and
#'   \code{expected_spend}. Entries are \code{NA} where the formula requires
#'   a condition that fails (\eqn{p x + q \le 1}).
#' @references Fader, P. S., & Hardie, B. G. S. (2024). \emph{Incorporating
#'   Covariates in the Gamma-Gamma Spend Model} (Research Note No. 045).
#'   \url{https://brucehardie.com/notes/045/}
#' @examples
#' fit <- GGcov_MLE(
#'   Price ~ discount + gender + channel,
#'   data    = spend_data_sample,
#'   subject = "Id"
#' )
#' last_rows <- spend_data_sample[!duplicated(spend_data_sample$Id, fromLast = TRUE), ]
#' future <- last_rows[1:5, c("Id", "discount", "gender", "channel")]
#' future$discount <- 0.1
#' GGcov_expected_spend(fit, newdata = future, history = spend_data_sample)
#' @export
#' @importFrom stats model.frame model.matrix na.fail
GGcov_expected_spend <- function(fit, newdata, history = NULL) {

  if (!inherits(fit, "GGcov"))
    stop("'fit' must be a model of class \"GGcov\" from GGcov_MLE().",
         call. = FALSE)
  if (!is.data.frame(newdata))
    stop("'newdata' must be a data frame.", call. = FALSE)

  subject <- fit$subject_name
  if (!subject %in% names(newdata))
    stop("Subject column '", subject, "' not found in 'newdata'.",
         call. = FALSE)

  p     <- unname(fit$par[1])
  q     <- unname(fit$par[2])
  gam   <- unname(fit$par[3])
  beta  <- fit$par[-(1:3)]

  # Linear predictor w * beta' for each row of a data frame, with covariates
  # coded exactly as in the fit (original scale: fit$par is on that scale).
  linear_predictor <- function(df) {
    if (length(beta) == 0L) return(numeric(nrow(df)))
    mf <- model.frame(fit$terms, df, xlev = fit$xlevels, na.action = na.fail)
    X  <- model.matrix(fit$terms, mf)[, -1, drop = FALSE]
    if (!identical(colnames(X), names(beta)))
      stop("Covariate columns of the data do not match the fitted model.",
           call. = FALSE)
    c(X %*% beta)
  }

  eta_new <- linear_predictor(newdata)
  id_new  <- as.character(newdata[[subject]])

  # Per-customer history summaries: x (number of past transactions) and
  # B(x) = sum of z * exp(-w beta').
  x_new <- numeric(length(id_new))
  B_new <- numeric(length(id_new))
  if (!is.null(history)) {
    if (!is.data.frame(history))
      stop("'history' must be a data frame.", call. = FALSE)
    spend <- fit$spend_name
    miss  <- setdiff(c(subject, spend), names(history))
    if (length(miss) > 0L)
      stop("Column(s) not found in 'history': ",
           paste(miss, collapse = ", "), ".", call. = FALSE)
    z <- history[[spend]]
    if (!is.numeric(z) || anyNA(z) || any(z <= 0))
      stop("Spend column '", spend,
           "' in 'history' must be numeric, strictly positive and have no NA.",
           call. = FALSE)

    id_h  <- history[[subject]]
    eta_h <- linear_predictor(history)
    x_h   <- rowsum(rep(1, length(z)),     id_h, reorder = FALSE)
    B_h   <- rowsum(z * exp(-eta_h),       id_h, reorder = FALSE)
    key   <- rownames(x_h)

    pos   <- match(id_new, key)
    found <- !is.na(pos)
    x_new[found] <- x_h[pos[found], 1]
    B_new[found] <- B_h[pos[found], 1]
  }

  denom <- p * x_new + q - 1
  out   <- p * (gam + B_new) / denom * exp(eta_new)
  out[denom <= 0] <- NA_real_
  if (anyNA(out))
    warning("Expected spend is NA where p * x + q <= 1 (for example, a ",
            "customer with no history and q <= 1).", call. = FALSE)

  res <- data.frame(newdata[[subject]], x_new, out)
  names(res) <- c(subject, "n_trans", "expected_spend")
  res
}
