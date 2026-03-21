# fitting.R

#' Compute negative log-likelihood of a p value or vector of p values
#'  under the model with the indicated parameters.
#' @param p Real 0-1 p value or vector for which nll is to be computed.
#' @inheritParams pdf
#' @returns Real negative log-likelihood of the p values.
#' @export
nll <- function(p, mu, sigma, pi = 1, alpha = 1, tails = 2) {
  if (pi <= 0 || pi >= 1 || sigma <= 0 || mu < 0) return(1e12)
  if (any(!is.finite(p)) || any(p <= 0 | p >= 1)) return(1e12)
  pdfs <- pdf(p, mu, sigma, pi, alpha, tails)
  -sum(log(pmax(pdfs, .Machine$double.xmin)))
}

# This version is just for optim's use; it unpacks to-be-adjusted
# parameters bundled in par.
# param par Vector of the three model parameters pi, mu, sigma (in order)
nll_optim <- function(par, p, alpha = 1, tails = 2) {
  pi <- par[1]; mu <- par[2]; sigma <- par[3]
  nll(p, mu, sigma, pi, alpha = alpha, tails = tails)
}

#' Fit estimates of model pi, mu, sigma to a vector of p values.
#' @param p Vector of p values in 0--1
#' @inheritParams pdf
#' @param start List of starting parameter values for optim search
#'  (defaults:  pi = 0.5, mu = 2, sigma = 2).
#' @param lower Vector of lower bounds for pi, mu, and sigma
#'  (defaults = c(1e-6, 0, 1e-6))
#' @param upper Vector of upper bounds for pi, mu, and sigma
#'  (defaults = c(1 - 1e-6, 15, 10))
#' @returns List with estimated parameter values, their standard errors
#'  and 95% confidence limits, etc.
#' @export
fit_p_curve <- function(p, alpha = 1, tails = 2,
                        start = list(pi = 0.5, mu = 2, sigma = 2),
                        lower = c(1e-6,      0, 1e-6),
                        upper = c(1 - 1e-6, 15, 10)) {
  p <- as.numeric(p); p <- p[p > 0 & p < 1]
  if (!length(p)) stop("No valid p-values in (0,1).")

  opt <- stats::optim(par = unlist(start), fn = nll_optim, p = p, alpha = alpha, tails = tails,
               method = "L-BFGS-B", lower = lower, upper = upper, hessian = TRUE)
  est <- opt$par; H <- opt$hessian
  se <- ci <- NULL
  if (is.matrix(H) && all(is.finite(H))) {
    Vinv <- try(solve(H), silent = TRUE)
    if (!inherits(Vinv, "try-error")) {
      se <- sqrt(pmax(diag(Vinv), 0)); names(se) <- c("pi","mu","sigma")
      z <- 1.96
      ci <- cbind(est - z*se, est + z*se)
      rownames(ci) <- c("pi","mu","sigma"); colnames(ci) <- c("lwr95","upr95")
      ci["pi",]    <- pmin(pmax(ci["pi",], 1e-6), 1 - 1e-6)
      ci["mu",]    <- pmax(ci["mu",], 0)
      ci["sigma",] <- pmax(ci["sigma",], 1e-6)
    }
  }
  fit <- list(pi = est[1], mu = est[2], sigma = est[3],
       se = se, ci95 = ci,
       logLik = -opt$value, converged = (opt$convergence == 0),
       n = length(p))
  fit$power_hat <- cdf(alpha, mu = fit$mu, sigma = fit$sigma, pi = 1, alpha = 1, tails = tails)
  cdf_fit <- function(x) cdf(x, mu = fit$mu, sigma = fit$sigma, pi = fit$pi, alpha = 1, tails = tails)
  fit$ks <- ks_with_cdf(p, cdf_fit)
  return(fit)
} # fit_p_curve

# ---------- Helper: KS with tiny jitter to avoid ties warnings ----------
ks_with_cdf <- function(p, cdf_fun, jitter_scale = 1e-9) {
  p2 <- if (any(duplicated(p))) p + stats::runif(length(p), -jitter_scale, jitter_scale) else p
  suppressWarnings(stats::ks.test(p2, cdf_fun))
}

#' Convert the output list from fit_p_curve
#'   into a nice data frame.
#' @param fit Output list from fit_p_curve
#' @returns A data frame
#' @importFrom rlang .data
#' @export
fit_to_estimates_tbl <- function(fit) {
  mle_tbl <- data.frame(
    parameter = c("pi","mu","sigma","power"),
    estimate  = c(fit$pi, fit$mu, fit$sigma, fit$power_hat),
    Wald_SE   = c(if (!is.null(fit$se)) fit$se else c(NA,NA,NA), NA),
    Wald_lwr  = c(if (!is.null(fit$ci95)) fit$ci95[, "lwr95"] else c(NA,NA,NA), NA),
    Wald_upr  = c(if (!is.null(fit$ci95)) fit$ci95[, "upr95"] else c(NA,NA,NA), NA),
    row.names = NULL
  )
  mle_tbl <- mle_tbl |> dplyr::arrange(factor(.data$parameter, levels = c("mu", "sigma", "pi", "power")))
  return(mle_tbl)
}
