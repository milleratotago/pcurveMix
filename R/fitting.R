# fitting.R

# Compute negative log-likelihood of a p value or vector of p values
#  under the model with the indicated parameters.
# @param p Real 0-1 p value or vector for which nll is to be computed.
# @inheritParams pdf
# @returns Real negative log-likelihood of the p values.
# @export
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
#' @param alpha_sig Significance cutoff used in computing the estimated average
#'  power when H0 is false (default = 0.05).
#' @param start List of starting parameter values for the optim search
#'  (defaults:  pi = 0.5, mu = 2, sigma = 2).
#' @param lower Vector of lower bounds for pi, mu, and sigma
#'  (defaults = c(1e-6, 0, 1e-6))
#' @param upper Vector of upper bounds for pi, mu, and sigma
#'  (defaults = c(1 - 1e-6, 15, 10))
#' @returns List including estimated parameter values, their standard errors
#'  and 95% confidence limits, an estimate of the average power to reject
#'  H0 when it is false, and more.
#' @export
fit_p_curve <- function(p, alpha = 1, tails = 2, alpha_sig = 0.05,
                        start = list(pi = 0.5, mu = 2, sigma = 2),
                        lower = c(1e-6,      0, 1e-6),
                        upper = c(1 - 1e-6, 15, 10)) {
  p <- as.numeric(p);
  check_ps_list <- check_ps(p, alpha_cutoff = alpha)
  if (!check_ps_list$all_in_bounds) {
    p <- check_ps_list$ps_in_bounds
  }
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
  fit <- list(alpha = alpha, alpha_sig = alpha_sig, tails = tails,
              pi = est[1], mu = est[2], sigma = est[3],
              se = se, ci95 = ci, logLik = -opt$value,
              converged = (opt$convergence == 0), n = length(p), min_p = min(p), max_p = max(p),
              check_ps_list = check_ps_list)
  fit$power_hat <- cdf(alpha_sig, mu = fit$mu, sigma = fit$sigma, pi = 1, alpha = 1, tails = tails)
  cdf_fit <- function(x) cdf(x, mu = fit$mu, sigma = fit$sigma, pi = fit$pi, alpha = 1, tails = tails)
  fit$ks <- ks_with_cdf(p, cdf_fit)
  return(fit)
} # fit_p_curve

# ---------- Helper: KS with tiny jitter to avoid ties warnings ----------
ks_with_cdf <- function(p, cdf_fun, jitter_scale = 1e-9) {
  p2 <- if (any(duplicated(p))) p + stats::runif(length(p), -jitter_scale, jitter_scale) else p
  suppressWarnings(stats::ks.test(p2, cdf_fun))
}

#' Convert the fit_p_curve parameter estimates into a nice data frame.
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

#' Convert the fit_p_curve fit descriptors into a nice data frame.
#' @param fit Output list from fit_p_curve
#' @param file_name Optional string used to include p file name in table
#' @returns A data frame
#' @export
fit_to_descriptor_tbl <- function(fit, file_name = NULL) {
  descriptor_tbl <- data.frame()
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("---FITTING OPTIONS---", "-------------"))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("alpha",as.character(round(fit$alpha,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("tails",as.character(round(fit$tails,0))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("alpha_sig",as.character(round(fit$alpha_sig,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("---DATASET OF P's---", "-------------"))
  if (!is.null(file_name)) descriptor_tbl <- rbind(descriptor_tbl, descriptor("file name", file_name))
  if (!fit$check_ps_list$all_in_bounds) {
    if (fit$check_ps_list$n_too_small > 0) descriptor_tbl <- rbind(descriptor_tbl, descriptor("n excluded p's < 0", as.character(round(fit$check_ps_list$n_too_small,0))))
    if (fit$check_ps_list$n_equal_zero > 0) descriptor_tbl <- rbind(descriptor_tbl, descriptor("n small p's set to 1e-12", as.character(round(fit$check_ps_list$n_equal_zero,0))))
    if (fit$check_ps_list$n_too_large > 0) descriptor_tbl <- rbind(descriptor_tbl, descriptor("n excluded p's > alpha_cutoff", as.character(round(fit$check_ps_list$n_too_large,0))))
  }
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("n_fitted_p's", as.character(round(fit$n,0))))
  smin <- formatC(fit$min_p, format = "e", digits = 6)
  if (fit$max_p < 0.001) {
    smax <- formatC(fit$max_p, format = "e", digits = 6)
  } else {
    smax <- as.character(round(fit$max_p,6))
  }
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("min(p)", smin))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("max(p)", smax))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("---FITTING RESULTS---", "-------------"))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("real p fit converged",as.character(fit$converged)))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("logLik",as.character(round(fit$logLik,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("ks statistic",as.character(round(fit$ks$statistic,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("ks p value",as.character(round(fit$ks$p.value,5))))
  rownames(descriptor_tbl) <- NULL
  return(descriptor_tbl)
}

descriptor <- function(slabel, svalue) {
  tbl <- data.frame("Property" = slabel, "Value" = svalue)
  return(tbl)
}
