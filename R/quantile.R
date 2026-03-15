# quantile.R

#' Get quantile(s) of p-curves corresponding to requested target_cdfs.
#' @inheritParams pdf
#' @param target_cdfs The CDFs of the p-curve distribution at which to determine the quantile p value.
#' @param tol Tolerance value for uniroot search function (default = 1e-8).
#' @export
quantile <- function(target_cdfs, mu, sigma, pi = 1, alpha = 1, tails = 2, tol = 1e-8) {
  f <- function(p, mu, sigma, pi, alpha, tails, target_cdf) {
    pcurveMix::cdf(p, mu, sigma, pi = pi, alpha = alpha, tails = tails) - target_cdf
  }
  nps <- length(target_cdfs)
  quantiles <- numeric(nps)
  for (i in 1:nps) {
    result <- stats::uniroot(f, interval = c(0,1), tol = tol, mu = mu, sigma = sigma, pi =  pi,
                    alpha = alpha, tails = tails, target_cdf = target_cdfs[i])
    quantiles[i] <- result$root
  }
  return(quantiles)
}

