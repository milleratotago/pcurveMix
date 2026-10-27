# cdfs.R

#' Compute CDFs of 1- or 2-sided p values under H1 for random-effects model.
#' @inheritParams pcurveMix-package
#' @returns Real CDF value or vector of CDF values.
#' @export
cdf <- function(ps, mu, sigma, pi = 1, alpha = 1, tails = 2) {
  cdfs <- switch(
    case_id(alpha, tails, pi),
    "uncond_2t_h1"  = F_uncond_2t_h1  (ps, mu, sigma),
    "uncond_2t_mix" = F_uncond_2t_mix (ps, mu, sigma, pi),
    "uncond_1t_h1"  = F_uncond_1t_h1  (ps, mu, sigma),
    "uncond_1t_mix" = F_uncond_1t_mix (ps, mu, sigma, pi),
    "cond_2t_h1"    =   F_cond_2t_h1  (ps, mu, sigma, alpha),
    "cond_2t_mix"   =   F_cond_2t_mix (ps, mu, sigma, pi, alpha),
    "cond_1t_h1"    =   F_cond_1t_h1  (ps, mu, sigma, alpha),
    "cond_1t_mix"   =   F_cond_1t_mix (ps, mu, sigma, pi, alpha)
  )
  too_small <- ps <= 0
  too_large <- ps >= alpha
  cdfs[too_small] <- 0
  cdfs[too_large] <- 1
  return(cdfs)
}

# Compute CDF of 2-sided p value for random-effects model.
F_uncond_2t_h1 <- function(p, mu, sigma) {
  z <- stats::qnorm(p / 2)
  s <- sqrt(1 + sigma^2)
  stats::pnorm((z - mu) / s) + stats::pnorm((z + mu) / s)
}

# Compute CDF of 2-sided p value for mixture of H0 & H1 random-effects model.
F_uncond_2t_mix  <- function(p, mu, sigma, pi) (1 - pi) * p + pi * F_uncond_2t_h1(p, mu, sigma)

F_uncond_1t_h1 <- function(p, mu, sigma) {
  z <- stats::qnorm(p, lower.tail = FALSE)
  s <- sqrt(1 + sigma^2)
  log_cdf_vals <- stats::pnorm((mu - z) / s, lower.tail = TRUE, log.p = TRUE)
  cdf_vals <- exp(log_cdf_vals)
  # The following original version may have lower precision:  PRECISION ISSUES
  # z <- stats::qnorm(1 - p)
  # s <- sqrt(1 + sigma^2)
  # stats::pnorm((mu - z) / s)
}

F_uncond_1t_mix  <- function(p, mu, sigma, pi) (1 - pi) * p + pi * F_uncond_1t_h1(p, mu, sigma)

F_cond_2t_h1 <- function(p, mu, sigma, alpha) {
  Cnorm <- F_uncond_2t_h1(alpha, mu, sigma)
  F_uncond_2t_h1(p, mu, sigma) / Cnorm
}

# Conditional CDF of p values given p<alpha
#  as a function of model parameters.
F_cond_2t_mix <- function(p, mu, sigma, pi, alpha) {
  Cnorm <- (1 - pi) * alpha + pi * F_uncond_2t_h1(alpha, mu, sigma)
  F_uncond_2t_mix(p, mu, sigma, pi) / Cnorm
}

F_cond_1t_h1 <- function(p, mu, sigma, alpha) {
  Cnorm <- F_uncond_1t_h1(alpha, mu, sigma)
  F_uncond_1t_h1(p, mu, sigma) / Cnorm
}

F_cond_1t_mix  <- function(p, mu, sigma, pi, alpha) {
  Cnorm <- (1 - pi) * alpha + pi * F_uncond_1t_h1(alpha, mu, sigma)
  F_uncond_1t_mix(p, mu, sigma, pi) / Cnorm
}



