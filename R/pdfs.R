# pdfs.R

#' Compute PDF(s) of p values for random-effects model.
#' @param p Real 0-1 p value or vector for which function values are to be computed.
#' @param mu Mean of noncentrality parameters when H1 is true.
#' @param sigma Standard deviation of noncentrality parameters when H1 is true.
#' @param pi Proportion of studies with H1 true in mixture of studies with H0 vs H1 true (default = 1).
#' @param tails 1 or 2 to indicate 1- or 2-tailed p values (default = 2).
#' @param alpha Maximum p value in distribution conditional on p<=alpha (default = 1).
#' @returns Real PDF value or vector of PDF values.
#' @export
pdf <- function(p, mu, sigma, pi = 1, alpha = 1, tails = 2) {
  p <- pmin(pmax(as.numeric(p), 1e-12), min(1 - 1e-12, alpha) )
  pdfs <- switch(
    case_id(alpha, tails, pi),
    "uncond_2t_h1"  = f_uncond_2t_h1  (p, mu, sigma),
    "uncond_2t_mix" = f_uncond_2t_mix (p, mu, sigma, pi),
    "uncond_1t_h1"  = f_uncond_1t_h1  (p, mu, sigma),
    "uncond_1t_mix" = f_uncond_1t_mix (p, mu, sigma, pi),
    "cond_2t_h1"    =   f_cond_2t_h1  (p, mu, sigma, alpha),
    "cond_2t_mix"   =   f_cond_2t_mix (p, mu, sigma, pi, alpha),
    "cond_1t_h1"    =   f_cond_1t_h1  (p, mu, sigma, alpha),
    "cond_1t_mix"   =   f_cond_1t_mix (p, mu, sigma, pi, alpha)
  )
  return(pdfs)
}

# Compute PDF of 2-sided p value under H1 for random-effects model.
f_uncond_2t_h1 <- function(p, mu, sigma) {
  z <- stats::qnorm(p/2)
  s2 <- sigma^2
  pref <- 1 / (2 * sqrt(1 + s2))
  base <- (s2 * z^2 - mu^2) / (2 * (1 + s2))
  t    <- (mu * z) / (1 + s2)
  m <- pmax(t, -t)  # log-sum-exp stabilization
  pref * exp(base + m) * (exp(t - m) + exp(-t - m))
}

# Compute PDF of 2-sided p value for mixture of H0 & H1 random-effects model.
f_uncond_2t_mix  <- function(p, mu, sigma, pi) (1 - pi) + pi * f_uncond_2t_h1(p, mu, sigma)

# Compute PDF of 1-sided p value under H1 for random-effects model.
f_uncond_1t_h1 <- function(p, mu, sigma) {
  z <- stats::qnorm(p)
  s2 <- sigma^2
  pref <- 1 / sqrt(1 + s2)
  base <- (s2 * z^2 - mu^2) / (2 * (1 + s2))
  t    <- (mu * z) / (1 + s2)
  m <- pmax(t, -t)  # log-sum-exp stabilization
  pref * exp(base + m) * exp(-t - m)
}

f_uncond_1t_mix <- function(p, mu, sigma, pi) (1 - pi) + pi * f_uncond_1t_h1(p, mu, sigma)

f_cond_2t_h1 <- function(p, mu, sigma, alpha) f_uncond_2t_h1(p, mu, sigma) / F_uncond_2t_h1(alpha, mu, sigma)

f_cond_2t_mix <- function(p, mu, sigma, pi, alpha) f_uncond_2t_mix(p, mu, sigma, pi) / F_uncond_2t_mix(alpha, mu, sigma, pi)

f_cond_1t_h1 <- function(p, mu, sigma, alpha) f_uncond_1t_h1(p, mu, sigma) / F_uncond_1t_h1(alpha, mu, sigma)

f_cond_1t_mix <- function(p, mu, sigma, pi, alpha) f_uncond_1t_mix(p, mu, sigma, pi) / F_uncond_1t_mix(alpha, mu, sigma, pi)
