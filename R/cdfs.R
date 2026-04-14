# cdfs.R

#' Compute CDFs of 1- or 2-sided p values under H1 for random-effects model.
#' @inheritParams pdf
#' @returns Real CDF value or vector of CDF values.
#' @export
cdf <- function(p, mu, sigma, pi = 1, alpha = 1, tails = 2) {
  p2 <- pmin(pmax(as.numeric(p), pcm_env$edge_p), min(1 - pcm_env$edge_p, alpha) )
  cdfs <- switch(
    case_id(alpha, tails, pi),
    "uncond_2t_h1"  = F_uncond_2t_h1  (p2, mu, sigma),
    "uncond_2t_mix" = F_uncond_2t_mix (p2, mu, sigma, pi),
    "uncond_1t_h1"  = F_uncond_1t_h1  (p2, mu, sigma),
    "uncond_1t_mix" = F_uncond_1t_mix (p2, mu, sigma, pi),
    "cond_2t_h1"    =   F_cond_2t_h1  (p2, mu, sigma, alpha),
    "cond_2t_mix"   =   F_cond_2t_mix (p2, mu, sigma, pi, alpha),
    "cond_1t_h1"    =   F_cond_1t_h1  (p2, mu, sigma, alpha),
    "cond_1t_mix"   =   F_cond_1t_mix (p2, mu, sigma, pi, alpha)
  )
  too_small <- p < 0
  too_large <- p > alpha
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
  z <- stats::qnorm(1 - p)
  s <- sqrt(1 + sigma^2)
  stats::pnorm((mu - z) / s)
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



