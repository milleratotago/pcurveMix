# random_generators.R

#' Generate random p values from random-effects model.
#' @param n Number of random values to be generated.
#' @param mu Mean of noncentrality parameters when H1 is true.
#' @param sigma Standard deviation of noncentrality parameters when H1 is true.
#' @param pi Proportion of studies with H1 true in mixture of studies with H0 vs H1 true (default = 1).
#' @param tails 1 or 2 to indicate 1- or 2-tailed p values (default = 2).
#' @param alpha Maximum p value in distribution conditional on p<=alpha (default = 1).
#' @param cond_method Set to string "rejection" (default) or "inversion" to generate random
#'  p's from conditional distribution via rejection or quantile method. Rejection generates
#'  values 0-1 but only keeps those less than alpha. Inversion uses numerical search of the p
#'  distribution with the quantile function, which is usually slower except for really small alpha.
#' @param tol Real tolerance when generating p's via quantiles (default = 1e-8).
#' @returns Real vector of n random p values.
#' @export
random <- function(n, mu, sigma, pi = 1, alpha = 1, tails = 2, cond_method = c("rejection", "inversion"), tol = 1e-8 ) {
  this_case <- case_id(alpha, tails, pi)
  rands <- switch(
    this_case,
    "uncond_2t_h1"  = r_uncond_2t_h1  (n, mu, sigma),
    "uncond_2t_mix" = r_uncond_2t_mix (n, mu, sigma, pi),
    "uncond_1t_h1"  = r_uncond_1t_h1  (n, mu, sigma),
    "uncond_1t_mix" = r_uncond_1t_mix (n, mu, sigma, pi)
  )  # returns NULL if no match
  if (!is.null(rands)) {
    return(rands)
  }
  cond_method <- match.arg(cond_method)
  if (cond_method == "rejection") {
    rands <- switch(
      this_case,
      "cond_2t_h1"    = cond_sampler(r_uncond_2t_h1,  n, mu, sigma, alpha),          # r_cond_2t_h1  (n, mu, sigma, alpha),
      "cond_2t_mix"   = cond_sampler(r_uncond_2t_mix, n, mu, sigma, alpha, pi = pi), # r_cond_2t_mix (n, mu, sigma, pi, alpha),
      "cond_1t_h1"    = cond_sampler(r_uncond_1t_h1,  n, mu, sigma, alpha),          # r_cond_1t_h1  (n, mu, sigma, alpha),
      "cond_1t_mix"   = cond_sampler(r_uncond_1t_mix, n, mu, sigma, alpha, pi = pi)  # r_cond_1t_mix (n, mu, sigma, pi, alpha)
    ) # rejection
  } else { # cond_method == "inversion"
    # Generate runif's and then invert CDF back to p values with quantile function.
    # Slow due to CDF inversion by search.
    rr <- stats::runif(n)
    rands <- quantile(rr, mu, sigma, pi = pi, alpha = alpha, tails = tails, tol = tol)
  }
  return(rands)
}

r_uncond_2t_h1 <- function(n, mu, sigma) {
  s1 <- sqrt(1 + sigma^2)
  z  <- stats::rnorm(n, mean = mu, sd = s1)
  rands <- 2 * (1 - stats::pnorm(abs(z)))
  return(rands)
}

r_uncond_2t_mix <- function(n, mu, sigma, pi) {
  h1 <- stats::rbinom(n, 1, pi)
  s1 <- sqrt(1 + sigma^2)
  z  <- stats::rnorm(n,
              mean = ifelse(h1 == 1, mu, 0),
              sd   = ifelse(h1 == 1, s1, 1))
  rands <- 2 * (1 - stats::pnorm(abs(z)))
  return(rands)
}

r_uncond_1t_h1 <- function(n, mu, sigma) {
  s1 <- sqrt(1 + sigma^2)
  z  <- stats::rnorm(n, mean = mu, sd = s1)
  rands <- 1 - stats::pnorm(z)  # one-sided upper-tail p-value
  return(rands)
}

r_uncond_1t_mix <- function(n, mu, sigma, pi) {
  # mixture in Z-space, then transform to one-sided p = 1 - Phi(Z)
  h1 <- stats::rbinom(n, 1, pi)
  s1 <- sqrt(1 + sigma^2)
  z  <- stats::rnorm(
    n,
    mean = ifelse(h1 == 1, mu, 0),
    sd   = ifelse(h1 == 1, s1, 1)
  )
  rands <- 1 - stats::pnorm(z)  # one-sided upper-tail p-value
  return(rands)
}

# Simple rejection sampler to select p's less than alpha
cond_sampler <- function(f, n, mu, sigma, alpha, pi = NULL) {
  n_found <- 0
  rands <- numeric()
  while (n_found < n) {
    n_needed <- n - n_found
    n_to_generate <- 2 * n_needed / alpha  # Generate lots and hope we get enough < alpha
    if (is.null(pi)) {
      rand_batch <- f(n_to_generate, mu, sigma)
    } else {
      rand_batch <- f(n_to_generate, mu, sigma, pi = pi)
    }
    keepers <- rand_batch <= alpha
    rands <- c(rands, rand_batch[keepers])
    n_found <- length(rands)
  } # while
  return( rands[1:n] )
}

