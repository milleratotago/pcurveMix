# folded_normal_R
# from https://en.wikipedia.org/wiki/Folded_normal_distribution
# X = normal(mu, sigma)
# Y = folded normal(mu, sigma) = abs(X)

dfolded_normal <- function(x, mu, sigma) {
  two_sig_sqr <- 2 * sigma^2
  val <- 1 / (sigma*sqrt(2*pi) ) * ( exp( -(x-mu)^2 / two_sig_sqr ) + exp( -(x+mu)^2 / two_sig_sqr ) )
  return(val)
}

pfolded_normal <- function(x, mu, sigma) {
  sig_sqrt_2 <- sigma * sqrt(2)
  val <- 0.5 * ( pracma::erf( (x+mu)/sig_sqrt_2 ) + pracma::erf( (x-mu)/sig_sqrt_2 ) )
  return(val)
}

rfolded_normal <- function(n, mu, sigma) {
  vec <- stats::rnorm(n, mean = mu, sd = sigma)
  return( abs(vec) )
}

mean_folded_normal <- function(mu, sigma) {
  two_sig_sqr <- 2 * sigma^2
  val <- sigma * sqrt(2/pi) * exp( -mu^2/(two_sig_sqr) ) + mu * pracma::erf( mu/sqrt(two_sig_sqr) )
  return(val)
}

variance_folded_normal <- function(mu, sigma) {
  val <- mu^2 + sigma^2 - mean_folded_normal(mu, sigma)^2
  return(val)
}

sd_folded_normal <- function(mu, sigma) {
  val <- sqrt( variance_folded_normal(mu, sigma) )
  return(val)
}

# Compute folded mu & sigma from unfolded mu & sigma.
folded_moments <- function(mu, sigma) {
  c(mean = mean_folded_normal(mu, sigma), sd = sd_folded_normal(mu, sigma) )
}

# Helper function to find the root for c needed by unfolded_moments()
f_ratio <- function(c, target_ratio) {
  mean_scaled <- sqrt(2 / base::pi) * exp(-c^2 / 2) + c * (1 - 2 * stats::pnorm(-c))
  var_scaled <- (c^2 + 1) - mean_scaled^2
  sd_scaled <- sqrt(var_scaled)
  return((mean_scaled / sd_scaled) - target_ratio)
}

# Compute unfolded mu & sigma from folded mu & sigma.
unfolded_moments <- function(folded_mu, folded_sigma, tol = 1e-10) {  # NEWJEFF: Magic constant.
  target_ratio <- folded_mu / folded_sigma
  # Use helper fn to solve for c_val in a reasonable interval (e.g., 0 to 20)  # NEWJEFF: Magic constants.
  root_res <- stats::uniroot(f_ratio, lower = 0, upper = 20, tol = tol,
                             target_ratio = target_ratio)
  c_val <- root_res$root

  # Recover sigma and mu from c_val analytically
  mean_scaled_val <- sqrt(2 / base::pi) * exp(-c_val^2 / 2) +
    c_val * (1 - 2 * stats::pnorm(-c_val))
  sigma <- folded_mu / mean_scaled_val
  mu <- c_val * sigma
  return( c(mean = mu, sd = sigma) )
}

