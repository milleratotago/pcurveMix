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
  vec <- rnorm(n, mean = mu, sd = sigma)
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
