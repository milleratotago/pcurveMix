# transforms.R
# functions to transform bounded parameters <-> unbounded reals

pis_to_reals <- function(pis) {
  return( stats::qlogis(pis) )
}

reals_to_pis <- function(reals) {
  return( stats::plogis(reals) )
}

mus_to_reals <- function(mus) {
  return( log(mus) )
}

reals_to_mus <- function(reals) {
  return( exp(reals) )
}

sigmas_to_reals <- function(sigmas) {
  return( log(sigmas) )
}

reals_to_sigmas <- function(reals) {
  return( exp(reals) )
}

reals_to_powers <- function(reals) {
  return( stats::plogis(reals) )
}

powers_to_reals <- function(powers) {
  return( stats::qlogis(powers) )
}

# Function to convert parameters on their natural scales (i.e., positive or 0-1)
#  into values across the full -inf/+inf real range for optim to adjust.
# param parms List with elements of mu>0, sigma>0, and 0<pi<1
parms_to_reals <- function(parms) {
  r <- parms
  r$pi <- pis_to_reals(parms$pi)
  r$mu <- mus_to_reals(parms$mu)
  r$sigma <- sigmas_to_reals(parms$sigma)
  return(r)
}

# Function to convert parameters in the full -inf/+inf real ranges
#  into values on their natural scales (i.e., positive or 0-1)
# param real List with real values of mu, sigma, and pi that are to be
#  converted back to the natural scale.
reals_to_parms <- function(reals) {
  p <- reals
  p$pi <- reals_to_pis(reals$pi)
  p$mu <- reals_to_mus(reals$mu)
  p$sigma <- reals_to_sigmas(reals$sigma)
  return(p)
}

#### NEWJEFF: Experimental custom_bounds_identity from here
# Use a Piecewise Linear Function with Exponential Tails to achieve a transformation
# that maps $(-\infty, +\infty)$ onto $(0, 1)$ while maintaining
# a strictly **linear region** across a central inner interval, e.g. $[0.05, 0.95]$

INNER_LOWER_P <- 0.02
INNER_UPPER_P <- 0.98

#' Identity Transformation with Separately Configurable Outer Asymptotic Limits
#'
#' @param x Numeric vector of inputs on (-Inf, +Inf)
#' @param inner_lower Start of linear identity region (default: 0.05)
#' @param inner_upper End of linear identity region (default: 0.95)
#' @param outer_lower Asymptotic lower limit as x -> -Inf (default: 0.0)
#' @param outer_upper Asymptotic upper limit as x -> +Inf (default: 1.0)
#' @return Numeric vector mapped onto (outer_lower, outer_upper)
custom_bounds_identity <- function(x,
                                   inner_lower = 0.05,
                                   inner_upper = 0.95,
                                   outer_lower = 0.0,
                                   outer_upper = 1.0) {
  # stopifnot(
  #   outer_lower < inner_lower,
  #   inner_lower < inner_upper,
  #   inner_upper < outer_upper
  # )

  # Distances to outer asymptotic limits
  d1 <- inner_lower - outer_lower
  d2 <- outer_upper - inner_upper

  ifelse(
    x < inner_lower,
    # Lower tail: approaches outer_lower asymptotically
    outer_lower + d1 * exp((x - inner_lower) / d1),
    ifelse(
      x <= inner_upper,
      # Linear core: identity function y = x
      x,
      # Upper tail: approaches outer_upper asymptotically
      outer_upper - d2 * exp(-(x - inner_upper) / d2)
    )
  )
}

#' Inverse Identity Transformation with Custom Outer Limits
#'
#' @param y Numeric vector strictly in (outer_lower, outer_upper)
#' @param inner_lower Start of linear identity region (default: 0.05)
#' @param inner_upper End of linear identity region (default: 0.95)
#' @param outer_lower Asymptotic lower limit (default: 0.0)
#' @param outer_upper Asymptotic upper limit (default: 1.0)
#' @return Numeric vector mapped back to (-Inf, +Inf)
inverse_custom_bounds_identity <- function(y,
                                           inner_lower = 0.05,
                                           inner_upper = 0.95,
                                           outer_lower = 0.0,
                                           outer_upper = 1.0) {
  # stopifnot(
  #   outer_lower < inner_lower,
  #   inner_lower < inner_upper,
  #   inner_upper < outer_upper,
  #   all(y > outer_lower & y < outer_upper)
  # )

  d1 <- inner_lower - outer_lower
  d2 <- outer_upper - inner_upper

  ifelse(
    y < inner_lower,
    # Inverse of lower tail
    inner_lower + d1 * log((y - outer_lower) / d1),
    ifelse(
      y <= inner_upper,
      # Inverse of linear identity core
      y,
      # Inverse of upper tail
      inner_upper - d2 * log((outer_upper - y) / d2)
    )
  )
}

reals_to_pis_2 <- function(reals) {
  return( custom_bounds_identity(reals,
                                 inner_lower = INNER_LOWER_P,
                                 inner_upper = INNER_UPPER_P,
                                 outer_lower = 0,
                                 outer_upper = 1)
          )
}

pis_to_reals_2 <- function(pis) {
  return( inverse_custom_bounds_identity(pis,
                                 inner_lower = INNER_LOWER_P,
                                 inner_upper = INNER_UPPER_P,
                                 outer_lower = 0,
                                 outer_upper = 1)
  )
}

##### END EXPERIMENTAL PART

