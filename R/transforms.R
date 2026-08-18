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

