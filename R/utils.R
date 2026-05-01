# utils.R

#' Function to start the shiny app for model-fitting from
#' the RStudio console.
#' @export
run_shiny_app <- function() {
  appDir <- system.file("shiny", package = "pcurveMix")
  shiny::runApp(appDir, display.mode = "normal")
}

# Function to compute a case identifier for use in switch statements.
# Possible identifiers are strings:
#  uncond_2t_h1
case_id <- function(alpha = 1, tails = 2, pi = 1) {

  if (alpha == 1) {
    s <- "uncond"
  } else if (alpha < 1 && alpha > 0) {
    s <- "cond"
  } else {
    stop("alpha must be in the range 0--1")
  }

  if (tails == 2) {
    s <- paste0(s,"_2t")
  } else if (tails == 1) {
    s <- paste0(s,"_1t")
  } else {
    stop("tails must be 1 or 2")
  }

  if (pi == 1) {
    s <- paste0(s,"_h1")
  } else if (pi < 1 && pi > 0) {
    s <- paste0(s,"_mix")
  } else {
    stop("pi must be in the range 0--1")
  }

    return(s)
}

# Check a vector of p values to see whether they are
#   all >0 and <=alpha_cutoff as expected.
# @param ps Vector of p values
# @param alpha_cutoff Maximum p value allowed in file
# @returns A list all_in_bounds, n_too_small, etc
# DO NOT export
check_ps <- function(ps, alpha_cutoff) {
  too_small <- ps < 0
  equal_zero <- ps == 0
  too_large <- (ps > alpha_cutoff) | (ps == 1)
  n_too_small <- sum(too_small)
  n_equal_zero <- sum(equal_zero)
  n_too_large <- sum(too_large)
  all_in_bounds <- (n_too_small + n_equal_zero + n_too_large == 0)
  if (n_equal_zero > 0) ps[equal_zero] <- pcm_env$edge_p
  l <- list(all_in_bounds = all_in_bounds,
            alpha_cutoff = alpha_cutoff,
            n_too_small = n_too_small,
            n_equal_zero = n_equal_zero,
            n_too_large = n_too_large,
            ps_too_small = ps[too_small],
            ps_too_large = ps[too_large],
            ps_in_bounds = ps[ !(too_small | too_large) ]
  )
  return(l)
}

# Construct a string describing the problems found by check_ps
# @param l List produced by check_ps
# @returns String
# DO NOT exp  ort
bad_ps_report_string <- function(l) {
  s <- "Check p's; found and altered or eliminated"
  if (l$n_too_small > 0) s <- paste(s,l$n_too_small,"p's < 0")
  if ( (l$n_too_small > 0) && (l$n_equal_zero > 0) ) s <- paste(s,"and")
  if (l$n_equal_zero > 0) s <- paste(s,l$n_equal_zero,"p's == 0")
  if ( (l$n_too_small + l$n_equal_zero > 0) && (l$n_too_large > 0) ) s <- paste(s,"and")
  if (l$n_too_large > 0) s <- paste(s,l$n_too_large,"p's >", l$alpha_cutoff,"cutoff")
  if ( (l$n_too_small == 0) && (l$n_equal_zero == 0) && (l$n_too_large == 0) ) s <- paste(s,"no problematic p's")
  return(s)
}

#' Convert a fit_list (output of fit_p_curve) to a data frame with a single row
#'  for convenient accumulation of multiple fit results via rbind
#' @param fit_list A list that was the output of fit_p_curve
#' @returns A data frame with a single row
#' @export
fit_list_to_df <- function(fit_list) {
  fit_list$start <- NULL
  fit_list$check_ps_list <- NULL
  fit_list$pi_se <- fit_list$se["pi"]
  fit_list$mu_se <- fit_list$se["mu"]
  fit_list$sigma_se <- fit_list$se["sigma"]
  fit_list$se <- NULL
  fit_list$pi_lwr95 <- fit_list$ci95["pi","lwr95"]
  fit_list$pi_upr95 <- fit_list$ci95["pi","upr95"]
  fit_list$mu_lwr95 <- fit_list$ci95["mu","lwr95"]
  fit_list$mu_upr95 <- fit_list$ci95["mu","upr95"]
  fit_list$sigma_lwr95 <- fit_list$ci95["sigma","lwr95"]
  fit_list$sigma_upr95 <- fit_list$ci95["sigma","upr95"]
  fit_list$ci95 <- NULL
  fit_list$ks_Dmax <- fit_list$ks$statistic
  fit_list$ks_p_value <- fit_list$ks$p.value
  fit_list$ks_exact <- fit_list$ks$exact
  fit_list$ks <- NULL
  df <- as.data.frame(fit_list)
  return(df)
}

# Function to convert parameters on their natural scales
#  into values across the full real range for optim to adjust.
# param parms List with elements of mu>0, sigma>0, and 0<pi<1
parms_to_reals <- function(parms) {
  r <- parms
  r$pi <- stats::qnorm(parms$pi)
  r$mu <- log(parms$mu)
  r$sigma <- log(parms$sigma)
  return(r)
}

# Function to convert parameters on their natural scales
#  into values across the full real range for optim to adjust.
# param parms List with elements of mu, sigma, and pi
reals_to_parms <- function(reals) {
  p <- reals
  p$pi <- stats::pnorm(reals$pi)
  p$mu <- exp(reals$mu)
  p$sigma <- exp(reals$sigma)
  return(p)
}
