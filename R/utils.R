# utils.R

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

#' Check a vector of p values to see whether they are
#'   all >0 and <=alpha_cutoff as expected.
#' @param ps Vector of p values
#' @param alpha_cutoff Maximum p value allowed in file
#' @returns A list all_in_bounds, n_too_small, etc
# DO NOT export
check_ps <- function(ps, alpha_cutoff) {
  too_small <- ps < 0
  equal_zero <- ps == 0
  too_large <- ps > alpha_cutoff
  n_too_small <- sum(too_small)
  n_equal_zero <- sum(equal_zero)
  n_too_large <- sum(too_large)
  if (n_too_small + n_equal_zero + n_too_large == 0) {
    l <- list(all_in_bounds = TRUE,
              alpha_cutoff = alpha_cutoff)
  } else {
    ps[equal_zero] <- 1e-12
    l <- list(all_in_bounds = FALSE,
              alpha_cutoff = alpha_cutoff,
              n_too_small = n_too_small,
              n_equal_zero = n_equal_zero,
              n_too_large = n_too_large,
              ps_too_small = ps[too_small],
              ps_too_large = ps[too_large],
              ps_in_bounds = ps[ !(too_small | too_large) ]
              )
  }
  return(l)
}

#' Construct a string describing the problems found by check_ps
#' @param l List produced by check_ps
#' @returns String
# DO NOT export
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
