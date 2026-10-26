# aaa.R

# Notes:
#  "parameter" is used as a literal in many places; dangerous to change.

# Constant strings used as labels.
# Note that these constants are NOT available to the shiny app
#  except via references like pcurveMix:::CI_LOWER_BOUND_LABEL
CI_LOWER_BOUND_LABEL <- "lower"
CI_UPPER_BOUND_LABEL <- "upper"
FOLDED_NORMAL_MU_LABEL <- "folded_normal_mu"
FOLDED_NORMAL_SIGMA_LABEL <- "folded_normal_sigma"
LIKELIHOOD_LABEL <- "log likelihood"
BIAS_CORRECTED_ORIGINAL_ESTIMATE_LABEL <- "bc_estimate"
PR_TP <- "Pr_TP"
PR_FN <- "Pr_FN"
PR_FP <- "Pr_FP"
PR_TN <- "Pr_TN"
R_FP <- "Rate_FPs"
R_FN <- "Rate_FNs"
N_BASE_PARMS <- 4 # mu, sigma, pi, power
N_DERIVED_PARMS <- 6 # PR_TP ... R_FN
DEMO_DATA_FILE_NAME <- "demo_data.csv"

MINIMUM_FOLDED_NORMAL_RATIO <- sqrt( 2 / (base::pi - 2))
START_MU_DEFAULT <- 2
START_SIGMA_DEFAULT <- 1
START_PI_DEFAULT <- 0.5
# OBSOLETE PARAMETER_ESTIMATES_NOTE <- paste("Wald_lower and Wald_upper", sep = "\n")

# Note: setting up progressr::handlers is not allowed here.

# Define an environment to hold settings that
# are global within the package
pcm_env <- new.env(parent = baseenv())

#' Initialize the variables in pcm_env to their default values.
#' @export
initialize_globals <- function() {
  pwrs <- 4:12
  small_ps <- sort( 10^(-pwrs) )
  # pcm_env$optim_starting_parms <- list(mu = START_MU_DEFAULT, sigma = START_SIGMA_DEFAULT, pi = START_PI_DEFAULT)
  # pcm_env$profCI_model <- structure(list(coefficients = c(mu = 0, sigma = 0, pi = 0)),
  #                          class = "profCI_model")
  pcm_env$bias_correct_ci_bounds <- TRUE
  pcm_env$confidence_level <- 95  # NOTE ALWAYS ON 0-100 scale
  pcm_env$edge_p <- 1e-20  # Literal also used in set_globals roxygen
  pcm_env$fast_boot_jack <- TRUE
  #  pcm_env$MLSEh <- 1e-7
  pcm_env$optim_control <- list(maxit = 2500, reltol = 1e-10)  # Use other optim defaults
  pcm_env$optim_starting_parms <- make_optim_starting_parms_df()
  pcm_env$p_seq_cdf <- c(0, small_ps, seq(0.001, 0.999, 0.002)) # p values for plotting predicted CDFs
  pcm_env$p_seq_pdf <- seq(0.001, 0.999, 0.002)  # p values for plotting predicted PDFs
  pcm_env$profileCI_args <- list(parm = "all", profile = TRUE, mult = 2, faster = FALSE, flat = 1e-08, lb = rep(-200,3), ub = rep(200,3) )
  pcm_env$round_to <- 3
  pcm_env$censor_at <- NA
  # pcm_env$small_rcond <- 1e-15
  # @param small_rcond The cutoff reciprocal condition number for deciding that
  #  a Fisher information matrix is ill-conditioned (default = 1e-15)

  # Check if shiny is installed (returns TRUE or FALSE)
  pcm_env$shiny_is_installed <- requireNamespace("shiny", quietly = TRUE)
  pcm_env$shiny_running <- FALSE
}

# Previously included confidence_level

#' Function to override defaults of some global variables.
#' @inheritParams pcurveMix-package
#' @param reset_to_defaults Boolean; if true, reset all values to their
#'  defaults before applying the other arguments
#' @returns A list of the values of the global variables, after changing any
#'  of the values as indicated.
#' @export
#' @examples
#' set_globals(confidence_level = 99, fast_boot_jack = FALSE)
set_globals <- function(
    bias_correct_ci_bounds = NA,
    confidence_level = NA,
    edge_p = NA,
    fast_boot_jack = NA,
    # MLSEh = NA,
    # @param MLSEh Small +/- increment to parameter values used in computing
    #  the Fisher information matrix (default = 1e-7)
    optim_control = NA,
    optim_starting_parms = NA,
    p_seq_cdf = NA,
    p_seq_pdf = NA,
    profileCI_args = NA,
    round_to = NA,
    censor_at = NA,
    # small_rcond = NA,

    reset_to_defaults = FALSE
) {
  if (reset_to_defaults) initialize_globals()
  if (!is.na(confidence_level)) pcm_env$confidence_level <- confidence_level
  if (!is.na(round_to)) pcm_env$round_to <- round_to
  if (!is.na(bias_correct_ci_bounds)) pcm_env$bias_correct_ci_bounds <- bias_correct_ci_bounds
  if (!is.na(edge_p)) pcm_env$edge_p <- edge_p
  if (is.numeric(p_seq_pdf)) pcm_env$p_seq_pdf <- p_seq_pdf
  if (is.numeric(p_seq_cdf)) pcm_env$p_seq_cdf <- p_seq_cdf
  if (!identical(optim_control,NA)) pcm_env$optim_control <- optim_control
  if (!is.na(censor_at)) pcm_env$censor_at <- censor_at
  # if (!is.na(MLSEh)) pcm_env$MLSEh <- MLSEh
  # if (!is.na(small_rcond)) pcm_env$small_rcond <- small_rcond
  if (any(!is.na(optim_starting_parms))) pcm_env$optim_starting_parms <- optim_starting_parms
  if (any(!is.na(profileCI_args))) pcm_env$profileCI_args <- profileCI_args
  if (!is.na(fast_boot_jack)) pcm_env$fast_boot_jack <- fast_boot_jack
  l <- list(
    bias_correct_ci_bounds = pcm_env$bias_correct_ci_bounds,
    confidence_level = pcm_env$confidence_level,
    edge_p = pcm_env$edge_p,
    fast_boot_jack = pcm_env$fast_boot_jack,
    # MLSEh = pcm_env$MLSEh,
    optim_control = pcm_env$optim_control,
    optim_starting_parms = pcm_env$optim_starting_parms,
    p_seq_cdf = pcm_env$p_seq_cdf,
    p_seq_pdf = pcm_env$p_seq_pdf,
    profileCI_args = pcm_env$profileCI_args,
    round_to = pcm_env$round_to,
    censor_at = pcm_env$censor_at
    # small_rcond = pcm_env$small_rcond
  )
  invisible(l)
}

#' Function to construct a grid of parameter values to use as starting points
#'  for fitting the model using fit_p_curve. The grid df has rows
#'  for all possible combinations of the values in the vectors mu, sigma, and pi.
#' @param mu Vector of different starting values of the mu parameter
#'  (default = 0.5, 1.0, 2.0)
#' @param sigma Vector of different starting values of the sigma parameter
#'  (default = 1, 2, 4)
#' @param pi Vector of different starting values of the pi parameter.
#'  (default = 0.2, 0.5, 0.8). If this is set to NA, then a single
#'  starting value will be computed based on the proportion of significant
#'  results in the vector of p's that is to be fit.
#' @export
make_optim_starting_parms_df <- function(mu = c(0.25, 1.0, 2.0),
                                         sigma = c(1, 2, 4),
                                         pi = c(0.2, 0.5, 0.8) ) {
  start_df <- expand.grid(mu = mu, sigma = sigma, pi = pi)
  return(start_df)
}

#' Return the value of a single environment variable or a
#'  list with the values of multiple variables.
#' @param variable_names A string or vector of strings indicating the variables
#'  whose values are to be returned
#' @returns The value or list of values for the named variable(s)
#' @export
#' @examples
#' # current_confidence_level <- get_globals("confidence_level")
get_globals <- function(variable_names = c() ) {
  n_vars <- length(variable_names)
  if (n_vars == 0) return( set_globals() )
  if (n_vars == 1) { return(pcm_env[[variable_names]]) }
  l <- mget(variable_names, envir = pcm_env)
  return(l)
}

# Next line suppresses package check warning about "density"
# that is used in quick_pdf_plot
utils::globalVariables(c("density"))

# Suppress messages about masked conflicts with either
# options(conflicts.policy = list(warn = FALSE))
#  or
# library(pcurveMix, warn.conflicts = FALSE)

.onAttach <- function(libname, pkgname) {
  initialize_globals()
  if (!interactive()) return()
  s <- utils::packageVersion(pkgname)
  s <- paste("Package",pkgname,"version",s)
  packageStartupMessage(s) # NWJEFF combine strings into one call and export it in a separate function available to users with just a short note here to call that function for help
  packageStartupMessage('Get help with these RStudio console commands:')
  packageStartupMessage('  ?',pkgname,'    # shows a summary of the package.')
  packageStartupMessage('  vignette("Intro", package = ',pkgname,') # basic intro to package & shiny app.')
  packageStartupMessage('  browseVignettes("',pkgname,'")    # shows a catalog of all vignettes.')
  packageStartupMessage('  help(package = "',pkgname,'")   # shows a manual of all functions exported from the package.')
  packageStartupMessage('  run_shiny_app()  # starts the shiny app')

  # Check progressr global state (returns TRUE, FALSE, or NA if never set)
  is_global_active <- progressr::handlers(global = NA)
  # If it is turned off or unconfigured, print a polite tip
  # \u2139\ufe0f produces a non-ascii information source emoji,
  # use "\u2139" for the standard information icon
  # This code breaks knitting to pdf, however.
  if (is.na(is_global_active) || !is_global_active) {
    packageStartupMessage(
      # "\u2139 [", pkgname, "] This package supports real-time progress bars!\n",
      '  To enable real-time progress bars, run: progressr::handlers(global = TRUE)'  )
  }
} # .onAttach

