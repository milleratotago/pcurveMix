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
START_MU_DEFAULT <- 2
START_SIGMA_DEFAULT <- 1
START_PI_DEFAULT <- 0.5
# OBSOLETE PARAMETER_ESTIMATES_NOTE <- paste("Wald_lower and Wald_upper", sep = "\n")  NEWJEFF: Did I handle confidence_level?
# Be sure to "cat" rather than "print" this to console
BOOTSTRAP_TABLE_NOTE <- paste("TABLE NOTES:",
                              "- mean & sd are summaries of parameter estimates across bootstrap samples.",
                              paste("- bias &",BIAS_CORRECTED_ORIGINAL_ESTIMATE_LABEL,"are estimated bias & bias-corrected parameter value."),
                              paste0("- hw, ", CI_LOWER_BOUND_LABEL, ", & ", CI_UPPER_BOUND_LABEL,
                                     " are half-width and bounds of t confidence interval for ",BIAS_CORRECTED_ORIGINAL_ESTIMATE_LABEL,"."),
                              "- bias-corrected quantiles of bootstrap parameter estimates.",
                              sep = "\n")

# This is not allowed here.
# # Set up default console handler state for interactive work
# progressr::handlers(global = TRUE)
# progressr::handlers("txtprogressbar") # Forces standard text console output

# Define an environment to hold settings that
# are global within the package
pcm_env <- new.env(parent = baseenv())

initialize_globals <- function() {
  pcm_env$shiny_running <- FALSE
  pcm_env$edge_p <- 1e-12  # Literal also used in set_globals roxygen
  pcm_env$p_seq_pdf <- seq(0.001, 0.999, 0.002)  # p values for plotting predicted PDFs
  pwrs <- 4:12
  small_ps <- sort( 10^(-pwrs) )
  pcm_env$p_seq_cdf <- c(0, small_ps, seq(0.001, 0.999, 0.002)) # p values for plotting predicted CDFs
  pcm_env$optim_control <- list(maxit = 1000)  # Use other optim defaults
  pcm_env$small_p_bin_cutoff <- NULL
  pcm_env$MLSEh <- 1e-7
  pcm_env$small_rcond <- 1e-15
  # pcm_env$optim_starting_parms <- list(mu = START_MU_DEFAULT, sigma = START_SIGMA_DEFAULT, pi = START_PI_DEFAULT)
  pcm_env$optim_starting_parms <- make_optim_starting_parms_df()
  # pcm_env$profCI_model <- structure(list(coefficients = c(mu = 0, sigma = 0, pi = 0)),
  #                          class = "profCI_model")
  pcm_env$profileCI_args <- list(parm = "all", profile = TRUE, mult = 2, faster = FALSE, flat = 1e-08,
                                 lb = rep(-200,3), ub = rep(200,3) )
  pcm_env$fast_boot_jack <- TRUE
  pcm_env$confidence_level <- 95
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

#' Function to override defaults of some global variables.
#' @param confidence_level Used in computing confidence intervals (default = 95)
#' @param edge_p To avoid numerical errors, change p==0 to edge_p and
#'  change p==1 to 1-edge_p (default = 1e-12)
#' @param p_seq_pdf Sequence of p values at which to compute predicted pdf
#'  values for plots (default = seq(0.001, 0.999, 0.002))
#' @param p_seq_cdf Sequence of p values at which to compute predicted cdf
#'  values for plots (default same as pdf with added 10^(4:10))
#' @param optim_control A control list passed to R's optim() function
#'  (default = NULL, in which case the optim defaults are used)
#' @param small_p_bin_cutoff The cutoff point for computing likelihoods with
#'  censoring (default = NULL, in which case likelihoods are computed without
#'  censoring)
#' @param MLSEh Small +/- increment to parameter values used in computing
#'  the Fisher information matrix (default = 1e-7)
#' @param small_rcond The cutoff reciprocal condition number for deciding that
#'  a Fisher information matrix is ill-conditioned (default = 1e-15)
#' @param optim_starting_parms A list or data frame of parameter combinations
#'  at which to start the optim searches (default: list(mu = 2, sigma = 2, pi = 0.5))
#' @param profileCI_args A list of optional arguments to be passed to profileCI.
#' @param fast_boot_jack Boolean with default TRUE indicating that bootstrapping
#'  and jackknifing should always run optim() starting from the maximum likelihood
#'  parameter values, which is faster than running it from the starting value
#'  grid given by optim_starting_parms,
#' @param reset_to_defaults Boolean; if true, reset all values to their
#'  defaults before applying the other arguments
#' @returns A list of the values of the global variables, after changing any
#'  of the values as indicated.
#' @export
#' @examples
#' set_globals(confidence_level = 99, fast_boot_jack = FALSE)
set_globals <- function(confidence_level = NA,
                        edge_p = NA, p_seq_pdf = NA, p_seq_cdf = NA, optim_control = NA,
                        small_p_bin_cutoff = NA,
                        MLSEh = NA, small_rcond = NA,
                        optim_starting_parms = NA, profileCI_args = NA,
                        fast_boot_jack = NA,
                        reset_to_defaults = FALSE) {
  if (reset_to_defaults) initialize_globals()
  if (!is.na(confidence_level)) pcm_env$confidence_level <- confidence_level
  if (!is.na(edge_p)) pcm_env$edge_p <- edge_p
  if (is.numeric(p_seq_pdf)) pcm_env$p_seq_pdf <- p_seq_pdf
  if (is.numeric(p_seq_cdf)) pcm_env$p_seq_cdf <- p_seq_cdf
  if (is.null(optim_control) || !is.na(optim_control)) pcm_env$optim_control <- optim_control
  if (is.null(small_p_bin_cutoff) || !is.na(small_p_bin_cutoff)) pcm_env$small_p_bin_cutoff <- small_p_bin_cutoff
  if (!is.na(MLSEh)) pcm_env$MLSEh <- MLSEh
  if (!is.na(small_rcond)) pcm_env$small_rcond <- small_rcond
  if (any(!is.na(optim_starting_parms))) pcm_env$optim_starting_parms <- optim_starting_parms
  if (any(!is.na(profileCI_args))) pcm_env$profileCI_args <- profileCI_args
  if (!is.na(fast_boot_jack)) pcm_env$fast_boot_jack <- fast_boot_jack
  l <- list(confidence_level = pcm_env$confidence_level,
            edge_p = pcm_env$edge_p, p_seq_pdf = pcm_env$p_seq_pdf,
            p_seq_cdf = pcm_env$p_seq_cdf, optim_control = pcm_env$optim_control,
            small_p_bin_cutoff = pcm_env$small_p_bin_cutoff,
            MLSEh = pcm_env$MLSEh, small_rcond = pcm_env$small_rcond,
            optim_starting_parms = pcm_env$optim_starting_parms,
            profileCI_args = pcm_env$profileCI_args,
            fast_boot_jack = pcm_env$fast_boot_jack)
  invisible(l)
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
  l <- pcm_env[variable_names]
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
  packageStartupMessage(s) # NEWJEFF combine strings into one call and export it in a separate function available to users with just a short note here to call that function for help
  packageStartupMessage('Get help with these RStudio console commands:')
  packageStartupMessage('  ?',pkgname,'    # shows a summary of the package.')
  # NEWJEFF packageStartupMessage('  vignette("Intro", package = ',pkgname,')   # shows a basic introductory vignette illustrating the package and its shiny app.')
  packageStartupMessage('  browseVignettes(',pkgname,')    # shows a catalog of all vignettes.')
  packageStartupMessage('  help(package = "',pkgname,'")   # shows a manual of all functions exported from the package.')
  packageStartupMessage('  run_shiny_app()  # starts the shiny app')

  # Check progressr global state (returns TRUE, FALSE, or NA if never set)
  is_global_active <- progressr::handlers(global = NA)
  # If it is turned off or unconfigured, print a polite tip
  # \u2139\ufe0f produces a non-ascii information source emoji,
  # use "\u2139" for the standard information icon
  if (is.na(is_global_active) || !is_global_active) {
    packageStartupMessage(
      "\u2139 [", pkgname, "] This package supports real-time progress bars!\n",
      "   To enable them, run: progressr::handlers(global = TRUE)"  )
  }
} # .onAttach

