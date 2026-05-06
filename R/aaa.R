# aaa.R

# Define an environment to hold settings that
# are global within the package
pcm_env <- new.env(parent = baseenv())

initialize_globals <- function() {
  pcm_env$edge_p <- 1e-12  # Literal also used in set_globals roxygen
  pcm_env$p_seq_pdf <- seq(0.001, 0.999, 0.002)  # p values for plotting predicted PDFs
  pwrs <- 4:12
  small_ps <- sort( 10^(-pwrs) )
  pcm_env$p_seq_cdf <- c(0, small_ps, seq(0.001, 0.999, 0.002)) # p values for plotting predicted CDFs
  pcm_env$optim_control <- NULL # Use optim defaults
  pcm_env$small_p_bin_cutoff <- NULL
  pcm_env$fit_constrained <- FALSE
  pcm_env$MLSEh <- 1e-7
  pcm_env$small_rcond <- 1e-15
}

#' Function to override defaults of a few global variables.
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
#' @param fit_constrained Boolean determining whether optim() is run using
#'  parameters constrained with limits (e.g., pi = 0-1) or is run with
#'  unconstrained -inf/+inf real parameters that are artificially transformed
#'  into the desired limits (default = FALSE, recommended)
#' @param MLSEh Small +/- increment to parameter values used in computing
#'  the Fisher information matrix (default = 1e-7)
#' @param small_rcond The cutoff reciprocal condition number for deciding that
#'  a Fisher information matrix is ill-conditioned (default = 1e-15)
#' @returns A list of the adjusted values of the global variables
#' @export
set_globals <- function(edge_p = NA, p_seq_pdf = NA, p_seq_cdf = NA, optim_control = NA,
                        small_p_bin_cutoff = NA, fit_constrained = NA, MLSEh = NA, small_rcond = NA) {
  if (!is.na(edge_p)) pcm_env$edge_p <- edge_p
  if (is.numeric(p_seq_pdf)) pcm_env$p_seq_pdf <- p_seq_pdf
  if (is.numeric(p_seq_cdf)) pcm_env$p_seq_cdf <- p_seq_cdf
  if (is.null(optim_control) || !is.na(optim_control)) pcm_env$optim_control <- optim_control
  if (is.null(small_p_bin_cutoff) || !is.na(small_p_bin_cutoff)) pcm_env$small_p_bin_cutoff <- small_p_bin_cutoff
  if (!is.na(fit_constrained)) pcm_env$fit_constrained <- fit_constrained
  if (!is.na(MLSEh)) pcm_env$MLSEh <- MLSEh
  if (!is.na(small_rcond)) pcm_env$small_rcond <- small_rcond
  l <- list(edge_p = pcm_env$edge_p, p_seq_pdf = pcm_env$p_seq_pdf,
            p_seq_cdf = pcm_env$p_seq_cdf, optim_control = pcm_env$optim_control,
            small_p_bin_cutoff = pcm_env$small_p_bin_cutoff,
            fit_constrained = pcm_env$fit_constrained,
            MLSEh = pcm_env$MLSEh, small_rcond = pcm_env$small_rcond)
  invisible(l)
}

# Next line suppresses package check warning about "density"
# that is used in quick_pdf_plot
utils::globalVariables(c("density"))

# Suppress messages about masked conflicts with either
# options(conflicts.policy = list(warn = FALSE))
#  or
# library(pcurveMix, warn.conflicts = FALSE)

.onAttach <- function(libname, pkgname) {
  s <- utils::packageVersion(pkgname)
  s <- paste("Package",pkgname,"version",s)
  packageStartupMessage(s)
  packageStartupMessage("Get help with these RStudio console commands:")
  packageStartupMessage(' ?',pkgname,'    # shows a summary of the package.')
  packageStartupMessage(' vignette("Intro", package = ',pkgname,')   # shows a basic introductory vignette illustrating the package and its shiny app.')
  packageStartupMessage(' browseVignettes(',pkgname,')    # shows a catalog of all vignettes.')
  packageStartupMessage(' help(package = "',pkgname,'")   # shows a manual of all functions exported from the package.')
  initialize_globals()
} # .onAttach

