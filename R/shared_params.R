# shared_params.R
# This file defines parameter specifications for parameters used across multiple functions.
# Functions that use these parameters should include inheritParams pcurveMix-package
# THOSE FUNCTIONS MUST NOT INCLUDE ANY OTHER inheritParams specs selecting specific parameters
#
# Note that recursive inheritance is OK, so an R package function using roxygen2 can inherit
# parameters from another function, which in turn inherited them from a third function.

#' pcurveMix: Shared Parameter Documentation
#'
#' @param alpha Maximum p value in distribution conditional on p<=alpha (default = 1)
#' @param alpha_sig Significance level p cutoff used in computing the estimated
#'  average power when H0 is false (default = 0.05).
#' @param bias_correct_ci_bounds Boolean; if true (default), adjust confidence
#'  interval bounds to correct for estimated bias.
#' @param censor_at The cutoff point for computing likelihoods with
#'  censoring (default = NA, in which case likelihoods are computed without
#'  censoring).
#' @param confidence_level A numeric value 0-100 used in computing confidence
#'  intervals. The default value is taken from the Shiny app interface if that
#'  app is being used; otherwise, it is taken from pcm_env$confidence_level,
#'  which has the default value of 95.
#' @param edge_p To avoid numerical errors, change p==0 to edge_p and
#'  change p==1 to 1-edge_p (default = 1e-20)
#' @param estimates Data frame of parameter estimates for many datasets
#'  (e.g., bootstrap samples), with one row per dataset and columns for parameters.
#' @param fast_boot_jack Boolean with default TRUE indicating that bootstrapping
#'  and jackknifing should always run optim() starting from the maximum likelihood
#'  parameter values, which is faster than running it from the default grid of
#'  starting values given by pcm_env$optim_starting_parms.
#' @param fit_results_list List returned by fit_p_curve() with results of model fitting.
#' @param mu Mean of noncentrality parameters when H1 is true
#' @param optim_control A control list passed to R's optim() function
#'  (default = NULL, in which case the optim defaults are used).
#' @param optim_starting_parms A list or data frame of parameter combinations
#'  at which to start the optim searches (default: list(mu = 2, sigma = 2, pi = 0.5)).
#' @param p_seq_cdf Sequence of p values at which to compute predicted cdf
#'  values for plots (default same as pdf with added 10^(4:10)).
#' @param p_seq_pdf Sequence of p values at which to compute predicted pdf
#'  values for plots (default = seq(0.001, 0.999, 0.002)).
#' @param pi Proportion of studies with H1 true in mixture of studies with H0 vs H1 true (default = 1).
#' @param ps Real single p value or vector of p's in the range (0--alpha].
#' @param profileCI_args A list of optional arguments to be passed to profileCI.
#' @param round_to Integer number of decimal places to display (default = 3, use
#'  NULL for no rounding).
#' @param sigma Standard deviation of noncentrality parameters when H1 is true
#' @param tails 1 or 2 to indicate 1- or 2-tailed p values (default = 2)
#'
#' @docType _PACKAGE
#' @keywords internal
"_PACKAGE"

# start_parms
# SINGULAR @param cond_method Set to string "rejection" (default) or "inversion" to generate random
# SINGULAR @param converged Boolean indicating whether the optim estimation process converged
# SINGULAR @param ests_orig A list with numerical values of the full-sample estimates
# SINGULAR @param file_name Optional string used to include p file name in table
# SINGULAR @param file_name_prefix String prepended to output file names (default = "").
# SINGULAR @param full_sample_n Number of observations in the original dataset from
# SINGULAR @param generate_time_stamp
# SINGULAR @param lhs A value or the magrittr placeholder.
# SINGULAR @param mat_of_ps Matrix of p values with n_samples rows and n_per_sample columns
# SINGULAR @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl() with
# @param mu Vector of different starting values of the mu parameter
# @param n Number of random p values to be generated
# @param n_boot_samples Number of bootstrap samples to take
# @param n_bootstrap_nonparametric N of samples for nonparametric bootstrap;
# @param n_bootstrap_parametric N of samples for parametric bootstrap;
# @param n_per_subsample Number of values per bootstrap subsample
# @param n_progress_bar_steps Number of times to update progress bar within
# @param n_samples Number of bootstrap samples
# @param n_samples Number of jackknife subsamples
# @param n_subsamples Number of bootstrap subsamples to generate
# @param output_dir Directory where the final output file should go
# @param output_dir Directory where the output files should go
# @param output_file_name Name (with path) for the report file.
# @param output_format String either "html", "pdf", or "docx" (default "html").
# @param p Real 0-1 p value or vector for which nll is to be computed. p values
# @param p Real 0-1 single p value or vector of p's for which function values are to be computed
# @param p Vector of to-be-fitted p values in 0--1
# @param params Output list produced by auto_shiny function.
# @param params Output list produced by auto_shiny function.
# @param parms_to_summarize String vector with names of parameters to estimate
# @param pct_converged Percentage of bootstrap samples for which estimation converged
# @param pct_converged Percentage of jackknife subsamples for which estimation converged
# @param profileCI_folded_normal_mu Profile result list for mu
# @param profileCI_folded_normal_sigma Profile result list for sigma
# @param profileCI_power Profile result list for power
# @param profileCI_std Profile result list for mu, sigma, and pi
# @param ps Vector of p values
# @param ps Vector of ps to be fit
# @param ps_label A string label to identify this set of p's in the
# @param quantiles Vector of 0-1 quantile values to be returned
# @param real_ps p-values in original to-be-bootstrapped sample
# @param reset_to_defaults Boolean; if true, reset all values to their
# @param rhs A function call using the magrittr semantics.
# @param sample_size Number of p's in each bootstrap sample
# @param show_error Show error message if any p value is out of
# @param show_progress_bar Boolean determining whether progress bar is used (default = TRUE)
# @param sigma Vector of different starting values of the sigma parameter
# @param target_cdfs The CDFs of the p-curve distribution at which to determine the quantile p value
# @param SINGULAR target_folded_normal_mu Boolean where TRUE requests CI for folded normal mu
# @param tol Real tolerance when generating p's via quantiles (default = 1e-8)
# @param tol Tolerance value for uniroot search function (default = 1e-8)
# @param type Integer 1-7 indicating the quantile type
# @param variable_names A string or vector of strings indicating the variables
# @param want_converged Boolean with default TRUE indicating that the boolean
# @param want_jackknifing Boolean to indicate that jackknifing should be done
# @param want_names Boolean indicating whether vector elements should be named,
# @param want_optim_hessian Boolean indicating whether optim() should compute
# @param want_profile_cis Boolean to indicate that profile confidence intervals
# @param wide_table_output Produce wide table by default TRUE, otherwise long
# @param x Numeric vector of inputs on (-Inf, +Inf)
# @param y Numeric vector strictly in (outer_lower, outer_upper)
