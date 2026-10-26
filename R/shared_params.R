# shared_params.R
# This file defines parameter specifications for parameters used across multiple functions.
# Functions that use these parameters should include inheritParams pcurveMix-package
# THOSE FUNCTIONS MUST NOT INCLUDE ANY OTHER inheritParams specs selecting specific parameters

#' pcurveMix: Shared Parameter Documentation
#'
#' @param alpha_sig Significance level p cutoff used in computing the estimated
#'  average power when H0 is false (default = 0.05).
#' @param bias_correct_ci_bounds Boolean; if true (default), adjust confidence
#'  interval bounds to correct for estimated bias.
#' @param censor_at The cutoff point for computing likelihoods with
#'  censoring (default = NA, in which case likelihoods are computed without
#'  censoring).
#' @param confidence_level A numeric value 0-100 used in computing confidence
#'  intervals (default = 95).
#' @param fast_boot_jack Boolean with default TRUE indicating that bootstrapping
#'  and jackknifing should always run optim() starting from the maximum likelihood
#'  parameter values, which is faster than running it from the default grid of
#'  starting values given by pcm_env$optim_starting_parms.
#'
#' @docType _PACKAGE
#' @keywords internal
"_PACKAGE"


# @param alpha Maximum p value in distribution conditional on p<=alpha (default = 1)
# @param alpha Maximum p value in distribution conditional on p<=alpha (default = 1)
# @param alpha_cutoff Maximum p value allowed in file
# @param alpha_sig Significance cutoff used in computing the estimated average
# @param cond_method Set to string "rejection" (default) or "inversion" to generate random
# @param converged Boolean indicating whether the optim estimation process converged
# @param edge_p To avoid numerical errors, change p==0 to edge_p and
# @param estimates Data frame of parameter estimates for many datasets
# @param estimates Data frame of parameter estimates for many datasets
# @param ests_orig A list with numerical values of the full-sample estimates
# @param ests_tbl Data frame of parameter estimates, one row per bootstrap sample
# @param file_name Optional string used to include p file name in table
# @param file_name_prefix String prepended to output file names (default = "").
# @param fit List output of fit_p_curve function
# @param fit Output list from fit_p_curve
# @param fit Output list from fit_p_curve
# @param fit_list A list that was the output of fit_p_curve
# @param fit_list Fitted model list returned by fit_p_curve
# @param full_sample_n Number of observations in the original dataset from
# @param level Confidence level for the CI (0-1, default = pcm_env$confidence_level/100) # NEWJEFF: Or Shiny
# @param lhs A value or the magrittr placeholder.
# @param mat_of_ps Matrix of p values with n_samples rows and n_per_sample columns
# @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl() with
# @param mu Mean of noncentrality parameters when H1 is true
# @param mu Mean of noncentrality parameters when H1 is true
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
# @param optim_control A control list passed to R's optim() function
# @param optim_starting_parms A list or data frame of parameter combinations
# @param output_dir Directory where the final output file should go
# @param output_dir Directory where the output files should go
# @param output_file_name Name (with path) for the report file.
# @param output_format String either "html", "pdf", or "docx" (default "html").
# @param p Real 0-1 p value or vector for which nll is to be computed. p values
# @param p Real 0-1 single p value or vector of p's for which function values are to be computed
# @param p Vector of to-be-fitted p values in 0--1
# @param p_seq Sequence of p values at which to compute predictions
# @param p_seq_cdf Sequence of p values at which to compute predicted cdf
# @param p_seq_pdf Sequence of p values at which to compute predicted pdf
# @param params Output list produced by auto_shiny function.
# @param params Output list produced by auto_shiny function.
# @param parms_to_summarize String vector with names of parameters to estimate
# @param pct_converged Percentage of bootstrap samples for which estimation converged
# @param pct_converged Percentage of jackknife subsamples for which estimation converged
# @param pi Proportion of studies with H1 true in mixture of studies with H0 vs H1 true (default = 1)
# @param pi Proportion of studies with H1 true in mixture of studies with H0 vs H1 true (default = 1)
# @param pi Vector of different starting values of the pi parameter.
# @param profileCI_args A list of optional arguments to be passed to profileCI.
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
# @param round_to Integer number of decimal places for rounding; use NULL for no rounding.
# @param round_to Number of decimal places to display (default = 3)
# @param sample_size Number of p's in each bootstrap sample
# @param show_error Show error message if any p value is out of
# @param show_progress_bar Boolean determining whether progress bar is used (default = TRUE)
# @param sigma Standard deviation of noncentrality parameters when H1 is true
# @param sigma Standard deviation of noncentrality parameters when H1 is true
# @param sigma Vector of different starting values of the sigma parameter
# @param start A list of starting parameter values for mu, sigma, and pi.
# @param start_parms Either a list of starting parameter values for the optim search,
# @param summary_fn_names Optional list of names for the summary function
# @param summary_fns List of the functions to be computed across the different
# @param tails 1 or 2 to indicate 1- or 2-tailed p values (default = 2)
# @param tails 1 or 2 to indicate 1- or 2-tailed p values (default = 2)
# @param target_cdfs The CDFs of the p-curve distribution at which to determine the quantile p value
# @param target_folded_normal_mu Boolean where TRUE requests CI for folded normal mu
# @param time_stamp String time_stamp appended to output file names
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
