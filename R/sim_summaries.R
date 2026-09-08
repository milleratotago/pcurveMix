# sim_summaries.R
# Functions to analyze results across many sets of parameter estimates
# produced by fits_for_matrix()

#' Compute a long-format data frame with summaries of each combination of
#' summary function & estimated value.
#' @param estimates Data frame of parameter estimates for many datasets
#'  with rows for datasets and cols for parameters
#' @param parms_to_summarize String vector with names of parameters to estimate
#'  (default = "All")
#' @param summary_fns List of the functions to be computed across the different
#'  estimates of each parameter (default = mean = mean, median = median,
#'   sd = sd, iqr, min = min, max = max))
#' @param summary_fn_names Optional list of names for the summary function
#'  values (default = NULL, in which case string function name is used)
#' @returns long-format data frame with cols for parameter, summary type & value
#' @importFrom EnvStats iqr
#' @importFrom stats median sd
#' @export
get_parm_summaries <- function(estimates, parms_to_summarize = "All",
                               summary_fns = list(mean = mean, median = stats::median, sd = stats::sd,
                                                  iqr = EnvStats::iqr, min = min, max = max),
                               summary_fn_names = NULL) {
  if (parms_to_summarize == "All") {
    parms_to_summarize <- colnames(estimates)
    parms_to_summarize <- parms_to_summarize[parms_to_summarize != "converged"]
  }
  if (is.null(summary_fn_names)) summary_fn_names <- names(summary_fns)

  summaries <- data.frame()
  for (parm in parms_to_summarize) {
    for (i_summary_fn in 1:length(summary_fns)) {
      # summary_fn_name <- summary_fn_names[i_summary_fn]
      # one_fn_cols_df <- data.frame(matrix(ncol = length(parms_to_summarize), nrow = 1))
      # colnames(one_fn_cols_df) <- paste0(parms_to_summarize,"_",summary_fn_name)
      # parm_est <- paste0(parm,"_",summary_fn_name)
      # if (some_converged) {
      value <- summary_fns[[i_summary_fn]](estimates[[parm]], na.rm = TRUE)
      # } else {
      #   one_fn_cols_df[[parm_est]] <- NA
      # }
      one_summary <- data.frame(parm = parm, summary = summary_fn_names[i_summary_fn], value = value)
      summaries <- rbind(summaries, one_summary)
    }
  }
  return(summaries)
}

#' Compute a long-format data frame with quantile values for each parameter
#' @param estimates Data frame of parameter estimates for many datasets
#'  with rows for datasets and cols for parameters
#' @inheritParams get_parm_summaries parms_to_summarize
#' @param quantiles Vector of 0-1 quantile values to be returned
#'  (default = c(0.025, 0.975))
#' @param type Integer 1-7 indicating the quantile type
#'  (see quantile function; default = 7)
#' @returns long-format data frame with cols for quantile & parameter
#' @export
get_parm_quantiles <- function(estimates, parms_to_summarize = "All",
                               quantiles = c(0.025, 0.975), type = 7) {
  if (parms_to_summarize == "All") {
    parms_to_summarize <- colnames(estimates)
    parms_to_summarize <- parms_to_summarize[parms_to_summarize != "converged"]
  }
  nquantiles <- length(quantiles)
  quantiles_df <- data.frame()
  for (parm in parms_to_summarize) {
    values <- stats::quantile(estimates[[parm]], probs = quantiles, na.rm = TRUE, names = FALSE, type = type)
    one_set_df <- data.frame(parm = rep(parm,nquantiles), quantile = quantiles, value = values)
    quantiles_df <- rbind(quantiles_df, one_set_df)
  }
  return(quantiles_df)
}

jackknife_comps1 <- function(est_orig, jack_mean, jack_sd, full_sample_n, t_or_z = 2,
                             center_ci_at_est_orig = TRUE) {
  bias <- (full_sample_n - 1) * (jack_mean - est_orig)
  bias_corrected_estimate <- est_orig - bias
  if (center_ci_at_est_orig) {
    center_ci <- est_orig
  } else {
    center_ci <- bias_corrected_estimate
  }
  jack_se <- jack_sd * (full_sample_n - 1) /  sqrt(full_sample_n)
  lower_bound <- center_ci - t_or_z * jack_se
  upper_bound <- center_ci + t_or_z * jack_se
  return( data.frame(bias_corrected_estimate = bias_corrected_estimate, bias = bias, jack_se = jack_se, lower_bound = lower_bound, upper_bound = upper_bound) )
}

#' Function to make a data frame with rows for parms and cols
#'  for jackknife stats associated with each parm.
#' @param ests_orig A list with numerical values of the full-sample estimates
#'  for each parameter
#' @param jackknife_summaries Data frame produced by get_parm_summaries() from
#'  the jackknife subsample parameter estimates
#' @param full_sample_n Number of observations in the original dataset from
#'  which est_orig values were computed
#' @param t_or_z Multiplier of the jackknife standard error used to compute
#'  confidence interval halfwidth
#' @param center_ci_at_est_orig Boolean indicating whether CI is centered
#'  at original estimate (default = TRUE) or at the mean of the jackknife
#'  estimates (when FALSE)
#' @returns data frame with rows for parms and cols for bias_corrected_estimate,
#'  bias, jack_se, lower_bound, and upper_bound
#' @export
jackknife_computations <- function(ests_orig, jackknife_summaries, full_sample_n, t_or_z = 2,
                                   center_ci_at_est_orig = TRUE) {
  parms_to_summarize <- unique(jackknife_summaries$parm)
  jack_df <- data.frame()
  for (parm in parms_to_summarize) {
    print(parm)
    parm_est_orig <- ests_orig[[parm]]
    print(parm_est_orig)
    parm_jack_mean <- jackknife_summaries$value[jackknife_summaries$parm == parm
                                                & jackknife_summaries$summary == "mean"]
    print(parm_jack_mean)
    parm_jack_sd <- jackknife_summaries$value[jackknife_summaries$parm == parm
                                              & jackknife_summaries$summary == "sd"]
    jack_1_parm <- jackknife_comps1(parm_est_orig, parm_jack_mean, parm_jack_sd,
                                    full_sample_n, t_or_z = t_or_z,
                                    center_ci_at_est_orig = center_ci_at_est_orig)
    jack1_df <- cbind( data.frame(parm = parm), jack_1_parm)
    jack_df <- rbind(jack_df, jack1_df)
  }
  return(jack_df)
}

check_jack_comps <- function() {  # NEWJEFF: Move to tests.
  ests_orig <- list(sample_var = 11.6)
  jacksample_ests <- c(9.5, 13.25, 14.1875, 14.1875, 3.25)
  jackknife_summaries <- data.frame(parm = c("sample_var", "sample_var"),
                                    summary = c("mean", "sd"),
                                    value = c(10.875, sd(jacksample_ests)))
  full_sample_n <- 5
  answers <- jackknife_computations(ests_orig, jackknife_summaries, full_sample_n,
                                    t_or_z = 1.96, center_ci_at_est_orig = FALSE)
  print(answers)
  # Correct answers: bias = -2.9, estimate_bc = 14.5 jack_se = 8.372201,
  #  bounds = -1.909514, 30.909514
}

#' To-be-removed function to augment fit_list with parameter names used
#'  by sim_summaries routines.
#' @param fit_list List produced by fit_p_curves
#' @returns List with additional elements for renamed parameters
#' @export
parm_naming_cluge <- function(fit_list) { # NEWJEFF: Super-ugly
  fit_list$power <- fit_list$power_hat
  fit_list$folded_normal_mu <- fit_list$noncentrality_mean
  fit_list$folded_normal_sigma <- fit_list$noncentrality_sd
  return(fit_list)
}

