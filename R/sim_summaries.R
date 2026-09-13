# sim_summaries.R
# Functions to analyze results across many sets of parameter estimates
# produced by fitting.R/fits_for_matrix()

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
  if (length(parms_to_summarize) == 1 && parms_to_summarize == "All") {
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
  if (length(parms_to_summarize) == 1 && parms_to_summarize == "All") {
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
                                   center_ci_at_est_orig = TRUE) { # NEWJEFF: Provide user control over center_ci
  parms_to_summarize <- unique(jackknife_summaries$parm)
  jack_df <- data.frame()
  for (parm in parms_to_summarize) {
    # print(parm)
    parm_est_orig <- ests_orig[[parm]]
    # print(parm_est_orig)
    parm_jack_mean <- jackknife_summaries$value[jackknife_summaries$parm == parm
                                                & jackknife_summaries$summary == "mean"]
    # print(parm_jack_mean)
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

#' Function to summarize a set of estimated parameter values across multiple
#'  samples of p values (e.g., for bootstrap samples).
#' @param ests_df Data frame with rows for samples and columns for the
#'  parameters estimated from each sample.
#' @param confidence_level Confidence level used to find confidence interval
#'  quantiles. If <= 0, don't compute quantiles.
#' @param confidence_quantiles A vector with the two limiting proportions
#'  (lower, upper) for bootstrap confidence intervals (default = NA, in
#'  whicn case these are determined symmetrically from the confidence level)
#' @returns Data frame with row for parameters and columns for the mean,
#'  standard error, and (if requested) lower/upper quantiles of the
#'  parameter estimates across samples.
#' @export
summarize_estimates_mn_sd_quan <- function(ests_df,
                                           confidence_level = 95,
                                           confidence_quantiles = NA) {
  # Determine confidence_quantiles if they were not specified:
  if (identical(confidence_quantiles,NA) && confidence_level > 0) confidence_quantiles <-
      symmetric_tail_quantiles_from_confidence(confidence_level)
  # Remember summaries & quantiles are long-form data frames.
  summaries <- get_parm_summaries(ests_df, summary_fns = c(mean = mean, sd = sd))
  tbl <- summaries %>% tidyr::pivot_wider(names_from = summary, values_from = value)
  if (!identical(confidence_quantiles,NA)) {
    quantiles <- get_parm_quantiles(ests_df, quantiles = confidence_quantiles)
    quantiles <- quantiles %>% tidyr::pivot_wider(names_from = quantile, values_from = value)
    tbl <- cbind(tbl, quantiles[,-1]) # Omit parameter column of quantiles
  }
  return( as.data.frame(tbl) )
}

symmetric_tail_quantiles_from_confidence <- function(confidence_level = 95) {
  tail_prob <- (1 - confidence_level/100) / 2
  confidence_quantiles <- c(tail_prob, 1 - tail_prob)
  return(confidence_quantiles)
}

compute_bias_corrected_estimates <- function(original_estimates, bootstrap_mean_estimates) {
  return( 2*original_estimates - bootstrap_mean_estimates )
}

