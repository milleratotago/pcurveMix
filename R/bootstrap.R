# bootstrap.R

#' Function for parametric bootstrapping of fitted model.
#' @inheritParams pcurveMix-package
#' @inheritParams random
#' @inheritParams fit_p_curve
#' @param n_boot_samples Number of bootstrap samples to take
#' @param show_progress_bar Boolean determining whether progress bar is used (default = TRUE)
#' @returns Data frame with 1 row per bootstrap sample & cols for estimated pi, mu, sigma, etc.
#' @export
bootstrap <- function(n, fit_results_list, n_boot_samples,
                      # alpha = 1, tails = 2, alpha_sig = 0.05, -- these are taken from "fit_results_list"
                      show_progress_bar = TRUE,
                      fast_boot_jack = get_ao("fast_boot_jack"),
                      cond_method = "rejection", tol = 1e-8) {
  alpha <- fit_results_list$alpha
  tails <- fit_results_list$tails
  alpha_sig <- fit_results_list$alpha_sig
  use_fn <- tails == 2  # fn is an abbreviation for folded normal
  cols_to_boot <- c("pi", "mu", "sigma", "power")
  if (use_fn) {
    cols_to_boot <- c(cols_to_boot, "folded_normal_mu", "folded_normal_sigma")
  }
  boot <- matrix(NA_real_, nrow = n_boot_samples, ncol = length(cols_to_boot))
  colnames(boot) <- cols_to_boot
  ps_mat <- generate_parametric_subsamples(n_boot_samples, n, fit_results_list$mu, fit_results_list$sigma,
                                           pi = fit_results_list$pi, alpha = alpha, tails = tails,
                                           cond_method = cond_method, tol = tol)
  if (fast_boot_jack) {
    start_list <- list(mu = fit_results_list$mu, sigma = fit_results_list$sigma, pi = fit_results_list$pi)
  } else {
    start_list <- pcm_env$optim_starting_parms
  }
  boot <- fits_for_matrix(ps_mat, alpha = alpha, tails = tails, alpha_sig = alpha_sig,
                          want_optim_hessian = FALSE, start_parms = start_list)
  boot_df <- as.data.frame(boot)
  return(boot_df)
}

get_ci_half_width <- function(sds, confidence_level = get_ao("confidence_level") ) {
  upper_q <- get_upper_q(confidence_level)
  zcrit <- stats::qnorm(upper_q)
  hw <- zcrit * sds
  return(hw)
}

get_ci_bounds <- function(means, sds, confidence_level = get_ao("confidence_level") ) {
  hw <- get_ci_half_width(sds, confidence_level)
  df <- data.frame(lower = means - hw, upper = means + hw)
  names(df) <- c(CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL)
  return(df)
}

#' Function to summarize the data frame produced by parametric or nonparametric bootstrapping.
#' @inheritParams pcurveMix-package
#' @inheritParams fit_to_estimates_tbl
#' @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl() with
#'  original MLE estimates used as parameter values for parametric bootstrapping
#' @returns A data frame with the means and sds of bootstrap sample
#'  parameter estimates, plus confidence interval boundss.
#' @export
make_boot_summary_tbl <- function(mle_estimates_tbl, estimates,
                                  confidence_level = get_ao("confidence_level"),
                                  bias_correct_ci_bounds = get_ao("bias_correct_ci_bounds"),
                                  round_to = get_ao("round_to") ) {
  mn_sd_df <- get_parm_summaries(estimates, summary_fns = list(mean = mean, sd = sd))

  # Parametric bootstrapping:
  # In this case the "true" values are those in the mle_estimates_tbl, and the
  # bias is the extent to which the estimates of the samples depart from those
  # true values.
  bias_df <- data.frame(bias = mn_sd_df$mean - mle_estimates_tbl$estimate)

  # Nonparametric bootstrapping:
  # In this case it works out the same
  # bias_df <- data.frame(bias = mn_sd_df$mean - mle_estimates_tbl$estimate)
  # bias_df$bc_estimate <- mle_estimates_tbl$estimate - bias_df$bias

  ci_df <- get_ci_bounds(mle_estimates_tbl$estimate, mn_sd_df$sd, confidence_level)
  target_quantiles <- symmetric_tail_quantiles_from_confidence(confidence_level)
  q_df <- get_parm_quantiles(estimates, quantiles = target_quantiles)
  if (bias_correct_ci_bounds) {
    bias_df$bc_estimate <- mle_estimates_tbl$estimate - bias_df$bias
    ci_df[[CI_LOWER_BOUND_LABEL]] <- ci_df[[CI_LOWER_BOUND_LABEL]] - bias_df$bias
    ci_df[[CI_UPPER_BOUND_LABEL]] <- ci_df[[CI_UPPER_BOUND_LABEL]] - bias_df$bias
    names(ci_df)[names(ci_df) == CI_LOWER_BOUND_LABEL] <- bias_corrected_name(CI_LOWER_BOUND_LABEL)
    names(ci_df)[names(ci_df) == CI_UPPER_BOUND_LABEL] <- bias_corrected_name(CI_UPPER_BOUND_LABEL)
    q_df[,2] <- q_df[,2] - bias_df$bias
    q_df[,3] <- q_df[,3] - bias_df$bias
    names(q_df)[2:3] <- bias_corrected_name(names(q_df)[2:3])
  }
  df <- cbind(mn_sd_df, bias_df, ci_df, q_df[,-1])
  if (!is.null(round_to)) {
    df[,-1] <- round(df[,-1], round_to)
  }
  return(df)
}

