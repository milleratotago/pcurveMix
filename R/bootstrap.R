# bootstrap.R

#' Function for parametric bootstrapping of fitted model.
#' @inheritParams random
#' @inheritParams fit_p_curve
#' @param fit_list Fitted model list returned by fit_p_curve
#' @param n_boot_samples Number of bootstrap samples to take
#' @param show_progress_bar Boolean determining whether progress bar is used (default = TRUE)
#' @returns Data frame with 1 row per bootstrap sample & cols for estimated pi, mu, sigma, etc.
#' @export
bootstrap <- function(n, fit_list, n_boot_samples,
                      # alpha = 1, tails = 2, alpha_sig = 0.05, -- these are taken from "fit_list"
                      show_progress_bar = TRUE,
                      cond_method = "rejection", tol = 1e-8) {
  alpha <- fit_list$alpha
  tails <- fit_list$tails
  alpha_sig <- fit_list$alpha_sig
  use_fn <- tails == 2  # fn is an abbreviation for folded normal
  cols_to_boot <- c("pi", "mu", "sigma", "power")
  if (use_fn) {
    cols_to_boot <- c(cols_to_boot, "folded_normal_mu", "folded_normal_sigma")
  }
  boot <- matrix(NA_real_, nrow = n_boot_samples, ncol = length(cols_to_boot))
  colnames(boot) <- cols_to_boot
  ps_mat <- generate_parametric_subsamples(n_boot_samples, n, fit_list$mu, fit_list$sigma,
                                           pi = fit_list$pi, alpha = alpha, tails = tails,
                                           cond_method = cond_method, tol = tol)
  if (pcm_env$fast_boot_jack) {
    start_list <- list(mu = fit_list$mu, sigma = fit_list$sigma, pi = fit_list$pi)
  } else {
    start_list <- pcm_env$optim_starting_parms
  }
  boot <- fits_for_matrix(ps_mat, alpha = alpha, tails = tails, alpha_sig = alpha_sig,
                          want_optim_hessian = FALSE, start_parms = start_list)
  boot_df <- as.data.frame(boot)
  return(boot_df)
}

# # OBSOLETE
# # Compute named vector of boot_mean, boot_sd, bias, bc_estimate,
# #  not_bc_lower, not_bc_upplower
# boot_comps1 <- function(est_orig, boot_mean, boot_sd, full_sample_n, t_or_z = 2,
#                              center_ci_at_est_orig = FALSE) {
#    tbl$bias <- 999
#    tbl$bc_estimate <- est_orig - tbl$bias
#    # compute CI bounds relative to est_orig
#    lower <- 999
#    upper <- 999
#    q_lower <- 999
#    q_upper <- 999
#    lower_name <- CI_LOWER_BOUND_LABEL
#    upper_name <- CI_UPPER_BOUND_LABEL
#    q_lower_name <- quantile_name(CI_LOWER_BOUND_LABEL)
#    q_upper_name <- quantile_name(CI_UPPER_BOUND_LABEL)
#    if (!center_ci_at_est_orig) {
#      # subtract bias
#      lower <- lower - bias
#      upper <- upper - bias
#      q_lower <- q_lower - bias
#      q_upper <- q_upper - bias
#    }
#    tbl <- data.frame(parameter = parm, mean = boot_mean, sd = boot_sd, bias = bias, bc_estimate = bc_estimate)
#    tbl[[lower_name]] <- lower
#    tbl[[upper_name]] <- upper
#    tbl[[q_lower_name]] <- q_lower
#    tbl[[q_upper_name]] <- q_upper
# }

get_ci_half_width <- function(sds, confidence_level = pcm_env$confidence_level) {
  upper_q <- get_upper_q(confidence_level)
  zcrit <- stats::qnorm(upper_q)
  hw <- zcrit * sds
  return(hw)
}

get_ci_bounds <- function(means, sds, confidence_level = pcm_env$confidence_level) {
  hw <- get_ci_half_width(sds, confidence_level)
  df <- data.frame(lower = means - hw, upper = means + hw)
  names(df) <- c(CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL)
  return(df)
}

#' Function to summarize the data frame produced by parametric or nonparametric bootstrapping.
#' @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl() with
#'  original MLE estimates used as parameter values for parametric bootstrapping
#' @param ests_tbl Data frame of parameter estimates, one row per bootstrap sample
#' @inheritParams set_globals confidence_level bias_correct_ci_bounds
#' @returns A data frame with the means and sds of bootstrap sample
#'  parameter estimates, plus confidence interval boundss.
#' @export
make_boot_summary_tbl <- function(mle_estimates_tbl, ests_tbl, # NEWJEFF: ests_tbl is named differently elsewhere
                                       confidence_level = pcm_env$confidence_level,
                                       bias_correct_ci_bounds = pcm_env$bias_correct_ci_bounds) {
  mn_sd_df <- get_parm_summaries(ests_tbl, summary_fns = list(mean = mean, sd = sd))
  # print(mle_estimates_tbl)  # NEWJEFF
  # print(mn_sd_df)
  bias_df <- data.frame(bias = mle_estimates_tbl$estimate - mn_sd_df$mean)
  bias_df$bc_estimate <- mle_estimates_tbl$estimate - bias_df$bias
  ci_df <- get_ci_bounds(mle_estimates_tbl$estimate, mn_sd_df$sd, confidence_level)
  target_quantiles <- symmetric_tail_quantiles_from_confidence(confidence_level)
  q_df <- get_parm_quantiles(ests_tbl, quantiles = target_quantiles)
  if (bias_correct_ci_bounds) {
    ci_df[[CI_LOWER_BOUND_LABEL]] <- ci_df[[CI_LOWER_BOUND_LABEL]] - bias_df$bias
    ci_df[[CI_UPPER_BOUND_LABEL]] <- ci_df[[CI_UPPER_BOUND_LABEL]] - bias_df$bias
    names(ci_df)[names(ci_df) == CI_LOWER_BOUND_LABEL] <- bias_corrected_name(CI_LOWER_BOUND_LABEL)
    names(ci_df)[names(ci_df) == CI_UPPER_BOUND_LABEL] <- bias_corrected_name(CI_UPPER_BOUND_LABEL)
    q_df[,2] <- q_df[,2] - bias_df$bias
    q_df[,3] <- q_df[,3] - bias_df$bias
    names(q_df)[2:3] <- bias_corrected_name(names(q_df)[2:3])
  }
  df <- cbind(mn_sd_df, bias_df, ci_df, q_df[,-1])
  return(df)
}

# NEWJEFF: OBSOLETE? BETTER TO USE SHINY VERSION
#' Function to summarize the data frame produced by parametric or nonparametric bootstrapping.
#' @param boot_df Output data frame produced by bootstrap() function
#' @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl() with
#'  original MLE estimates used as parameter values for parametric bootstrapping
#' @param boot_ci_limits A vector with the two limiting proportions
#'  (lower, upper) for bootstrap confidence intervals (default = c(0.025, 0.975))
#' @returns A list with the percent of samples in which the estimation process
#'  converged OK and a data frame with the bootstrap
#'  means, SEs, CIs, & bootstrap-corrected estimates of the model parameters
#' @export
make_boot_summary_list <- function(boot_df, mle_estimates_tbl, boot_ci_limits = c(0.025, 0.975)) {
  n_attempts <- nrow(boot_df)
  boot_df <- boot_df[stats::complete.cases(boot_df), , drop = FALSE]
  boot_ok <- !is.na(boot_df$mu) &
    boot_df$pi >= 0     &  boot_df$pi <= 1     &
    boot_df$mu >= 0     &  boot_df$sigma >= 0  &
    boot_df$power >= 0  &  boot_df$power <= 1
  boot_df <- boot_df[boot_ok,]
  n_ok <- nrow(boot_df)
  if (n_ok == 0) {
    problem_string <- "No successful bootstrap refits; try adjusting fit_p_curve() starting parameter values."
    if (pcm_env$shiny_running) {
      shiny::showNotification(problem_string, type = "warning", duration = 45)
      return( list(pct_converged = NULL, boot_tbl = NULL) )
    } else {
      stop(problem_string)
    }
  }
  pct_converged <- 100 * n_ok / n_attempts

  boot_mn <- sapply(boot_df, mean)
  boot_se <- sapply(boot_df, stats::sd)
  boot_ci <- t(sapply(boot_df, stats::quantile, probs = boot_ci_limits))
  colnames(boot_ci) <- c(CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL)

  use_fn <- "folded_normal_mu" %in% names(boot_df)
  parameters <- c("pi","mu","sigma","power")
  if (use_fn) {
    parameters <- c(parameters,"folded_normal_mu","folded_normal_sigma")
  }
  boot_tbl <- data.frame(
    parameter = parameters,
    Boot_Mean = round(boot_mn[parameters], 6),
    Boot_SE   = round(boot_se[parameters], 6),
    Boot_lwr  = round(boot_ci[parameters, CI_LOWER_BOUND_LABEL], 6),
    Boot_upr  = round(boot_ci[parameters, CI_UPPER_BOUND_LABEL], 6),
    row.names = NULL
  )

  boot_tbl <- boot_tbl |> dplyr::arrange(factor(.data$parameter, levels = c("mu", "sigma", "pi")))

  # augment original estimates with folded normal parameters derived from those
  if (use_fn) {
    original_mu <- mle_estimates_tbl$estimate[mle_estimates_tbl$parameter == "mu"]
    original_sigma <- mle_estimates_tbl$estimate[mle_estimates_tbl$parameter == "sigma"]
    folded_normal_mu_original <- mean_folded_normal(original_mu, original_sigma)
    folded_normal_sigma_original <- sd_folded_normal(original_mu, original_sigma)
    original_estimates <- c(mle_estimates_tbl$estimate) # NEWJEFF TESTING, folded_normal_mu_original, folded_normal_sigma_original)
  } else {
    original_estimates <- mle_estimates_tbl$estimate
  }
  # Compute simple bias-corrected estimate:
  boot_tbl$BC_est <- 2*original_estimates - boot_tbl$Boot_Mean

  return( list(pct_converged = pct_converged, boot_tbl = boot_tbl) )
} # boot_summary

