# bootstrap.R

#' Function for parametric bootstrapping of fitted model.
#' @inheritParams random
#' @inheritParams fit_p_curve
#' @param fit Fitted model returned by fit_p_curve
#' @param n_boot_samples Number of bootstrap samples to take
#' @param show_progress_bar Boolean determining whether progress bar is used (default = TRUE)
#' @returns Data frame with 1 row per bootstrap sample & cols for estimated pi, mu, sigma, etc.
#' @export
bootstrap <- function(n, fit, n_boot_samples, alpha = 1, tails = 2, alpha_sig = 0.05,
                      show_progress_bar = TRUE,
                      cond_method = "rejection", tol = 1e-8) {
  use_fn <- tails == 2  # fn is an abbreviation for folded normal
  cols_to_boot <- c("pi", "mu", "sigma", "power")
  if (use_fn) {
    cols_to_boot <- c(cols_to_boot, "folded_normal_mu", "folded_normal_sigma")
  }
  boot <- matrix(NA_real_, nrow = n_boot_samples, ncol = length(cols_to_boot))
  colnames(boot) <- cols_to_boot

  # Nest function for one sample that is used with console progress bar,
  # shiny progress bar, or no progress bar
  one_boot_sample <- function() {
    rand_ps <- random(n, fit$mu, fit$sigma, pi = fit$pi, alpha = alpha, tails = tails,
                      cond_method = cond_method, tol = tol)
    # print("rand_ps =")
    # print(rand_ps[1:4])
    rand_ps[rand_ps == 0] <- pcm_env$edge_p
    fit_list <- fit_p_curve(rand_ps, alpha = alpha, tails = tails, want_optim_hessian = FALSE)
    pi <- fit_list$pi
    mu <- fit_list$mu
    sigma <- fit_list$sigma
    if (use_fn) {
      folded_normal_mu <- mean_folded_normal(mu, sigma)
      folded_normal_sigma <- sd_folded_normal(mu, sigma)
      n_cols_produced <- 6
    } else {
      n_cols_produced <- 4
    }
    # print(fit_list)
    # opt <- stats::optim(par = start_vec, fn = nll_optim, p = rand_ps, alpha = alpha, tails = tails,
    #                     method = "L-BFGS-B", lower = lower_vec, upper = upper_vec, hessian = FALSE,
    #                     control = pcm_env$optim_control)
    # print(opt)
    # est <- opt$par; pi <- est[1]; mu <- est[2]; sigma <- est[3]
    # print("est = ")
    # print(est)
    # if (isTRUE(opt$convergence == 0)) {
    if (fit_list$converged) {
      vec <- c(pi, mu, sigma,
               cdf(alpha_sig, mu = mu, sigma = sigma, pi = 1, alpha = 1, tails = tails) )   # power estimated from current mu/sigma/pi
      if (use_fn) {
        vec <- c(vec, folded_normal_mu, folded_normal_sigma)
      }
    } else {  # not converged
      vec <- rep(NA,n_cols_produced)
    }
    return(vec)
  } # nested function one_boot_sample

  if (show_progress_bar) {
    if (pcm_env$shiny_running) {
      shiny::withProgress(message = 'Bootstrapping in progress', value = 0, {
        for (b in seq_len(n_boot_samples)) {
          boot[b,] <- one_boot_sample()
          shiny::incProgress(1/n_boot_samples)
        }
      })
    } else {
      pb <- utils::txtProgressBar(min = 0, max = n_boot_samples, style = 3)
      for (b in seq_len(n_boot_samples)) {
        boot[b,] <- one_boot_sample()
        utils::setTxtProgressBar(pb, b)
      }
      close(pb)
    }
  } else {    # No progress bar
    for (b in seq_len(n_boot_samples)) {
      boot[b,] <- one_boot_sample()
    }
  } # if show_progress bar
  boot_df <- as.data.frame(boot)
  return(boot_df)
}

#' Function to summarize the data frame produced by parametric bootstrapping of fitted model.
#' @param boot_df Output data frame produced by bootstrap() function
#' @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl()
#' @param boot_ci_limits A vector with the two limiting proportions
#'  (lower, upper) for bootstrap confidence intervals (default = c(0.025, 0.975))
#' @returns A list with the percent of samples in which the estimation process
#'  converged OK and a data frame with the bootstrap
#'  means, SEs, CIs, & bootstrap-corrected estimates of the model parameters
#' @export
make_bootstrap_summary_list <- function(boot_df, mle_estimates_tbl, boot_ci_limits = c(0.025, 0.975)) {
  n_attempts <- nrow(boot_df)
  boot_df <- boot_df[stats::complete.cases(boot_df), , drop = FALSE]
  boot_ok <- !is.na(boot_df$mu) &
    boot_df$pi >= 0     &  boot_df$pi <= 1     &
    boot_df$mu >= 0     &  boot_df$sigma >= 0  &
    boot_df$power >= 0  &  boot_df$power <= 1
  boot_df <- boot_df[boot_ok,]
  n_ok <- nrow(boot_df)
  if (n_ok == 0) {
    problem_string <- "No successful bootstrap refits; try adjusting start/lower/upper of fit_p_curve()."
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
  colnames(boot_ci) <- c("lwr","upr")

  use_fn <- "folded_normal_mu" %in% names(boot_df)
  parameters <- c("pi","mu","sigma","power")
  if (use_fn) {
    parameters <- c(parameters,"folded_normal_mu","folded_normal_sigma")
  }
  boot_tbl <- data.frame(
    parameter = parameters,
    Boot_Mean = round(boot_mn[parameters], 6),
    Boot_SE   = round(boot_se[parameters], 6),
    Boot_lwr  = round(boot_ci[parameters, "lwr"], 6),
    Boot_upr  = round(boot_ci[parameters, "upr"], 6),
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
} # bootstrap_summary.

