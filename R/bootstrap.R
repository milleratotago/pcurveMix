# bootstrap.R

# NEWJEFF: Accepting lots of defaults for parameters of these two functions:
# random <- function(n, mu, sigma, pi = 1, alpha = 1, tails = 2, cond_method = c("rejection", "inversion"), tol = 1e-8 )
# fit_p_curve <- function(p, alpha = 1, tails = 2,
#                         start = list(pi = 0.5, mu = 2, sigma = 2),
#                         lower = c(1e-6,      0, 1e-6),
#                         upper = c(1 - 1e-6, 15, 10))

#' Function for parametric bootstrapping of fitted model.
#' @inheritParams random
#' @param fit Fitted model returned by fit_p_curve.
#' @param n_boot_samples Number of bootstrap samples to take
#' @param show_progress_bar Boolean determining whether progress bar is used (default = TRUE)
#' @returns Data frame with 1 row per bootstrap sample.
#' @export
bootstrap <- function(n, fit, n_boot_samples, alpha = 1, tails = 2,
                      show_progress_bar = TRUE) {
  if (show_progress_bar) shiny_running <- shiny::isRunning()
  boot <- matrix(NA_real_, nrow = n_boot_samples, ncol = 4)
  colnames(boot) <- c("pi", "mu", "sigma", "power")

  # Nest function for one sample that is used with console progress bar,
  # shiny progress bar, or no progress bar
  one_boot_sample <- function() {
    rand_ps <- random(n, fit$mu, fit$sigma, pi = fit$pi, alpha = alpha, tails = tails)
    fit_b <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
    if (isTRUE(fit_b$converged)) {
      vec <- c(fit_b$pi, fit_b$mu, fit_b$sigma,
               cdf(fit$alpha_sig, mu = fit_b$mu, sigma = fit_b$sigma, pi = 1, alpha = 1, tails = tails))  # power for that draw
    } else {
      vec <- rep(NA,4)
    }
    return(vec)
  }

  if (show_progress_bar) {
    if (shiny_running) {
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
#' @param boot_df Output data frame produced by bootstrap() function.
#' @param mle_estimates_tbl Data frame produced by fit_to_estimates_tbl().
#' @param boot_ci_limits A vector with the two limiting proportions.
#'  (lower, upper) for bootstrap confidence intervals (default = c(0.025, 0.975)).
#' @returns A list with the percent of samples in which the estimation process
#'  converged OK and a data frame with the bootstrap
#'  means, SEs, CIs, & bootstrap-corrected estimates of the model parameters.
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
    if (shiny_running) {
      showNotification(problem_string, type = "warning", duration = 15)
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

  boot_tbl <- data.frame(
    parameter = c("pi","mu","sigma","power"),
    Boot_Mean = round(boot_mn[c("pi","mu","sigma","power")], 6),
    Boot_SE   = round(boot_se[c("pi","mu","sigma","power")], 6),
    Boot_lwr  = round(boot_ci[c("pi","mu","sigma","power"), "lwr"], 6),
    Boot_upr  = round(boot_ci[c("pi","mu","sigma","power"), "upr"], 6),
    row.names = NULL
  )

  boot_tbl <- boot_tbl |> dplyr::arrange(factor(.data$parameter, levels = c("mu", "sigma", "pi", "power")))
  # Compute simple bias-corrected estimate:
  boot_tbl$BC_est <- 2*mle_estimates_tbl$estimate - boot_tbl$Boot_Mean

  return( list(pct_converged = pct_converged, boot_tbl = boot_tbl) )
} # bootstrap_summary.

