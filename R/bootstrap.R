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
#' @returns Data frame with 1 row per bootstrap sample.
#' @export
bootstrap <- function(n, fit, n_boot_samples, alpha = 1, tails = 2) {
  boot <- matrix(NA_real_, nrow = n_boot_samples, ncol = 4)
  colnames(boot) <- c("pi", "mu", "sigma", "power")
  # pb <- txtProgressBar(min = 0, max = n_boot_samples, style = 3)
  for (b in seq_len(n_boot_samples)) {
    rand_ps <- random(n, fit$mu, fit$sigma, pi = fit$pi, alpha = alpha, tails = tails)
    fit_b <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
    if (isTRUE(fit_b$converged)) {
      boot[b, 1:3] <- c(fit_b$pi, fit_b$mu, fit_b$sigma)
      boot[b, 4]   <- cdf(alpha, mu = fit_b$mu, sigma = fit_b$sigma, pi = fit_b$pi, alpha = alpha, tails = tails)  # power for that draw
    }
    # setTxtProgressBar(pb, b)
  }
  # close(pb)

  boot_df <- as.data.frame(boot)
  return(boot_df)
  #
}

#' Function to summarize the data frame produced by parametric bootstrapping of fitted model.
#' @param boot_df Output data frame produced by bootstrap() function.
#' @returns Summary data frame with SEs & CIs for model parameters
#' @export
bootstrap_summary <- function(boot_df) {
  boot_df <- boot_df[stats::complete.cases(boot_df), , drop = FALSE]
  if (!nrow(boot_df)) stop("No successful bootstrap refits -- adjust starts/bounds.")

  boot_se <- sapply(boot_df, stats::sd)
  boot_ci <- t(sapply(boot_df, stats::quantile, probs = c(0.025, 0.975)))  # NEWJEFF: probs argument
  colnames(boot_ci) <- c("lwr","upr")

  boot_tbl <- data.frame(
    parameter = c("pi","mu","sigma","power"),
    Boot_SE   = round(boot_se[c("pi","mu","sigma","power")], 6),
    Boot_lwr  = round(boot_ci[c("pi","mu","sigma","power"), "lwr"], 6),
    Boot_upr  = round(boot_ci[c("pi","mu","sigma","power"), "upr"], 6),
    row.names = NULL
  )
  return(boot_tbl)
  # cat("\n[Bootstrap summary]\n")
  # print(boot_tbl, row.names = FALSE)
}

#' Function to combine tables produced by mle and by bootstrap.
#' @param mle_tbl Table produced fit fit_p_curve
#' @param boot_tbl Output data frame produced by bootstrap() function.
#' @returns Combined data frame with fit & boot estimates, SEs & CIs for model parameters
#' @importFrom rlang .data
#' @export
merge_tables <- function(mle_tbl, boot_tbl) {
  combined <- dplyr::inner_join(mle_tbl, boot_tbl, by = "parameter")
  names(combined) <- c("Param",
                       "MLE_est", "Wald_SE", "Wald_CI_lwr", "Wald_CI_upr",
                       "Boot_SE", "Boot_CI_lwr", "Boot_CI_upr")
  combined <- combined |> dplyr::arrange(factor(.data$Param, levels = c("mu", "sigma", "pi", "power")))
  return(combined)
}
