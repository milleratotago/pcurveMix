# fitting.R

#' Compute negative log-likelihood of a p value or vector of p values
#'  under the model with the indicated parameters. Use the direct
#'  method with pdf() or use a censoring method that bins all p's
#'  less than small_p_bin_cutoff
#' @param p Real 0-1 p value or vector for which nll is to be computed. p values
#'  should be checked to make sure they are in range (0--1] before calling
#'  this function.
#' @inheritParams pdf
#' @inheritParams set_globals
#' @returns Real negative log-likelihood of the p values
#' @export
nll <- function(p, mu, sigma, pi = 1, alpha = 1, tails = 2,
                small_p_bin_cutoff = pcm_env$small_p_bin_cutoff) {
  # print( paste("nll thinks: mu =",mu, "sigma =",sigma, "pi =",pi) )
  if (pi < 0 || pi > 1 || sigma < 0 || mu < 0) return(1e12)
  # if (any(!is.finite(p)) || any(p <= 0 | p >= 1)) return(1e12)
  if (is.null(small_p_bin_cutoff)) {
    # Direct method without censoring
    pdfs <- pcurveMix::pdf(p, mu, sigma, pi, alpha, tails)
# print("NEWJEFF no censoring pdfs:")
# print(pdfs)
    this_nll <- -sum(log(pmax(pdfs, .Machine$double.xmin)))
  } else {
    # Censoring method
    above_cutoff <- p > small_p_bin_cutoff
    pdfs <- pcurveMix::pdf(p[above_cutoff], mu = mu, sigma = sigma, pi = pi, alpha = alpha, tails = tails)
    n_in_small_bin <- sum(p <= small_p_bin_cutoff)
    cdf_at_cutoff <- pcurveMix::cdf(small_p_bin_cutoff, mu, sigma, pi, alpha, tails)
    # this_nll <- -sum(log(pmax(pdfs, .Machine$double.xmin))) -
    #   n_in_small_bin * log(cdf_at_cutoff)
    # print( paste("Cens:", small_p_bin_cutoff, mu, sigma, pi, n_in_small_bin, log(cdf_at_cutoff), sum(log(pdfs))))
    this_nll <- -n_in_small_bin * log(cdf_at_cutoff) - sum(log(pdfs))
  }
  return(this_nll)
}

# This version is just for optim's use; it unpacks to-be-adjusted
# parameters bundled in par.  USED BY profileCI as well as optim with wantpos = true
# param par Vector of the three model parameters pi, mu, sigma (in order)
# param p Vector of p's for which negative log likelihood is to be computed
nll_optim <- function(par, p, alpha = 1, tails = 2) {
  if (pcm_env$fit_constrained) {
    pi <- par[1]; mu <- par[2]; sigma <- par[3]
    # print("constrained")
  } else {
    # Convert reals to parms so that optim can
    #  search in unconstrained real space.
# print(par) # NEWJEFF
    reals <- list(pi = par[1], mu = par[2], sigma = par[3])
    parms <- reals_to_parms(reals)
    pi <- parms$pi
    mu <- parms$mu
    sigma <- parms$sigma
    # print("unconstrained")
  }
  this_nll <- pcurveMix::nll(p, mu, sigma, pi, alpha = alpha, tails = tails)
# print( paste("mu =",mu,"& sigma = ",sigma,"& pi =",pi,"gives nll =",this_nll)) # NEWJEFF
# readline(prompt="Press [enter] to continue")
  # if (this_nll < 0.01) {
  #   stop("error") # NEWJEFF
  # }
  return(this_nll)
}

#' Fit estimates of the model parameters pi, mu, sigma to a vector of p values.
#' Repeatedly fit model with multiple starting points,
#'  saving at the end the best fit.
#' @param p Vector of to-be-fitted p values in 0--1
#' @inheritParams pdf
#' @param alpha_sig Significance cutoff used in computing the estimated average
#'  power when H0 is false (default = 0.05)
#' @param want_optim_hessian Boolean indicating whether optim() should compute
#'  the hessian (default = TRUE, but set to FALSE for faster bootstrapping where
#'  hessian is not used)
#' @param start_parms Either a list of starting parameter values for the optim search,
#'  or else a data frame where each row is a combination of starting parameter values
#'  and the function tries all combinations (defaults to optim_starting_parms).
#'  NEWJEFF OBSOLETE start_parms$pi values of NA are replaced with the proportion of to-be-fitted
#'  p values that are significant (i.e., <= sig_cutoff_p)
#' @param sig_cutoff_p Significance cutoff used to determine the proportion of
#'  significant to-be-fitted p values for use in adjusting starting value of pi
#'  (default = 0.05)
#' @param lower List of lower bounds for the optim search
#'  (defaults: mu = 0, sigma = 1e-6, pi = 1e-6)
#' @param upper List of upper bounds for the optim search
#'  (defaults: mu = 20, sigma = 10, pi = 1 - 1e-6)
#' @returns List including estimated parameter values, their standard errors
#'  and 95% confidence limits, an estimate of the average power to reject
#'  H0 when it is false, and more
#' @export
fit_p_curve <- function(p, alpha = 1, tails = 2, alpha_sig = 0.05, want_optim_hessian = TRUE,
                        start_parms = pcm_env$optim_starting_parms,
                        sig_cutoff_p = 0.05,
                        lower = list(mu =  0, sigma = 1e-6, pi = 1e-6),
                        upper = list(mu = 20, sigma = 10,   pi = 1 - 1e-6)) {
  if (any(is.na(start_parms$pi))) {  # NEWJEFF: No longer supported?
    pi_est <- mean(p <= sig_cutoff_p)
    start_parms$pi[is.na(start_parms$pi)] <- pi_est
  }
  single_start <- !is.data.frame(start_parms)
  if (single_start) {
    best_fit <- fit_p_curve1(p, alpha = alpha, tails = tails, alpha_sig = alpha_sig,
                             want_optim_hessian = want_optim_hessian,
                             start = start_parms, lower = lower, upper = upper)
    best_fit$start_parm_set <- NA
  } else {
    n_starting_points <- nrow(start_parms)
    start_parms1 <- as.list(start_parms[1,])
    best_fit <- fit_p_curve1(p, alpha = alpha, tails = tails, alpha_sig = alpha_sig,
                             want_optim_hessian = want_optim_hessian,
                             start = start_parms1, lower = lower, upper = upper)
    for (i_row in 2:n_starting_points) {
      start_parms1 <- as.list(start_parms[i_row,])
      one_fit <- fit_p_curve1(p, alpha = alpha, tails = tails, alpha_sig = alpha_sig,
                              want_optim_hessian = want_optim_hessian,
                              start = start_parms1, lower = lower, upper = upper)
      if (one_fit$logLik > best_fit$logLik) best_fit <- one_fit
    }
    best_fit$start_parm_set <- start_parms
  }
  if (tails == 1) {
    best_fit$noncentrality_mean <- best_fit$mu
    best_fit$noncentrality_sd <- best_fit$sigma
  } else {
    best_fit$noncentrality_mean <- mean_folded_normal(best_fit$mu, best_fit$sigma)
    best_fit$noncentrality_sd <- sd_folded_normal(best_fit$mu, best_fit$sigma)
  }
  return(best_fit)
}


#' Fit estimates of model pi, mu, sigma to a vector of p values starting from
#'  a single combination of parameter values.
#' @inheritParams fit_p_curve
#' @param start A list of starting parameter values for mu, sigma, and pi.
#' @export
fit_p_curve1 <- function(p, alpha = 1, tails = 2, alpha_sig = 0.05,
                         want_optim_hessian = TRUE,
                         start = pcm_env$optim_starting_parms,
                         lower = list(mu =  0, sigma = 1e-6, pi = 1e-6),
                         upper = list(mu = 20, sigma = 10,   pi = 1 - 1e-6)) {
  p <- as.numeric(p);
  check_ps_list <- check_ps(p, alpha_cutoff = alpha)
  if (!check_ps_list$all_in_bounds) {
    p <- check_ps_list$ps_in_bounds
    if (pcm_env$shiny_running) {
      problem_string <- bad_ps_report_string(check_ps_list)
      shiny::showModal(shiny::modalDialog(title = "Problematic p values", problem_string, easyClose = TRUE))
      # shiny::showNotification(problem_string, type = "warning", duration = NULL) # NULL leaves it on screen permanently
    }
  }

  if (!length(p)) stop("No valid p-values in (0,1).")
  if (pcm_env$fit_constrained) {
    fit <- optim_fit_constrained(p, alpha, tails, alpha_sig,
                                 start, want_optim_hessian = want_optim_hessian, lower, upper)
  } else {
    fit <- optim_fit_unconstrained(p, alpha, tails, alpha_sig, start, want_optim_hessian = want_optim_hessian)
  }
  # computing power when effect is always present (pi = 1), unconditional on alpha cutoff
  fit$power_hat <- cdf(alpha_sig, mu = fit$mu, sigma = fit$sigma, pi = 1, alpha = 1, tails = tails)
  cdf_fit <- function(x) cdf(x, mu = fit$mu, sigma = fit$sigma, pi = fit$pi, alpha = alpha, tails = tails)
  fit$ks <- ks_with_cdf(p, cdf_fit)
  fit$n <- length(p)
  fit$min_p <- min(p)
  fit$max_p <- max(p)
  fit$check_ps_list <- check_ps_list
  # print( paste(start$mu, start$sigma, start$pi, fit$mu, fit$sigma, fit$pi, fit$logLik) )
  return(fit)
} # fit_p_curve

# optim_fit_unconstrained <- function(p, alpha, tails, alpha_sig, start_list,
#                                     want_optim_hessian) {
#   start_reals <- parms_to_reals(start_list)
#   start_real_vec <- c(start_reals$pi, start_reals$mu, start_reals$sigma)
#   opt <- stats::optim(par = start_real_vec, fn = nll_optim, p = p, alpha = alpha, tails = tails,
#                       method = "BFGS", hessian = want_optim_hessian,
#                       control = pcm_env$optim_control)
#   est <- opt$par;
#   real_parms <- list(mu = est[2], sigma = est[3], pi = est[1])
#   parms <- reals_to_parms(real_parms)
#   # MLSE <- pcm_MLSE(p, parms$mu, parms$sigma, parms$pi, alpha, tails)  # NEWJEFF These look wrong
#   # est <- c(parms$pi, parms$mu, parms$sigma)
#   # l <- make_se_ci(est, MLSE$SE)  # NEWJEFF: make_se_ci no longer used
#   l <- real_to_nat_se_ci(opt$par, opt$hessian)
#   fit <- list(alpha = alpha, alpha_sig = alpha_sig, tails = tails,
#               pi = parms$pi, mu = parms$mu, sigma = parms$sigma, start = start_list,
#               se = l$se, ci95 = l$ci, logLik = -opt$value,
#               converged = (opt$convergence == 0))
#   return(fit)
# }

make_se_ci <- function(est, se) {
  if (!any(is.na(se))) {
    z <- 1.96
    ci <- cbind(est - z*se, est + z*se)
    rownames(ci) <- c("pi","mu","sigma"); colnames(ci) <- c("lwr95","upr95")
    ci["pi",]    <- pmin(pmax(ci["pi",], 1e-6), 1 - 1e-6)
    ci["mu",]    <- pmax(ci["mu",], 0)
    ci["sigma",] <- pmax(ci["sigma",], 1e-6)
  } else {
    # Ensure that there is _something_ in these positions.
    se <- c(NA, NA, NA);
    ci <- matrix(rep(NA,6), nrow = 3, ncol = 2);
    rownames(ci) <- c("pi","mu","sigma"); colnames(ci) <- c("lwr95","upr95")
  }
  names(se) <- c("pi","mu","sigma")
  return( list(se = se, ci = ci) )
}

# Helper: KS with tiny jitter to avoid ties warnings
ks_with_cdf <- function(p, cdf_fun, jitter_scale = 1e-9) {
  p2 <- if (any(duplicated(p))) p + stats::runif(length(p), -jitter_scale, jitter_scale) else p
  suppressWarnings(stats::ks.test(p2, cdf_fun))
}

#' Convert the fit_p_curve parameter estimates into a nice data frame.
#' @param fit Output list from fit_p_curve
#' @returns A data frame
#' @importFrom rlang .data
#' @export
fit_to_estimates_tbl <- function(fit) {
  mle_tbl <- data.frame(
    parameter = c("pi","mu","sigma","power"),
    estimate  = c(fit$pi, fit$mu, fit$sigma, fit$power_hat),
    Wald_SE   = c(if (!is.null(fit$se)) fit$se else c(NA,NA,NA), NA),
    Wald_lwr  = c(if (!is.null(fit$ci95)) fit$ci95[, "lwr95"] else c(NA,NA,NA), NA),
    Wald_upr  = c(if (!is.null(fit$ci95)) fit$ci95[, "upr95"] else c(NA,NA,NA), NA),
    row.names = NULL
  )
  if (fit$tails == 2) {
    folded_normal_mu <- mean_folded_normal(fit$mu, fit$sigma)
    folded_normal_sigma <- sd_folded_normal(fit$mu, fit$sigma)
    folded_normal_cols <- data.frame(
      parameter = c("folded_normal_mu", "folded_normal_sigma"),
      estimate  = c(folded_normal_mu, folded_normal_sigma),
      Wald_SE   = c(NA, NA),
      Wald_lwr  = c(NA, NA),
      Wald_upr  = c(NA, NA),
      row.names = NULL
    )
    mle_tbl <- rbind(mle_tbl, folded_normal_cols)
  }
  mle_tbl <- mle_tbl |> dplyr::arrange(factor(.data$parameter, levels = c("mu", "sigma", "pi", "power")))
  return(mle_tbl)
}

starting_parms_to_descriptors <- function(starting_parm_set) {
  if ( is.data.frame(starting_parm_set) ) {
    mu_unique <- round( unique(starting_parm_set$mu), digits = 3 )
    sigma_unique <- round( unique(starting_parm_set$sigma), digits = 3 )
    pi_unique <- round( unique(starting_parm_set$pi), digits = 3 )
    start_str <- paste("mu =", paste(mu_unique, collapse = ",") )
    descriptors_mu <- descriptor("starting value grid:",start_str)
    start_str <- paste("sigma =", paste(sigma_unique, collapse = ",") )
    descriptors_sigma <- descriptor(" ",start_str)
    start_str <- paste("pi =", paste(pi_unique, collapse = ",") )
    descriptors_pi <- descriptor(" ",start_str)
    descriptors <- rbind(descriptors_mu, descriptors_sigma, descriptors_pi)
  } else {
    rounded <- lapply(starting_parm_set, round, digits = 3)
    start_str <- paste(names(starting_parm_set), rounded, sep = " = ", collapse = ", ")
    descriptors <- descriptor("starting values:",start_str)
  }
  return(descriptors)
}

#' Convert the fit_p_curve fit descriptors into a nice data frame.
#' @param fit Output list from fit_p_curve
#' @param file_name Optional string used to include p file name in table
#' @returns A data frame
#' @export
fit_to_descriptor_tbl <- function(fit, file_name = NULL) {
  descriptor_tbl <- data.frame()
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("---FITTING OPTIONS---", "-------------"))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("alpha",as.character(round(fit$alpha,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("tails",as.character(round(fit$tails,0))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("alpha_sig",as.character(round(fit$alpha_sig,3))))
  # rounded <- lapply(fit$start_parm_set, round, digits = 3)
  # start_str <- paste(names(fit$start_parm_set), rounded, sep = " = ", collapse = ", ")
  # descriptor_tbl <- rbind(descriptor_tbl, descriptor("starting values",start_str))
  if (any(!is.na(fit$start_parm_set))) {
    descriptor_tbl <- rbind(descriptor_tbl, starting_parms_to_descriptors(fit$start_parm_set) )
  }
  # descriptor_tbl <- rbind(descriptor_tbl, descriptor("edge_p",as.character(pcm_env$edge_p)))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("Parameter ranges", ifelse(pcm_env$fit_constrained,"Constrained","Unconstrained")))
  if (is.null(pcm_env$small_p_bin_cutoff)) {
    descriptor_tbl <- rbind(descriptor_tbl, descriptor("Low p censoring", "Unused"))
  } else {
    descriptor_tbl <- rbind(descriptor_tbl, descriptor( "Low p's censored at small_p_bin_cutoff =",
                                                        pcm_env$small_p_bin_cutoff) )
  }
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("---DATASET OF p's---", "-------------"))
  if (!is.null(file_name)) descriptor_tbl <- rbind(descriptor_tbl, descriptor("file name", file_name))
  if (fit$check_ps_list$n_too_small > 0) descriptor_tbl <- rbind(descriptor_tbl, descriptor("****** WARNING: ******", paste("excluded",fit$check_ps_list$n_too_small,"p's < 0")))
  if (fit$check_ps_list$n_too_large > 0) descriptor_tbl <- rbind(descriptor_tbl, descriptor("****** WARNING: ******", paste("excluded",fit$check_ps_list$n_too_large,"p's > ",fit$alpha)))
  if (fit$check_ps_list$n_equal_zero > 0) descriptor_tbl <- rbind(descriptor_tbl, descriptor("INFORMATION:", paste(fit$check_ps_list$n_equal_zero,"small p's set to",pcm_env$edge_p)))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("n_fitted_p's", as.character(round(fit$n,0))))
  smin <- formatC(fit$min_p, format = "e", digits = 6)
  if (fit$max_p < 0.001) {
    smax <- formatC(fit$max_p, format = "e", digits = 6)
  } else {
    smax <- as.character(round(fit$max_p,6))
  }
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("min(p)", smin))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("max(p)", smax))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("---FITTING RESULTS---", "-------------"))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("fit converged",as.character(fit$converged)))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("log likelihood",as.character(round(fit$logLik,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("k-s statistic",as.character(round(fit$ks$statistic,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("k-s p value",as.character(round(fit$ks$p.value,5))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("noncentrality mean",as.character(round(fit$noncentrality_mean,3))))
  descriptor_tbl <- rbind(descriptor_tbl, descriptor("noncentrality sd",as.character(round(fit$noncentrality_sd,3))))
  rownames(descriptor_tbl) <- NULL
  return(descriptor_tbl)
}

descriptor <- function(slabel, svalue) {
  tbl <- data.frame("Property" = slabel, "Value" = svalue)
  return(tbl)
}
