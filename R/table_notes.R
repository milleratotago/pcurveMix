# table_notes.R
# Unexported functions can be called via pcurveMix:::

#' Generate notes for table of ML estimates
#' @param converged Boolean indicating whether the optim estimation process converged
#' @inheritParams set_globals confidence_level
#' @returns A vector of strings
#' @export
estimates_table_notes <- function(converged, confidence_level = get_ao("confidence_level") ) {
  sconverged <- ifelse(converged,"Estimation converged","Estimation did NOT converge")
  s <- c(sconverged,
         "mu, sigma, and pi are the only fundamental model parameters; all others are derived from those three.")
  # "standard error (se) is based on Hessian from maximum likelihood estimation.",
  # sprintf("Wald %3.1f%% %s/%s confidence interval bounds are computed as estimate +/- Z_critical * se.",
  #         confidence_level, CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL))
  return(s)
}

# Utility function called to make jackknife and bootstrap table notes
utility_table_notes <- function(n_samples, pct_converged,
                                confidence_level, t_or_z,
                                sample_string,
                                bias_correct_ci_bounds) {
  if (bias_correct_ci_bounds) {
    est_line <- "estimated bias and bias-corrected estimate (bc_estimate)."
    ci_bounds_line <- sprintf("%s/%s  bias-corrected (bc_) %3.1f%% %s confidence interval bounds.",
                              CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL, confidence_level, t_or_z)
  } else {
    est_line <- "estimated bias."
    ci_bounds_line <- sprintf("%s/%s  %3.1f%% %s confidence interval bounds.",
                              CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL, confidence_level, t_or_z)
  }
  notes <- c(sprintf("results based on estimates from %d %s; %3.1f%% converged.",n_samples, sample_string, pct_converged),
             sprintf("mean and se of estimates across %s.",sample_string),
             est_line,
             ci_bounds_line)
  return(notes)
}

#' Generate notes for table of jackknife estimates
#' @param n_samples Number of jackknife subsamples
#' @param pct_converged Percentage of jackknife subsamples for which estimation converged
#' @inheritParams set_globals confidence_level bias_correct_ci_bounds
#' @returns A vector of strings
#' @export
jackknife_table_notes <- function(n_samples, pct_converged,
                                  confidence_level = get_ao("confidence_level"),
                                  # t_or_z = "t",
                                  # sample_string = "jackknife subsamples",
                                  bias_correct_ci_bounds = get_ao("bias_correct_ci_bounds") ) {
  # if (bias_correct_ci_bounds) {
  #   est_line <- "estimated bias and bias-corrected estimate (bc_estimate)."
  #   ci_bounds_line <- sprintf("%s/%s  bias-corrected (bc_) %3.1f%% %s confidence interval bounds.",
  #                             CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL, confidence_level, t_or_z)
  # } else {
  #   est_line <- "estimated bias."
  #   ci_bounds_line <- sprintf("%s/%s  %3.1f%% %s confidence interval bounds.",
  #                             CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL, confidence_level, t_or_z)
  # }
  # notes <- c(sprintf("results based on estimates from %d %s; %3.1f%% converged.",n_samples, sample_string, pct_converged),
  #            sprintf("mean and se of estimates across %s.",sample_string),
  #            est_line,
  #            ci_bounds_line)
  notes <- utility_table_notes(n_samples, pct_converged,
                               confidence_level, t_or_z = "t",
                               sample_string = "jackknife subsamples",
                               bias_correct_ci_bounds)
  return(notes)
}

#' Generate notes for table of bootstrap estimates
#' @param n_samples Number of bootstrap samples
#' @param pct_converged Percentage of bootstrap samples for which estimation converged
#' @inheritParams set_globals confidence_level bias_correct_ci_bounds
#' @returns A vector of strings
#' @export
boot_table_notes <- function(n_samples, pct_converged,
                             confidence_level = get_ao("confidence_level"),
                             bias_correct_ci_bounds = get_ao("bias_correct_ci_bounds") ) {
  # notes <- jackknife_table_notes(n_samples, pct_converged,
  #                                confidence_level = confidence_level,
  #                                bias_correct_ci_bounds = bias_correct_ci_bounds,
  #                                t_or_z = "z",
  #                                sample_string = "bootstrap samples")

  notes <- utility_table_notes(n_samples, pct_converged,
                               confidence_level, t_or_z = "z",
                               sample_string = "bootstrap subsamples",
                               bias_correct_ci_bounds)
  if (bias_correct_ci_bounds) {
    q_line <- "bc_q_xxx values are bias-corrected xxx quantiles of the bootstrap sample estimates."
  } else {
    q_line <- "q_xxx values are xxx quantiles of the bootstrap sample estimates."
  }
  notes <- c(notes, q_line)

  return(notes)
}

#' Generate notes for table of profile confidence intervals
#' @inheritParams set_globals confidence_level
#' @returns A vector of strings
#' @export
profile_table_notes <- function(confidence_level = get_ao("confidence_level") ) {
  s <- sprintf("%3.1f%% %s/%s confidence interval bounds computed from the likelihood profiles shown below using the 'profileCI' package.",
               confidence_level, CI_LOWER_BOUND_LABEL, CI_UPPER_BOUND_LABEL)
  return(s)
}

