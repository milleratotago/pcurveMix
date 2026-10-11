# table_notes.R
# Unexported functions can be called via pcurveMix:::

estimates_table_notes <- function(converged, confidence_level = get_ao("confidence_level"),
                                  lower_str = CI_LOWER_BOUND_LABEL,
                                  upper_str = CI_UPPER_BOUND_LABEL) {
  sconverged <- ifelse(converged,"Estimation converged","Estimation did NOT converge")
  s <- c(sconverged,
         "mu, sigma, and pi are the only fundamental model parameters; all others are derived from those three.")
         # "standard error (se) is based on Hessian from maximum likelihood estimation.",
         # sprintf("Wald %3.1f%% %s/%s confidence interval bounds are computed as estimate +/- Z_critical * se.",
         #         confidence_level, lower_str, upper_str))
  return(s)
}

jackknife_table_notes <- function(n_samples, pct_converged,
                                  confidence_level = get_ao("confidence_level"),
                                  t_or_z = "t",
                                  sample_string = "jackknife subsamples",
                                  bias_corrected = get_ao("bias_correct_ci_bounds"),
                                  lower_str = CI_LOWER_BOUND_LABEL,
                                  upper_str = CI_UPPER_BOUND_LABEL) {
  if (bias_corrected) {
    est_line <- "estimated bias and bias-corrected estimate (bc_estimate)."
    ci_bounds_line <- sprintf("%s/%s  bias-corrected (bc_) %3.1f%% %s confidence interval bounds.",
                              lower_str, upper_str, confidence_level, t_or_z)
  } else {
    est_line <- "estimated bias."
    ci_bounds_line <- sprintf("%s/%s  %3.1f%% %s confidence interval bounds.",
                              lower_str, upper_str, confidence_level, t_or_z)
  }
  notes <- c(sprintf("results based on estimates from %d %s; %3.1f%% converged.",n_samples, sample_string, pct_converged),
             sprintf("mean and se of estimates across %s.",sample_string),
             est_line,
             ci_bounds_line)
  return(notes)
}

boot_table_notes <- function(n_samples, pct_converged,
                             confidence_level = get_ao("confidence_level"),
                             bias_corrected = get_ao("bias_correct_ci_bounds"),
                             lower_str = CI_LOWER_BOUND_LABEL,
                             upper_str = CI_UPPER_BOUND_LABEL) {
  notes <- jackknife_table_notes(n_samples, pct_converged,
                                 confidence_level = confidence_level,
                                 bias_corrected = bias_corrected,
                                 t_or_z = "z",
                                 sample_string = "bootstrap samples",
                                 lower_str = lower_str, upper_str = upper_str)

  if (bias_corrected) {
    q_line <- "bc_q_xxx values are bias-corrected xxx quantiles of the bootstrap sample estimates."
  } else {
    q_line <- "q_xxx values are xxx quantiles of the bootstrap sample estimates."
  }
  notes <- c(notes, q_line)

  return(notes)
}

profile_table_notes <- function(confidence_level = get_ao("confidence_level"),
                                lower_str = CI_LOWER_BOUND_LABEL,
                                upper_str = CI_UPPER_BOUND_LABEL) {
  s <- sprintf("%3.1f%% %s/%s confidence interval bounds computed from the likelihood profiles shown below using the 'profileCI' package.",
               confidence_level, lower_str, upper_str)
  return(s)
}

