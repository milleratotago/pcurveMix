# table_notes.R
# Unexported functions must be called via pcurveMix:::
# NEWJEFF: Should these check whether bias_correction is being used?

estimates_table_notes <- function(converged, confidence_level = pcm_env$confidence_level,
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
                                  confidence_level = pcm_env$confidence_level,
                                  sample_string = "jackknife subsamples",
                                  lower_str = CI_LOWER_BOUND_LABEL,
                                  upper_str = CI_UPPER_BOUND_LABEL) {
  notes <- c(sprintf("results based on estimates from %d %s; %3.1f%% converged.",n_samples, sample_string, pct_converged),
             sprintf("mean and se of estimates across %s.",sample_string),
             "estimated bias and bias-corrected estimate (bc_estimate).",
             sprintf("%s/%s  bias-corrected (bc_) %3.1f%% confidence interval bounds.", lower_str, upper_str, confidence_level)
  )
  return(notes)
}

boot_table_notes <- function(n_samples, pct_converged,
                             confidence_level = pcm_env$confidence_level,
                             lower_str = CI_LOWER_BOUND_LABEL,
                             upper_str = CI_UPPER_BOUND_LABEL) {
  notes <- jackknife_table_notes(n_samples, pct_converged,
                                 confidence_level = confidence_level,
                                 sample_string = "bootstrap samples",
                                 lower_str = lower_str, upper_str = upper_str)

  notes <- c(notes,
             "bc_q_xxx values are bias-corrected xxx quantiles of the bootstrap sample estimates.")
  return(notes)
}


profile_table_notes <- function(confidence_level = pcm_env$confidence_level,
                                lower_str = CI_LOWER_BOUND_LABEL,
                                upper_str = CI_UPPER_BOUND_LABEL) {
  s <- sprintf("%3.1f%% %s/%s confidence interval bounds computed from the likelihood profiles shown below using the 'profileCI' package.",
               confidence_level, lower_str, upper_str)
  return(s)
}

