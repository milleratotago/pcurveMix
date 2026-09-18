# table_notes.R

estimates_table_notes <- function(confidence_level = pcm_env$confidence_level,
                                  lower_str = CI_LOWER_BOUND_LABEL,
                                  upper_str = CI_UPPER_BOUND_LABEL) { # NEWJEFF: Note converged?
  s <- sprintf("Wald standard error (se) and %3.1f%% %s/%s confidence interval bounds are computed based on Hessian from maximum likelihood estimation.",
               confidence_level, lower_str, upper_str)
  return(s)
}

jackknife_table_notes <- function(n_samples, pct_converged,
                                  confidence_level = pcm_env$confidence_level,
                                  lower_str = CI_LOWER_BOUND_LABEL,
                                  upper_str = CI_UPPER_BOUND_LABEL) {
  notes <- c(sprintf("results based on estimates from %d samples; %3.1f%% converged.",n_samples, pct_converged),
             "mean and sd of estimates across samples.",
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

