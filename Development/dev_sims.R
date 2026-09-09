# dev_sims.R
# development of simulation routines

# 2026-09-10 fits_for_matrix with progress bar

library(progressr)

### PARAMETRIC
n_subsamples <- 100
n_per_subsample <- 200
mu <- 2
sigma <- 1
pi <- 0.5

parametric_ps <- generate_parametric_subsamples(n_subsamples, n_per_subsample, mu, sigma, pi = pi, alpha = 1)
# ps <- parametric_ps[1,]
# hist(ps)
# fit_list <- fit_p_curve(ps, alpha = alpha)
# print(fit_to_parms_vec(fit_list), want_names = TRUE)
# hist(as.vector(parametric_ps))
parametric_fits_df <- fits_for_matrix(parametric_ps)

stop("stopped as requested")


# ===
# ps <- pcurveMix::random(n = 200, mu = 3, sigma = 1, pi = 0.5, alpha = alpha)
OSC <- read.csv("/R/Projects/pcurve_repo/ProfileCI/OSC_data.csv")
# alpha <- 0.05   # for ps_orig
# ps <- OSC$p_orig
alpha <- 1   # for ps_orig
ps <- OSC$p_rep  # NOTE LATER p_values
# NEWJEFF: Assuming unconstrained original fit
hist(ps)
fit_list <- fit_p_curve(ps, alpha = alpha)
print(fit_to_parms_vec(fit_list), want_names = TRUE)

osc_orig_jack_ps <- generate_jackknife_subsamples(ps)
osc_orig_jack_ests_df <- fits_for_matrix(osc_orig_jack_ps)
osc_orig_jack_summaries <- get_parm_summaries(osc_orig_jack_ests_df)
# osc_orig_jack_quantiles <- get_parm_quantiles(osc_orig_jack_ests_df)
full_sample_n <- nrow(osc_orig_jack_ests_df)
# fit_list <- parm_naming_cluge(fit_list)
osc_orig_jack_final_df <- jackknife_computations(fit_list, osc_orig_jack_summaries, full_sample_n)

stop("stopped as requested")

### PARAMETRIC
n_subsamples <- 10
n_per_subsample <- 200
mu <- 2
sigma <- 1
pi <- 0.5

parametric_ps <- generate_parametric_subsamples(n_subsamples, n_per_subsample, mu, sigma, pi = pi, alpha = 1)
# ps <- parametric_ps[1,]
# hist(ps)
# fit_list <- fit_p_curve(ps, alpha = alpha)
# print(fit_to_parms_vec(fit_list), want_names = TRUE)
# hist(as.vector(parametric_ps))
parametric_fits_df <- fits_for_matrix(parametric_ps)

# fit_list <- fit_p_curve(parametric_ps[1,])
# print(fit_list)

### NONPARAMETRIC
ps <- parametric_ps[1,]  # Just a pretend sample
nonparametric_ps <- generate_nonparametric_subsamples(n_subsamples, n_per_subsample, ps)
nonparametric_fits_df <- fits_for_matrix(nonparametric_ps)

### OLD PARAMETRIC SO I CAN CHECK THE OUTPUT
fit_list <- fit_p_curve(ps, alpha = alpha, tails = tails)
nps <- 90
n_boot_samples <- 100
old_parametric <- bootstrap(nps, fit_list, n_boot_samples,
                      # alpha = 1, tails = 2, alpha_sig = 0.05, -- these are taken from "fit"
                      show_progress_bar = FALSE)

