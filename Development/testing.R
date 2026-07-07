# testing.R

if ("pcurveMix" %in% .packages()) {
  detach(package:pcurveMix, unload = TRUE)
}
library(pcurveMix)

### Simple fit

n_ps <- 200
alpha <- 1
tails <- 2
set_globals(reset_to_defaults = TRUE, fit_constrained = !TRUE)
ps <- pcurveMix::random(n_ps, mu = 1, sigma = 2, pi = 0.3, alpha = alpha, tails = tails)
fit_list <- fit_p_curve(ps, alpha = alpha, tails = tails)
# print(fit_list)
descriptor_tbl <- fit_to_descriptor_tbl(fit_list)
print(descriptor_tbl)

estimates_tbl <- fit_to_estimates_tbl(fit_list)
print(estimates_tbl)
# print( round(estimates_tbl$estimate[1:3],4) )

print(fit_list_to_df((fit_list)))

stop("as requested")

### Generate & fit repeatedly

n_ps <- 10000
alpha <- 1
tails <- 2
set_globals(fit_constrained = !TRUE)
# set_globals(optim_starting_parms = pcurveMix::make_optim_starting_parms_df())
for (i_typ in 1:20) {
  ps <- pcurveMix::random(n_ps, mu = 1, sigma = 2, pi = 0.3, alpha = alpha, tails = tails)
  fit_list <- fit_p_curve(ps, alpha = alpha, tails = tails)
  estimates_tbl <- fit_to_estimates_tbl(fit_list)
  print( round(estimates_tbl$estimate[1:3],4) )
}

stop("as requested")

### CHECK WHETHER MU IS ESTIMATED AS NEAR ZERO WHEN WE KNOW IT IS NOT

n_ps <- 5000
alpha <- 0.05
tails <- 2
n_boot_samples <- 50  # Smaller number for demo, but 2000 recommended minimum for real analyses; more is better
set_globals(fit_constrained = FALSE)
set_globals(optim_starting_parms = pcurveMix::make_optim_starting_parms_df())

ps <- pcurveMix::random(n_ps, mu = 1, sigma = 1, pi = 0.9, alpha = alpha, tails = tails)
fit_list <- fit_p_curve(ps, alpha = alpha, tails = tails)
estimates_tbl <- fit_to_estimates_tbl(fit_list)
print(estimates_tbl)

if (n_boot_samples > 0) {
  df <- bootstrap(n_ps, fit_list, n_boot_samples,
                  alpha = alpha, tails = tails, show_progress_bar = TRUE)
  boot_list <- make_bootstrap_summary_list(df, estimates_tbl)
  cat("* n bootstrap samples =",n_boot_samples,"\n")
  cat("* percent converged OK =",round(boot_list$pct_converged,2),"\n")
  print(boot_list$boot_tbl)
}

stop("as requested")

# TRYING TO GET START_PARMS DESCRIPTION

set_globals(optim_starting_parms = pcurveMix::make_optim_starting_parms_df())
ps <- pcurveMix::random(100, 2, 1)
print(fit_to_descriptor_tbl(fit_list))

stop("as requested")


###

file_name <- "C:/R/Projects/pcurveMix_pkg/pkg/inst/extdata/sample_ps.csv"

df <- read.csv(file_name)
sample_ps <- df$p
n_ps <- length(sample_ps)

print("*** Defaults")

alpha_cutoff <- 1 # allow full range of p's, not selected for p<alpha significance
tails <- 2
fit_results_list <- fit_p_curve(sample_ps, alpha = alpha_cutoff, tails = tails)

descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list)
print(descriptor_tbl)

estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
print(estimates_tbl)

n_boot_samples <- 100  # Smaller number for demo, but 2000 recommended minimum for real analyses; more is better
df <- bootstrap(n_ps, fit_results_list, n_boot_samples,
                alpha = alpha_cutoff, tails = tails, show_progress_bar = FALSE)
boot_list <- make_bootstrap_summary_list(df, estimates_tbl)
cat("* n bootstrap samples =",n_boot_samples,"\n")
cat("* percent converged OK =",round(boot_list$pct_converged,2),"\n")
print(boot_list$boot_tbl)

print("*** UNCONSTRAINED, ALPHA_CUTOFF = 0.05, 2 tails, CENSORED")

set_globals(optim_starting_parms = make_optim_starting_parms_df())
set_globals(small_p_bin_cutoff = 1e-6, fit_constrained = FALSE)
alpha_cutoff <- 0.05

fit_results_list <- fit_p_curve(sample_ps, alpha = alpha_cutoff, tails = tails)
descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list)
print(descriptor_tbl)
estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
print(estimates_tbl)

print("*** UNCONSTRAINED, ALPHA_CUTOFF = 0.05, 1 tails, CENSORED")

set_globals(optim_starting_parms = make_optim_starting_parms_df())
set_globals(small_p_bin_cutoff = 1e-6, fit_constrained = FALSE)
alpha_cutoff <- 0.05

fit_results_list <- fit_p_curve(sample_ps, alpha = 0.05, tails = 1)
descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list)
print(descriptor_tbl)
estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
print(estimates_tbl)

print("*** UNCONSTRAINED, ALPHA_CUTOFF = 0.05, 2 tails, NOT CENSORED")

set_globals(optim_starting_parms = make_optim_starting_parms_df())
set_globals(small_p_bin_cutoff = NULL, fit_constrained = FALSE)
alpha_cutoff <- 0.05

fit_results_list <- fit_p_curve(sample_ps, alpha = alpha_cutoff, tails = tails)
descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list)
print(descriptor_tbl)
estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
print(estimates_tbl)



stop("stop as requested")

#### Suggested by failed adjusted_ps_fit.Rmd:

# set_globals(optim_starting_parms = list(mu = 2, sigma = 2, pi = 0.5), fit_constrained = TRUE)  # NEWJEFF
set_globals(optim_starting_parms = make_optim_starting_parms_df())

alpha_cutoff <- 0.05
tails <- 2
fit_results_list <- pcurveMix::fit_p_curve(sample_ps, alpha = alpha_cutoff, tails = tails)
descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list)
print(descriptor_tbl)
estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
print(estimates_tbl)

boot_df <- bootstrap(n_ps, fit_results_list, n_boot_samples,
                     alpha = alpha_cutoff, tails = tails, show_progress_bar = FALSE)
boot_list <- make_bootstrap_summary_list(boot_df, estimates_tbl)
cat("* n bootstrap samples =",n_boot_samples,"\n")
cat("* percent converged OK =",round(boot_list$pct_converged,2),"\n")
print(boot_list$boot_tbl)


