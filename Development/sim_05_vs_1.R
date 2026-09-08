# sim_05_vs_1.R

devtools::load_all(".")

n_subsamples <- 500
n_per_subsample <- 100

# Example
pi <- 0.3
mu <- 2
sigma <- 2.5

# Significant ps:
alpha <- 0.05
sig_ps_mat <- generate_parametric_subsamples(n_subsamples, n_per_subsample,
                                             mu, sigma, pi, alpha = alpha)
sig_ps_fits <- fits_for_matrix(sig_ps_mat, alpha = alpha)
sig_summaries <- get_parm_summaries(sig_ps_fits)
print(sig_summaries$value[sig_summaries$parm == "power" & sig_summaries$summary == "mean"])

alpha <- 1
nsig_ps_mat <- generate_parametric_subsamples(n_subsamples, n_per_subsample,
                                             mu, sigma, pi, alpha = alpha)
nsig_ps_fits <- fits_for_matrix(nsig_ps_mat, alpha = alpha)
nsig_summaries <- get_parm_summaries(nsig_ps_fits)
print(nsig_summaries$value[nsig_summaries$parm == "power" & nsig_summaries$summary == "mean"])
