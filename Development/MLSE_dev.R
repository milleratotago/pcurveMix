# MLSE_dev.R

# library(pcurveMix)

# Adjusted values for comparison with Rolf's check_and_suggestions.qmd

# Values from Rolf
set.seed(123)
# eps <- 0.00001; pcurveMix::set_globals(small_p_bin_cutoff = eps)  # NEWJEFF: small_p_bin_cutoff NOT WORKING
nps <- 8000
mu_true <- 2
sigma_true <- 2
pi_true <- 0.5
alpha = 1
tails = 2
rand_ps <- pcurveMix::random(nps, mu_true, sigma_true, pi = pi_true, alpha = alpha, tails = tails)
hist(rand_ps)

# set_globals(fit_constrained = TRUE)
# fit_list <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
# print( fit_list_to_df(fit_list) )
# l <- pcurveMix:::pcm_MLSE(rand_ps, fit_list$mu, fit_list$sigma, fit_list$pi, alpha, tails)
# print(l)

print("# Constrained fit with no small p binning")
set_globals(fit_constrained = TRUE, small_p_bin_cutoff = NULL)
fit_list_con <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
print(fit_list_to_df(fit_list_con))

print("------------------")
print("# Unconstrained fit with no small p binning")
set_globals(fit_constrained = FALSE, small_p_bin_cutoff = NULL)
fit_list_uncon <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
print(fit_list_to_df(fit_list_uncon))

try_eps <- 1e-15
print( paste(sum(rand_ps < try_eps),"below small_p_bin_cutoff") )

print("------------------")
print("# Constrained fit with yes small p binning")
set_globals(fit_constrained = TRUE, small_p_bin_cutoff = try_eps)
fit_list_con <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
print(fit_list_to_df(fit_list_con))

print("------------------")
print("# Unconstrained fit with yes small p binning")
# NEWJEFF: The following gives MLSE$SE Nans with the default MLSEh = 1e-7.
# It gives values that look far too large with MLSEh = 1e-6 and values
# that look quite good with MLSEh = 1e-8
set_globals(fit_constrained = FALSE, small_p_bin_cutoff = try_eps, MLSEh = 1e-8)
fit_list_uncon <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
print(fit_list_to_df(fit_list_uncon))

# Some old test code:
# nll1 <- nll(rand_ps, mu = 2.476798, sigma = 1.702053, pi = 0.4318923, alpha = 1, tails = 2,
#             small_p_bin_cutoff = NULL)
# try_eps <- 1e-15
# print( paste(sum(rand_ps < try_eps),"below small_p_bin_cutoff") )
# nll2 <- nll(rand_ps, mu = 2.476798, sigma = 1.702053, pi = 0.4318923, alpha = 1, tails = 2,
#             small_p_bin_cutoff = try_eps)
# # nll3 <- nll(rand_ps, mu = 2.476798, sigma = 1.702053, pi = 0.4318923, alpha = 1, tails = 2)
# pcurveMix:::pcm_env$small_p_bin_cutoff

print("------------------")
print("# Fitting with multipe starting points")
#### Develop fit_p_curve_many
# mu_vec <- 1:2
# sigma_vec <- 1:2
# pi_vec <- c(0.25, 0.75)
# start_df <- expand.grid(mu_vec, sigma_vec, pi_vec)
# colnames(start_df) <- c("mu", "sigma", "pi")
start_df <- set_optim_starting_parms_df(pi = NA)
set_globals(fit_constrained = TRUE, small_p_bin_cutoff = NULL, optim_starting_parms = start_df)
fit_best <- fit_p_curve(rand_ps, alpha = alpha, tails = tails)
print(fit_list_to_df(fit_best))
