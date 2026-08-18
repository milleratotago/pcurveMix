# dev_misc.R

### 2026-08-18 developing profileCI

# library(pcurveMix)  # Must initialize globals
devtools::load_all(".")

alpha <- 1
ps <- pcurveMix::random(n = 200, mu = 3, sigma = 1, pi = 0.5, alpha = alpha)
fit_list <- pcurveMix::fit_p_curve(ps, alpha = alpha)

# Early attempts to run profileCI "clean" without package
# coefficients <- c(mu = 0, sigma = 0, pi = 0)
# profCI_model <- list(coefficients = coefficients)
# class(profCI_model) <- "profCI_model"
# coef.profCI_model <- function(object, ...) object$coefficients
# vcov.profCI_model <- function(object, ...) {
#   # Provide a placeholder variance-covariance matrix matching parameter names
#   mat <- diag(rep(1,length(object$coefficients)))
#   dimnames(mat) <- list(names(object$coefficients), names(object$coefficients))
#   return(mat)
# }
# # Register them safely in R
# .S3method("coef", "profCI_model", coef.profCI_model)
# .S3method("vcov", "profCI_model", vcov.profCI_model)
# profCI_model$coefficients["mu"] <- mus_to_reals(fit_list$mu)
# profCI_model$coefficients["sigma"] <- sigmas_to_reals(fit_list$sigma)
# profCI_model$coefficients["pi"] <- pis_to_reals(fit_list$pi)
# test_plain <- profileCI::profileCI(profCI_model, loglik = pcurveMix:::pll_profileCI,
#                                    p = ps, alpha = fit_list$alpha, tails = fit_list$tails,
#                                    parm = "all", profile = TRUE, mult = 1.1, faster = FALSE, flat = 1e-08,
#                                    lb = rep(-20,3), ub = rep(20,3))

test_ci <- compute_profileCI(fit_list)
plot(test_ci$profile_curves$pi[,1],test_ci$profile_curves$pi[,2])
plot(test_ci$profile_curves$mu[,1],test_ci$profile_curves$mu[,2])
plot(test_ci$profile_curves$sigma[,1],test_ci$profile_curves$sigma[,2])

stop("stopped as requested")




### 2026-08-18 generate a fit_list to see what is in it.
alpha <- 1
ps <- pcurveMix::random(n = 200, mu = 3, sigma = 1, pi = 0.5, alpha = alpha)
fit_list <- pcurveMix::fit_p_curve(ps, alpha = alpha)
# > print(fit_list)
# $alpha [1] 1
# $alpha_sig [1] 0.05
# $tails [1] 2
# $pi [1] 0.4356507
# $mu [1] 3.283727
# $sigma [1] 0.895528
# $start$mu [1] 2
# $start$sigma [1] 2
# $start$pi [1] 0.5
# $se
# pi         mu      sigma
# 0.05550688 0.26206801 0.27319598
# $ci95
# lwr95    upr95
# pi    0.3315179 0.545787
# mu    2.8082407 3.839723
# sigma 0.4924994 1.628368
# $logLik [1] 331.0918
# $converged [1] TRUE
# $power_hat [1] 0.8380135
# $ks  Asymptotic one-sample Kolmogorov-Smirnov test
# data:  p2
# D = 0.069616, p-value = 0.287
# alternative hypothesis: two-sided
# $n [1] 200
# $min_p [1] 4.621525e-11
# $max_p [1] 0.9963029
# $check_ps_list$all_in_bounds [1] TRUE
# $check_ps_list$alpha_cutoff [1] 1
# $check_ps_list$n_too_small [1] 0
# $check_ps_list$n_equal_zero [1] 0
# $check_ps_list$n_too_large [1] 0
# $check_ps_list$ps_too_small numeric(0)
# $check_ps_list$ps_too_large numeric(0)
# $check_ps_list$ps_in_bounds   [1] 3.409856e-02 4.546709e-01 2.424460e-07 5.328215e-02 4.198367e-01 6.710021e-01 6.072424e-01
# $start_parm_set [1] NA
# $noncentrality_mean [1] 3.283781
# $noncentrality_sd [1] 0.8953326



##### Development script to explore nc | H0 ~ N(0, sigma_0)
#####  instead of nc | H0 = 0.

### Compare simulated with predicted p's ####

# Use brute-force simulation to get p's under H0 with
# the indicated sigma_0 & make histogram of them.
sigma_0 <- 1.2
n_ps <- 100000
nc0s <- rnorm(n_ps, sd = sigma_0)
z0s <- rnorm(n_ps, mean = nc0s)
print( paste(mean(z0s), sd(z0s)) )
pred_sd <- sqrt(1 + sigma_0^2)
print( paste("Predicted sd =",pred_sd) )
ps <- rep(NA,n_ps)  # make vector
pos_z0s <- z0s > 0
# 2-tailed computations
ps[pos_z0s] <- 2 * pnorm(z0s[pos_z0s], lower.tail = FALSE)
ps[!pos_z0s] <- 2 * pnorm(z0s[!pos_z0s], lower.tail = TRUE)
hist(ps, freq = FALSE)

# Get the pdf under H0 analytically with pcurveMix::pdf by
# pretending mu = 0, sigma = sigma_0, pi = 1.
# This is equivalent as I expected.
p_range <- seq(0.005, 0.995, by = 0.01)
pdfs <- pcurveMix::pdf(p_range, mu = 0.00001, sigma = sigma_0)
lines(p_range,pdfs)
