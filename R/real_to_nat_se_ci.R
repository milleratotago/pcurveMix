# real_to_nat_se_ci.R

# Function to compute the standard errors and confidence intervals
#  for the model parameters on their natural scales from the
#  estimates and Hessian returned by optim() when fitting on
#  the unconstrained real scales.
# par_hat is the vector of real-scale parameter estimates
#  returned by optim
# H is the hessian matrix on the real scale returned by optim
#  when fitting is unconstrained (full real parameter ranges).
#  The first row/col of H corresponds to pi, then mu, then sigma
# returns list with elements se & ci
real_to_nat_se_ci <- function(par_hat, H) {

  # print(par_hat)  # NEWJEFF: Lots of commented-out prints to delete in this function
  names(par_hat) <- c("logit_pi", "log_mu", "log_sigma")

  # Natural-scale parameter estimates.
  mu_hat    <- exp(par_hat["log_mu"])
  sigma_hat <- exp(par_hat["log_sigma"])
  pi_hat    <- stats::plogis(par_hat["logit_pi"])
  # print(pi_hat)

  # Apply numerical safety clipping to pi.
  pi_hat <- pmin(pmax(pi_hat, 1e-10), 1 - 1e-10)

  # print("H")
  # print(H)
  #
  # Compute variance-covariance matrix on the transformed scale.
  # This may fail if the Hessian is singular or numerically unstable.
  vcov_par <- tryCatch(
    solve(H),
    error = function(e) NULL
  )

  # print("vcov_par")
  # print(vcov_par)
  #
  # Extract standard errors on the transformed scale.
  if (is.null(vcov_par) ||
      any(!is.finite(vcov_par)) ||
      any(diag(vcov_par) <= 0)) {

    se_par <- c(
      se_log_mu = NA_real_,
      se_log_sigma = NA_real_,
      se_logit_pi = NA_real_
    )

  } else {

    se_par <- sqrt(diag(vcov_par))
    names(se_par) <- c("se_logit_pi", "se_log_mu", "se_log_sigma")
  }


  #------------------------------------------------------------
  # Delta-method standard errors on the natural scale
  #------------------------------------------------------------

  # These standard errors are reported on the natural scale.
  #
  # For mu = exp(log_mu):
  # d mu / d log_mu = mu
  se_mu <- mu_hat * se_par["se_log_mu"]

  # For sigma = exp(log_sigma):
  # d sigma / d log_sigma = sigma
  se_sigma <- sigma_hat * se_par["se_log_sigma"]

  # For pi = logistic(logit_pi):
  # d pi / d logit_pi = pi * (1 - pi)
  se_pi <- pi_hat * (1 - pi_hat) * se_par["se_logit_pi"]

  se <- c(se_pi, se_mu, se_sigma)
  # print("NEWJEFF 22")
  # print(se)

  #------------------------------------------------------------
  # Confidence intervals on the transformed scale
  # and back-transformation to the natural scale
  #------------------------------------------------------------

  # Confidence intervals are first computed on the transformed scale.
  # This is useful because the transformed parameters are unconstrained.
  #
  # The CI limits are then back-transformed:
  # log(mu)     -> mu
  # log(sigma)  -> sigma
  # stats::plogis(pi)   -> pi
  #
  # As a result, the CIs for mu, sigma, and pi are generally asymmetric.
  # They also automatically respect the parameter boundaries.

  z <- stats::qnorm(0.975)

  # CI for log(mu).
  ci_log_mu <- c(
    lower = unname(par_hat["log_mu"] - z * se_par["se_log_mu"]),
    upper = unname(par_hat["log_mu"] + z * se_par["se_log_mu"])
  )

  # CI for log(sigma).
  ci_log_sigma <- c(
    lower = unname(par_hat["log_sigma"] - z * se_par["se_log_sigma"]),
    upper = unname(par_hat["log_sigma"] + z * se_par["se_log_sigma"])
  )

  # CI for logit(pi).
  ci_logit_pi <- c(
    lower = unname(par_hat["logit_pi"] - z * se_par["se_logit_pi"]),
    upper = unname(par_hat["logit_pi"] + z * se_par["se_logit_pi"])
  )

  # Back-transform CI limits to the natural scale.
  ci_mu    <- exp(ci_log_mu)
  ci_sigma <- exp(ci_log_sigma)
  ci_pi    <- stats::plogis(ci_logit_pi)

  ci <- rbind(ci_pi, ci_mu, ci_sigma)
  rownames(ci) <- c("pi","mu","sigma"); colnames(ci) <- c("lwr95","upr95")

  names(se) <- c("pi", "mu", "sigma")
  # print("NEWJEFF 23")
  # print(se)

  l <- list(se = se, ci = ci)
  return(l)
}
