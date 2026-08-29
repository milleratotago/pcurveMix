# for_profileCI.R

# Code for profile CIs to include in pcurveMix package.
# This code relies heavily on the 'profileCI' package from NEWJEFF.
# To avoid boundary problems, parameters are considered on the
# full -Inf to +Inf scale with respect to computations in that package,
# using the same 'parms_to_reals' and 'reals_to_parms' functions
# used with 'optim'.

# Notes:
# - profileCI control parameters are held in pcm_env environment
#   and can be changed; e.g., profile = FALSE is faster.

# Be sure to run devtools::document() so that these exports are
# recorded in NAMESPACE.

# This doesn't work here because variables in the package's namespace environment
#  cannot be changed at run-time. Variables in a separate internal environment can be
#  changed because R explicitly locks the main package namespace environment upon loading,
#  but it does not recursively lock child environments that you create inside it.
# # Define the class. This class belongs to the package's namespace environment
# #  and so can be referenced directly by name.
# # @export
# profCI_model <- structure(list(coefficients <- c(mu = 0, sigma = 0, pi = 0)),
#                          class = "profCI_model")

# Define S3 methods for coef and vcov for this class.
# These definitions must be _outside_ of a function so that they
# are global with the package's NAMESPACE.
#' @export
coef.profCI_model <- function(object, ...) object$coefficients
#' @export
vcov.profCI_model <- function(object, ...) {
  if (!is.null(object$vcov)) {
    return(object$vcov)
  } else {
    # Provide a placeholder variance-covariance matrix matching parameter names
    # print("vcov.profCI_model was called and returned mat:")  # NEWJEFF
    nparms <- length(object$coefficients)
    mat <- diag(rep(1,nparms))
    dimnames(mat) <- list(names(object$coefficients), names(object$coefficients))
    return(mat)
  }
}

# This is not needed because roxygen2 handles the registration automatically.
# # Register the class methods safely in R
# .S3method("coef", "profCI_model", coef.profCI_model)
# .S3method("vcov", "profCI_model", vcov.profCI_model)

#' Computations for profile-based confidence intervals of
#'  basic model parameters mu, sigma, and pi (NOT folder).
#' @param fit_list  NEWJEFF param from elsewhere
#' @param level NEWJEFF
#' @returns NEWJEFF
#' @export
compute_profileCI <- function(fit_list, level = 0.95) {  # NEWJEFF: This `level` not used
  # print("Start compute_profileCI")
  coefficients <- c(mu = 0, sigma = 0, pi = 0)
  profCI_model <- list(coefficients = coefficients)
  class(profCI_model) <- "profCI_model"

  ps <- fit_list$check_ps_list$ps_in_bounds
  parms_list <- list(mu = fit_list$mu, sigma = fit_list$sigma, pi = fit_list$pi)
  reals_list <- parms_to_reals(parms_list)
  profCI_model$coefficients["mu"] <- reals_list$mu
  profCI_model$coefficients["sigma"] <- reals_list$sigma
  profCI_model$coefficients["pi"] <- reals_list$pi
  # Build the list of arguments that will be passed to profileCI
  args1 <- list(object = profCI_model, loglik = pll_profileCI,
                ps = ps, alpha = fit_list$alpha, tails = fit_list$tails) # profileCI passes these to pll_profileCI()
  full_args <- c(args1, pcm_env$profileCI_args)  # append profileCI args in environment, default or set by user

  hold <- pcm_env$fit_constrained  # ensure this is false for profileCI because parms are reals
  pcm_env$fit_constrained <- FALSE
  # print("**************** Call profileCI:")
  profile <- do.call(profileCI::profileCI, full_args)
  # profile <- rlang::exec(profileCI::profileCI, !!!full_args)  # Splice and execute using the !!! operator
  pcm_env$fit_constrained <- hold

  # Create a labelled matrix with the bounds on the real scale
  bounds_matrix <- matrix(profile, nrow = nrow(profile), ncol = ncol(profile), dimnames = dimnames(profile))
  # Convert the real values to their natural scales:
  bounds_matrix["mu",] <- reals_to_mus(bounds_matrix["mu",])
  bounds_matrix["sigma",] <- reals_to_sigmas(bounds_matrix["sigma",])
  bounds_matrix["pi",] <- reals_to_pis(bounds_matrix["pi",])
  # If profiles were computed, convert their real values to their natural scales:
  profile_curves <- attr(profile,"for_plot")
  # print(profile_curves)
  if (!is.null(profile_curves)) {
    is_single_na <- function(x) { length(x) == 1 && is.na(x) }  # Helper fn used in next 3 lines
    if (!is_single_na(profile_curves$mu)) profile_curves$mu[,1] <- reals_to_mus(profile_curves$mu[,1])
    if (!is_single_na(profile_curves$sigma)) profile_curves$sigma[,1] <- reals_to_sigmas(profile_curves$sigma[,1])
    if (!is_single_na(profile_curves$pi)) profile_curves$pi[,1] <- reals_to_pis(profile_curves$pi[,1])
  }
  return( list(bounds_matrix = bounds_matrix, profile_curves = profile_curves, profile_fn_output = profile) )
}

# Positive log-likelihood function that is called by profileCI
pll_profileCI <- function(par, ps, alpha = 1, tails = 2) {
  # Make sure parameters are in the order expected by nll_optim
  # and then negate its result to get positive log-likelihood
  par2 <- c(par[3], par[1], par[2])
  nll <- nll_optim(par2, ps, alpha, tails)
  return( -nll )
}



#### START of Special routines for computing profileCIs of folded-normal parameters

# For r = mu/sigma, the folded mean and SD can be written as sigma times
# a function of r. These scale factors permit an exact reparameterization
# that is useful in computing profileCI's for the folded mu and sigmma.
folded_scale_factors <- function(r) {
  a <- sqrt(2 / pi) * exp(-r^2 / 2) + r * (1 - 2 * stats::pnorm(-r))
  b_squared <- pmax(0, r^2 + 1 - a^2)
  b <- sqrt(b_squared)
  c(mean = unname(a), sd = unname(b))  # These are scale factors; eg folded_mu = mean*sigma
}

# Input z is a vector(3):
#   z[1] = profileCI's candidate value of target, either folded_mu or folded_sigma
#   z[2] = original MLE pi
#   z[3] = ratio of original (MLE mu) / (MLE sigma)
# Output is vector of (pi, mu, sigma) named values
decode_for_folded <- function(z, target_folded_mean) {
  if (length(z) != 3L || any(!is.finite(z))) {
    return(c(pi = NA_real_, mu = NA_real_, sigma = NA_real_))
  }
  folded_value <- exp(z[1])
  pi_value <- stats::plogis(z[2])
  ratio <- exp(z[3])
  factors <- folded_scale_factors(ratio)
  scale_factor <- if (target_folded_mean) {
    unname(factors["mean"])
  } else {
    unname(factors["sd"])
  }
  if (!is.finite(scale_factor) || scale_factor <= 1e-10) {
    return(c(pi = NA_real_, mu = NA_real_, sigma = NA_real_))
  }
  sigma <- folded_value / scale_factor
  mu <- ratio * sigma
  c(pi = unname(pi_value), mu = unname(mu), sigma = unname(sigma))
}

profile_loglik_for_folded <- function(z, p_values, alpha, tails, target_folded_mean) {
  pars <- decode_for_folded(z, target_folded_mean = target_folded_mean)
  if (any(!is.finite(pars)) || pars["pi"] <= 0 || pars["pi"] >= 1 ||
      pars["mu"] <= 0 || pars["sigma"] <= 0 ||
      pars["mu"] > 1e4 || pars["sigma"] > 1e4) return(-Inf)

  # NEWJEFF: No censoring here
  density <- pcurveMix::pdf(
    p = p_values,
    pi = pars["pi"],
    mu = pars["mu"],
    sigma = pars["sigma"],
    alpha = alpha,
    tails = tails
  )
  if (any(!is.finite(density)) || any(density <= 0)) return(-Inf)
  sum(log(density))
}

compute_profileCI_folded <- function(fit_list, target_folded_mean, level = 0.95) {

  alpha <- fit_list$alpha
  tails <- fit_list$tails
  p_values <- fit_list$check_ps_list$ps_in_bounds
  mu_hat <- fit_list$mu
  sigma_hat <- fit_list$sigma
  pi_hat <- fit_list$pi
  ratio_hat <- mu_hat / sigma_hat
  folded_hat <- folded_moments(mu_hat, sigma_hat)
  if (target_folded_mean) {
    target_hat <- unname(folded_hat["mean"])
    target_name <- "log_folded_mean"
  } else {
    target_hat <- unname(folded_hat["sd"])
    target_name <- "log_folded_sd"
  }

  # Reparameterized vector:
  # z[1] = log(folded target), z[2] = logit(pi), z[3] = log(mu/sigma).
  coefficients <- c(
    target = log(target_hat),
    logit_pi = stats::qlogis(pi_hat),
    log_ratio = log(ratio_hat)
  )
  names(coefficients)[1] <- target_name

  hessian <- stats::optimHess(
    coefficients,
    function(z) -profile_loglik_for_folded(
      z, p_values, alpha, tails, target_folded_mean)  )
  vcov_hat <- tryCatch(solve(hessian), error = function(e) NULL)

  if (is.null(vcov_hat) || any(!is.finite(vcov_hat)) ||
      any(diag(vcov_hat) <= 0)) {
    hessian_symmetric <- (hessian + t(hessian)) / 2
    eig <- eigen(hessian_symmetric, symmetric = TRUE)
    tolerance <- max(max(abs(eig$values)), 1) * 1e-8
    safe_values <- pmax(eig$values, tolerance)
    vcov_hat <- eig$vectors %*% diag(1 / safe_values) %*% t(eig$vectors)
  }
  dimnames(vcov_hat) <- list(names(coefficients), names(coefficients))

  profile_object <- structure(
    list(coefficients = coefficients, vcov = vcov_hat),
    class = "profCI_model"
  )

  lower_search <- log(1e-6)
  upper_search <- log(100)

  profile_result <- profileCI::profileCI(
    object = profile_object,
    loglik = profile_loglik_for_folded,
      p_values = p_values,  # passed to loglik fn
      alpha = alpha,  # passed to loglik fn
      tails = tails,  # passed to loglik fn
      target_folded_mean = target_folded_mean,  # passed to loglik fn
    parm = target_name,  # specifies which parameter to compute profile for (default = "all")
    level = level,
    faster = FALSE,
    mult = 16,
    epsilon = 1e-4,
    lb = lower_search,
    ub = upper_search,
    optim_args = list(
      method = "BFGS",
      control = list(maxit = 3000)
    )
  )

  ci_log <- as.numeric(profile_result[target_name, 1:2])
  ci_natural <- exp(ci_log)

  status <- if (all(is.finite(ci_natural))) {
    "OK"
  } else if (is.infinite(ci_natural[2])) {
    "Upper confidence limit is unbounded"
  } else {
    "Limit not found within the numerical search range"
  }

  result_table <- data.frame(
    n = length(p_values),
    Estimate = target_hat,
    `95% CI lower` = ci_natural[1],
    `95% CI upper` = ci_natural[2],
    Status = status,
    check.names = FALSE
  )

  result <- list(
    table = result_table,
    profile = profile_result,
    estimate = target_hat,
    ci = ci_natural,
    target_name = target_name
  )
  #}

} # do_1_profileCI_folded

#### END of Special routines for computing profileCIs of folded-normal parameters


#### START of Special routines for computing profileCIs of power

# Mean rejection probability after integrating over
# Delta ~ N(mu, sigma^2) under the alternative component.
average_power <- function(mu, sigma, alpha_sig, tails) {
  z_sd <- sqrt(1 + sigma^2)

  if (tails == 2) {
    critical_z <- stats::qnorm(1 - alpha_sig / 2)
    stats::pnorm(-critical_z, mean = mu, sd = z_sd) +
      stats::pnorm(critical_z, mean = mu, sd = z_sd, lower.tail = FALSE)
  } else {
    critical_z <- stats::qnorm(1 - alpha_sig)
    stats::pnorm(critical_z, mean = mu, sd = z_sd, lower.tail = FALSE)
  }
}

# Convert z vector on real scale to pi, mu, sigma on
# their natural scales.
decode_for_power <- function(z, alpha_sig, tails) {
  if (length(z) != 3L || any(!is.finite(z))) {
    return(c(pi = NA_real_, mu = NA_real_, sigma = NA_real_))
  }
  power <- alpha_sig + (1 - alpha_sig) * stats::plogis(z[1])
  pi_value <- stats::plogis(z[2])
  mu_sigma <- parameters_from_power(power, z[3], alpha_sig, tails)
  c(
    pi = unname(pi_value),
    mu = unname(mu_sigma["mu"]),
    sigma = unname(mu_sigma["sigma"])
  )
}

# Recover mu and sigma from a specified power and r = mu/sigma.
parameters_from_power <- function(power, log_ratio, alpha_sig, tails) {
  ratio <- unname(exp(log_ratio))

  equation <- function(log_sigma) {
    sigma <- exp(log_sigma)
    average_power(ratio * sigma, sigma, alpha_sig, tails) - power
  }

  search_interval <- c(log(1e-8), log(1e6))
  endpoints <- equation(search_interval)

  if (any(!is.finite(endpoints)) || prod(sign(endpoints)) > 0) {
    return(c(mu = NA_real_, sigma = NA_real_))
  }

  root <- tryCatch(
    stats::uniroot(equation, interval = search_interval, tol = 1e-10)$root,
    error = function(e) NA_real_
  )

  if (!is.finite(root)) {
    return(c(mu = NA_real_, sigma = NA_real_))
  }

  sigma <- exp(root)
  c(mu = unname(ratio * sigma), sigma = unname(sigma))
}

profile_ci_power <- function(p_values, alpha, alpha_sig = 0.05,
                             tails = 2, level = 0.95) {

  p_values <- p_values[
    is.finite(p_values) & p_values >= 0 & p_values <= alpha
  ]
  p_values[p_values == 0] <- 1e-15

  if (length(p_values) == 0L) {
    stop("No valid p-values remain after filtering.")
  }

  # Original parameterization for the unrestricted MLE:
  # eta[1] = log(mu), eta[2] = log(sigma), eta[3] = logit(pi).
  original_loglik <- function(eta) {
    if (length(eta) != 3L || any(!is.finite(eta))) return(-Inf)

    mu <- exp(eta[1])
    sigma <- exp(eta[2])
    pi_value <- stats::plogis(eta[3])

    density <- pcurveMix::pdf(
      p = p_values,
      pi = pi_value,
      mu = mu,
      sigma = sigma,
      alpha = alpha,
      tails = tails
    )

    if (any(!is.finite(density)) || any(density <= 0)) return(-Inf)
    sum(log(density))
  }

  # Use 3 x 3 x 3 = 27 starting values for the unrestricted MLE.
  starts <- expand.grid(
    mu = c(1, 2, 3),
    sigma = c(0.5, 1, 2),
    pi = c(0.2, 0.5, 0.8)
  )

  fits <- lapply(seq_len(nrow(starts)), function(i) {
    start <- c(
      log(starts$mu[i]),
      log(starts$sigma[i]),
      stats::qlogis(starts$pi[i])
    )

    tryCatch(
      stats::optim(
        par = start,
        fn = function(eta) -original_loglik(eta),
        method = "BFGS",
        control = list(maxit = 2000, reltol = 1e-10)
      ),
      error = function(e) list(value = Inf, par = start, convergence = 1)
    )
  })

  objective_values <- vapply(fits, function(x) x$value, numeric(1))
  if (all(!is.finite(objective_values))) {
    stop("The unrestricted maximum-likelihood fit failed.")
  }

  best <- fits[[which.min(objective_values)]]
  original_hat <- unname(best$par)
  mu_hat <- exp(original_hat[1])
  sigma_hat <- exp(original_hat[2])
  pi_hat <- stats::plogis(original_hat[3])
  ratio_hat <- mu_hat / sigma_hat
  power_hat <- unname(average_power(
    mu_hat, sigma_hat, alpha_sig, tails
  ))

  # Power is constrained to [alpha_sig, 1]. The relative position in this
  # interval is mapped to the real line using a logit transformation.
  relative_power_hat <- (power_hat - alpha_sig) / (1 - alpha_sig)
  relative_power_hat <- min(max(relative_power_hat, 1e-10), 1 - 1e-10)

  coefficients <- c(
    logit_relative_power = stats::qlogis(relative_power_hat),
    logit_pi = stats::qlogis(pi_hat),
    log_ratio = log(ratio_hat)
  )

  profile_loglik_power <- function(z) {
    pars <- decode_for_power(z, alpha_sig, tails)
    if (any(!is.finite(pars)) || pars["pi"] <= 0 || pars["pi"] >= 1 ||
        pars["mu"] <= 0 || pars["sigma"] <= 0 ||
        pars["mu"] > 1e6 || pars["sigma"] > 1e6) return(-Inf)

    density <- pcurveMix::pdf(
      p = p_values,
      pi = pars["pi"],
      mu = pars["mu"],
      sigma = pars["sigma"],
      alpha = alpha,
      tails = tails
    )
    if (any(!is.finite(density)) || any(density <= 0)) return(-Inf)
    sum(log(density))
  }

  hessian <- stats::optimHess(coefficients, function(z) -profile_loglik_power(z))
  vcov_hat <- tryCatch(solve(hessian), error = function(e) NULL)

  if (is.null(vcov_hat) || any(!is.finite(vcov_hat)) ||
      any(diag(vcov_hat) <= 0)) {
    hessian_symmetric <- (hessian + t(hessian)) / 2
    eig <- eigen(hessian_symmetric, symmetric = TRUE)
    tolerance <- max(max(abs(eig$values)), 1) * 1e-8
    safe_values <- pmax(eig$values, tolerance)
    vcov_hat <- eig$vectors %*% diag(1 / safe_values) %*% t(eig$vectors)
  }
  dimnames(vcov_hat) <- list(names(coefficients), names(coefficients))

  profile_object <- structure(
    list(coefficients = coefficients, vcov = vcov_hat),
    class = "profCI_model"
  )

  lower_search <- stats::qlogis(1e-6)
  upper_search <- stats::qlogis(1 - 1e-6)

  profile_result <- profileCI::profileCI(
    object = profile_object,
    loglik = profile_loglik_power,
    parm = "logit_relative_power",
    level = level,
    faster = FALSE,
    mult = 16,
    epsilon = 1e-4,
    lb = lower_search,
    ub = upper_search,
    optim_args = list(
      method = "BFGS",
      control = list(maxit = 3000)
    )
  )

  ci_transformed <- as.numeric(
    profile_result["logit_relative_power", 1:2]
  )

  # Verify whether a missing limit means that the confidence set reaches the
  # natural power boundary. Nuisance parameters are optimized from several
  # starting values for this explicit boundary check.
  profile_at_boundary <- function(fixed_power_parameter) {
    nuisance_starts <- rbind(
      unname(coefficients[c("logit_pi", "log_ratio")]),
      as.matrix(expand.grid(
        logit_pi = stats::qlogis(c(0.2, 0.5, 0.8)),
        log_ratio = log(c(0.1, 0.25, 0.5, 1, 2, 4))
      ))
    )

    boundary_fits <- lapply(seq_len(nrow(nuisance_starts)), function(i) {
      tryCatch(
        stats::optim(
          par = unname(nuisance_starts[i, ]),
          fn = function(nuisance) {
            -profile_loglik_power(c(fixed_power_parameter, nuisance))
          },
          method = "BFGS",
          control = list(maxit = 3000)
        ),
        error = function(e) list(value = Inf)
      )
    })

    values <- vapply(boundary_fits, function(x) x$value, numeric(1))
    if (all(!is.finite(values))) return(NA_real_)
    -min(values, na.rm = TRUE)
  }

  boundary_included <- c(lower = FALSE, upper = FALSE)
  likelihood_cutoff <- attr(profile_result, "crit")

  if (is.na(ci_transformed[1])) {
    boundary_loglik <- profile_at_boundary(lower_search)
    if (is.finite(boundary_loglik) &&
        boundary_loglik >= likelihood_cutoff - 1e-6) {
      ci_transformed[1] <- -Inf
      boundary_included["lower"] <- TRUE
    }
  }

  if (is.na(ci_transformed[2])) {
    boundary_loglik <- profile_at_boundary(upper_search)
    if (is.finite(boundary_loglik) &&
        boundary_loglik >= likelihood_cutoff - 1e-6) {
      ci_transformed[2] <- Inf
      boundary_included["upper"] <- TRUE
    }
  }

  back_transform <- function(z) {
    alpha_sig + (1 - alpha_sig) * stats::plogis(z)
  }
  ci_power <- back_transform(ci_transformed)

  status <- if (all(is.finite(ci_power))) {
    if (all(boundary_included)) {
      "Both power boundaries included"
    } else if (boundary_included["lower"]) {
      paste0("Lower power boundary set to ", alpha_sig)
    } else if (boundary_included["upper"]) {
      "Upper power boundary set to 1"
    } else {
      "OK"
    }
  } else {
    "Limit not found within the numerical search range"
  }

  result_table <- data.frame(
    n = length(p_values),
    Estimate = power_hat,
    `95% CI lower` = ci_power[1],
    `95% CI upper` = ci_power[2],
    Status = status,
    check.names = FALSE
  )

  list(
    table = result_table,
    profile = profile_result,
    estimate = power_hat,
    ci = ci_power,
    alpha_sig = alpha_sig
  )
}

compute_profileCI_power <- function(fit_list, level = 0.95) {
  alpha <- fit_list$alpha
  tails <- fit_list$tails
  p_values <- fit_list$check_ps_list$ps_in_bounds
  mu_hat <- fit_list$mu
  sigma_hat <- fit_list$sigma
  pi_hat <- fit_list$pi
  alpha_sig <- fit_list$alpha_sig
  result <- profile_ci_power(p_values, alpha, alpha_sig = alpha_sig,
                               tails = tails, level = level)
  return(result)
}

#### END of Special routines for computing profileCIs of power


