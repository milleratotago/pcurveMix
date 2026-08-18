# for_profileCI.R

# Development of code for profile CIs to include in pcurveMix package.
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

# Define S3 methods for coef and vcov for this class
#' @export
coef.profCI_model <- function(object, ...) object$coefficients
#' @export
vcov.profCI_model <- function(object, ...) {
  # Provide a placeholder variance-covariance matrix matching parameter names
  # print("vcov.profCI_model was called and returned mat:")  # NEWJEFF
  nparms <- length(object$coefficients)
  mat <- diag(rep(1,nparms))
  dimnames(mat) <- list(names(object$coefficients), names(object$coefficients))
  # print(mat)  # NEWJEFF
  return(mat)
}

# This is not needed because roxygen2 handles the registration automatically.
# # Register the class methods safely in R
# .S3method("coef", "profCI_model", coef.profCI_model)
# .S3method("vcov", "profCI_model", vcov.profCI_model)

# # OBSOLETE
# #' Computations for profile-based confidence interval
# #' @param fit_list  NEWJEFF param from elsewhere
# #' @returns NEWJEFF
# #' @importFrom rlang !!!
# #' @importFrom rlang exec
# #' @export
# compute_profileCI_0 <- function(fit_list) {
#   ps <- fit_list$check_ps_list$ps_in_bounds
#   parms_list <- list(mu = fit_list$mu, sigma = fit_list$sigma, pi = fit_list$pi)
#   reals_list <- parms_to_reals(parms_list)
# # print("Start compute_profileCI")
# # print(parms_list)  # NEWJEFF
# # print(reals_list)
# # readline(prompt="Press [enter] to continue")
#   pcm_env$profCI_model$coefficients["mu"] <- reals_list$mu
#   pcm_env$profCI_model$coefficients["sigma"] <- reals_list$sigma
#   pcm_env$profCI_model$coefficients["pi"] <- reals_list$pi
#   args1 <- list(object = pcm_env$profCI_model, loglik = pll_profileCI,
#                 p = ps, alpha = fit_list$alpha, tails = fit_list$tails)
#   full_args <- c(args1, pcm_env$profileCI_args)
# print(full_args)  # NEWJEFF
# readline(prompt="Press [enter] to continue")
# 
#   hold <- pcm_env$fit_constrained  # ensure this is false for profileCI because parms are reals
#   pcm_env$fit_constrained <- FALSE
#   # profile <- do.call(profileCI::profileCI, full_args)
#   # profile <- rlang::exec(profileCI::profileCI, !!!full_args)  # Splice and execute using the !!! operator
#   # NEWJEFF: Test with simplified call
# print("**************** Call profileCI:")
#   profile <- profileCI::profileCI(pcm_env$profCI_model, loglik = pll_profileCI,
#                 p = ps, alpha = fit_list$alpha, tails = fit_list$tails,
#                 parm = "all", profile = TRUE, mult = 1.1, faster = FALSE, flat = 1e-08,
#                                  lb = rep(-200,3), ub = rep(200,3))
#   pcm_env$fit_constrained <- hold
# 
#   # Create a labelled matrix with the bounds on the real scale
#   bounds_matrix <- matrix(profile, nrow = nrow(profile), ncol = ncol(profile), dimnames = dimnames(profile))
#   # Convert the real values to their natural scales:
#   bounds_matrix["mu",] <- reals_to_mus(bounds_matrix["mu",])
#   bounds_matrix["sigma",] <- reals_to_sigmas(bounds_matrix["sigma",])
#   bounds_matrix["pi",] <- reals_to_pis(bounds_matrix["pi",])
#   # # JEFF: reals_to_parms for CIs & for_plot$parameter$parameter_values
#   # reals_list_lb <- list(mu = profile["mu",1], sigma = profile["sigma",1], pi = profile["pi",1])
#   # reals_list_ub <- list(mu = profile["mu",2], sigma = profile["sigma",2], pi = profile["pi",2])
#   # parms_list_lb <- reals_to_parms(reals_list_lb)
#   # parms_list_ub <- reals_to_parms(reals_list_ub)
# 
#   # If profiles were computed for the real values, convert those
#   # to their natural scales:
#   profile_curves <- attr(profile,"for_plot")
#   # print(profile_curves)  #NEWJEFF
#   if (!is.na(profile_curves$mu)) profile_curves$mu[,1] <- reals_to_mus(profile_curves$mu[,1])
#   if (!is.na(profile_curves$sigma)) profile_curves$sigma[,1] <- reals_to_sigmas(profile_curves$sigma[,1])
#   if (!is.na(profile_curves$pi)) profile_curves$pi[,1] <- reals_to_pis(profile_curves$pi[,1])
# 
#   return( list(bounds_matrix = bounds_matrix, profile_curves = profile_curves, profile_fn_output = profile) )
# }

#' Computations for profile-based confidence interval
#' @param fit_list  NEWJEFF param from elsewhere
#' @returns NEWJEFF
#' @importFrom rlang !!!
#' @importFrom rlang exec
#' @export
compute_profileCI <- function(fit_list) {
# print("Start compute_profileCI")
  coefficients <- c(mu = 0, sigma = 0, pi = 0)
  profCI_model <- list(coefficients = coefficients)
  class(profCI_model) <- "profCI_model"
#  coef.profCI_model <- function(object, ...) object$coefficients
#  vcov.profCI_model <- function(object, ...) {
#    # Provide a placeholder variance-covariance matrix matching parameter names
#    mat <- diag(rep(1,nparms))
#    dimnames(mat) <- list(names(object$coefficients), names(object$coefficients))
#    return(mat)
#  }
#  # Register them safely in R
#  .S3method("coef", "profCI_model", coef.profCI_model)
#  .S3method("vcov", "profCI_model", vcov.profCI_model)

  ps <- fit_list$check_ps_list$ps_in_bounds
  parms_list <- list(mu = fit_list$mu, sigma = fit_list$sigma, pi = fit_list$pi)
  reals_list <- parms_to_reals(parms_list)
# print(parms_list)  # NEWJEFF
# print(reals_list)
# readline(prompt="Press [enter] to continue")
  profCI_model$coefficients["mu"] <- reals_list$mu
  profCI_model$coefficients["sigma"] <- reals_list$sigma
  profCI_model$coefficients["pi"] <- reals_list$pi
#  args1 <- list(object = profCI_model, loglik = pll_profileCI,
#                p = ps, alpha = fit_list$alpha, tails = fit_list$tails)
#  full_args <- c(args1, pcm_env$profileCI_args)
#print(full_args)  # NEWJEFF
#readline(prompt="Press [enter] to continue")

  hold <- pcm_env$fit_constrained  # ensure this is false for profileCI because parms are reals
  pcm_env$fit_constrained <- FALSE
  # profile <- do.call(profileCI::profileCI, full_args)
  # profile <- rlang::exec(profileCI::profileCI, !!!full_args)  # Splice and execute using the !!! operator
  # NEWJEFF: Test with simplified call
print("**************** Call profileCI:")
  profile <- profileCI::profileCI(profCI_model, loglik = pll_profileCI,
                p = ps, alpha = fit_list$alpha, tails = fit_list$tails,
                parm = "all", profile = TRUE, mult = 1.1, faster = FALSE, flat = 1e-08,
                                 lb = rep(-200,3), ub = rep(200,3))
  pcm_env$fit_constrained <- hold

  # Create a labelled matrix with the bounds on the real scale
  bounds_matrix <- matrix(profile, nrow = nrow(profile), ncol = ncol(profile), dimnames = dimnames(profile))
  # Convert the real values to their natural scales:
  bounds_matrix["mu",] <- reals_to_mus(bounds_matrix["mu",])
  bounds_matrix["sigma",] <- reals_to_sigmas(bounds_matrix["sigma",])
  bounds_matrix["pi",] <- reals_to_pis(bounds_matrix["pi",])
  # # JEFF: reals_to_parms for CIs & for_plot$parameter$parameter_values
  # reals_list_lb <- list(mu = profile["mu",1], sigma = profile["sigma",1], pi = profile["pi",1])
  # reals_list_ub <- list(mu = profile["mu",2], sigma = profile["sigma",2], pi = profile["pi",2])
  # parms_list_lb <- reals_to_parms(reals_list_lb)
  # parms_list_ub <- reals_to_parms(reals_list_ub)

  # If profiles were computed for the real values, convert those
  # to their natural scales:
  profile_curves <- attr(profile,"for_plot")
  # print(profile_curves)  #NEWJEFF
  is_single_na <- function(x) { length(x) == 1 && is.na(x) }
  if (!is_single_na(profile_curves$mu)) profile_curves$mu[,1] <- reals_to_mus(profile_curves$mu[,1])
  if (!is_single_na(profile_curves$sigma)) profile_curves$sigma[,1] <- reals_to_sigmas(profile_curves$sigma[,1])
  if (!is_single_na(profile_curves$pi)) profile_curves$pi[,1] <- reals_to_pis(profile_curves$pi[,1])

  return( list(bounds_matrix = bounds_matrix, profile_curves = profile_curves, profile_fn_output = profile) )
}

pll_profileCI <- function(par, p, alpha = 1, tails = 2) {
  # Make sure parameters are in the order expected by nll_optim
  # and then negate its result to get positive log-likelihood
# print("******** Start pll_profileCI with par:")
# print(par)
# readline(prompt="Press [enter] to continue")
  par2 <- c(par[3], par[1], par[2])
  nll <- nll_optim(par2, p, alpha, tails)
# print("and par2:")
# print(par2)
# print("yielding")
# print(-nll)
# readline(prompt="Press [enter] to continue")
  return( -nll )
}
