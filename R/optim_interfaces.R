# optim_interfaces.R

# Note: optim() fit method "L-BFGS-B" is "constrained";
#  it allows for lower and upper bounds for parameters,
# "BFGS" is better when bounds are not used, in which case
#  the real numbers manipulated by optim have to be converted
#  to the natural parameter space (i.e., 0<pi<1, sigma>0).
#  This conversion is done by parms_to_reals & reals_to_parms.

optim_fit_constrained <- function(p, alpha, tails, alpha_sig, start_list,
                                  want_optim_hessian,
                                  lower, upper) {
  start_vec <- c(start_list$pi, start_list$mu, start_list$sigma)
  lower_vec <- c(lower$pi, lower$mu, lower$sigma)
  upper_vec <- c(upper$pi, upper$mu, upper$sigma)
  opt <- tryCatch(  # tryCatch 1
    {
      stats::optim(par = start_vec, fn = nll_optim, p = p, alpha = alpha, tails = tails,
                   method = "L-BFGS-B", lower = lower_vec, upper = upper_vec,
                   hessian = want_optim_hessian,
                   control = pcm_env$optim_control)
    }, # end of try 1
    error = function(cond) {
      backup_opt <- optim_error_recovery(cond, start_vec, p, alpha = alpha, tails = tails,
                                         method = "L-BFGS-B",
                                         hessian = want_optim_hessian, lower_vec = lower_vec, upper_vec = upper_vec)
      return(backup_opt)
    }
  ) # end tryCatch 1
  est <- opt$par; H <- opt$hessian
  se <- ci <- NULL
  if (is.matrix(H) && all(is.finite(H))) {
    Vinv <- try(solve(H), silent = TRUE)
    if (!inherits(Vinv, "try-error")) {
      se <- sqrt(pmax(diag(Vinv), 0)); names(se) <- c("pi","mu","sigma")
      z <- 1.96
      ci <- cbind(est - z*se, est + z*se)
      rownames(ci) <- c("pi","mu","sigma"); colnames(ci) <- c("lwr95","upr95")
      ci["pi",]    <- pmin(pmax(ci["pi",], 1e-6), 1 - 1e-6)
      ci["mu",]    <- pmax(ci["mu",], 0)
      ci["sigma",] <- pmax(ci["sigma",], 1e-6)
    }
  } # if (is.matrix(H)
  if (is.null(se)) {
    # Ensure that there is _something_ in these positions.
    se <- c(NA, NA, NA); names(se) <- c("pi","mu","sigma")
    ci <- matrix(rep(NA,6), nrow = 3, ncol = 2);
    rownames(ci) <- c("pi","mu","sigma"); colnames(ci) <- c("lwr95","upr95")
  } # if (is.null(se))
  fit <- list(alpha = alpha, alpha_sig = alpha_sig, tails = tails,
              pi = est[1], mu = est[2], sigma = est[3], start = start_list,
              se = se, ci95 = ci, logLik = -opt$value,
              converged = (opt$convergence == 0))
  return(fit)
} # optim_fit_constrained

optim_fit_unconstrained <- function(p, alpha, tails, alpha_sig, start_list,
                                    want_optim_hessian) {
  start_reals <- parms_to_reals(start_list)
  start_real_vec <- c(start_reals$pi, start_reals$mu, start_reals$sigma)
  opt <- tryCatch(  # tryCatch 1
    { stats::optim(par = start_real_vec, fn = nll_optim, p = p, alpha = alpha, tails = tails,
                   method = "BFGS", hessian = want_optim_hessian,
                   control = pcm_env$optim_control)
    }, # end of try 1
    error = function(cond) {
      backup_opt <- optim_error_recovery(cond, start_real_vec, p, alpha = alpha, tails = tails,
                                         method = "BFGS",
                                         hessian = want_optim_hessian)
      return(backup_opt)
    }
  ) # end tryCatch 1
  est <- opt$par;
  real_parms <- list(mu = est[2], sigma = est[3], pi = est[1])
  parms <- reals_to_parms(real_parms)
  # MLSE <- pcm_MLSE(p, parms$mu, parms$sigma, parms$pi, alpha, tails)  # NEWJEFF These look wrong
  # est <- c(parms$pi, parms$mu, parms$sigma)
  # l <- make_se_ci(est, MLSE$SE)  # NEWJEFF: make_se_ci no longer used
  l <- real_to_nat_se_ci(opt$par, opt$hessian)
  fit <- list(alpha = alpha, alpha_sig = alpha_sig, tails = tails,
              pi = parms$pi, mu = parms$mu, sigma = parms$sigma, start = start_list,
              se = l$se, ci95 = l$ci, logLik = -opt$value,
              converged = (opt$convergence == 0))
  return(fit)
}

# This function is called when a "try 1" optim bombs.
# If that optim wanted a hessian, then try again without the hessian.
# If that optim did not want a hessian, return an opt structure
#  indicating failure.
optim_error_recovery <- function(cond, start_real_vec, p, alpha, tails,
                                 method, hessian, lower_vec = NULL, upper_vec = NULL) {
  if (!hessian) {
    opt <- optim_recovery_failed(cond, start_real_vec, p, alpha, tails,
                                 method, lower_vec, upper_vec)
    return(opt)
  }
  # Try again without hessian
  opt <- tryCatch(  # tryCatch 2
    {
      if (method == "BFGS") {
        temp <- stats::optim(par = start_real_vec, fn = nll_optim, p = p, alpha = alpha, tails = tails,
                             method = method, hessian = FALSE,
                             control = pcm_env$optim_control)
      } else if (method == "L-BFGS-B") {
        temp <- stats::optim(par = start_real_vec, fn = nll_optim, p = p, alpha = alpha, tails = tails,
                             method = method, hessian = FALSE,
                             control = pcm_env$optim_control, lower = lower_vec, upper = upper_vec)
      } else {
        stop( paste("FATAL ERROR: optim_error_recovery does not recognize optim method",method) )
      }
      return(temp)
    }, # end of try 2
    error = function(cond) {
      backup_opt <- optim_recovery_failed(cond, start_real_vec, p, alpha, tails,
                                          method, lower_vec, upper_vec)
      return(backup_opt)
    }
  ) # end tryCatch 2
}

# Function to print status/diagnostic information when optim fails.
# When shiny is going, this directs the info to a renderPrint.
# Otherwise, it prints to the console and also to a file
optim_recovery_failed <- function(cond, start_vec, p, alpha, tails,
                                  method, lower_vec, upper_vec) {
  if (pcm_env$shiny_running) {
    output$optim_failed_output <- shiny::renderPrint(
      print_optim_error_msg(cond, start_vec, p, alpha, tails, method, lower_vec, upper_vec) )
  } else {
    # Shiny not running so print to console and then to an error file
    print_optim_error_msg(cond, start_vec, p, alpha, tails, method, lower_vec, upper_vec)  # to console
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    unique_file <- paste0("pcurveMix_optim_error_", timestamp, ".txt")
    # unique_file <- tempfile(pattern = "pcurveMix_optim_error_", tmpdir = ".", fileext = ".txt")
    sink(file = unique_file)  # Send the printing to the file
    print_optim_error_msg(cond, start_vec, p, alpha, tails, method, lower_vec, upper_vec)  # to console
    sink()  # stop sinking
  }
  stop("NEWJEFF: I don't know what to return")
  # return(NULL)
}

print_optim_error_msg <- function(cond, start_vec, p, alpha, tails,
                                  method, lower_vec, upper_vec) {
  sep <- "-------"
  print("Optimization failed with error message:")
  print(cond)
  print(sep)
  print("Fitting conditions:")
  print( paste("alpha =",alpha) )
  print( paste("tails =",tails) )
  print( paste("optimization method =",method) )
  print("starting parameter values:")
  print(start_vec)
  if (!is.null(lower_vec)) {
    print("lower bounds on parameters:")
    print(lower_vec)
  }
  if (!is.null(upper_vec)) {
    print("upper bounds on parameters:")
    print(upper_vec)
  }
  print(sep)
  print("vector of p's to be fit:")
  print(p)
  print(sep)
} # print_optim_error_msg

