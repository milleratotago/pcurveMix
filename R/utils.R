# utils.R

#' Function to start the shiny app for model-fitting from
#' the RStudio console.
#' @export
run_shiny_app <- function() {
  # If the shiny app references un-exported definitions that are global to
  # the package, you can either reference them with pcurveMix::: or else
  # do the following, according to Gemini. I tried it but it did not work,
  # so I just used pcurveMix::: where it was needed.
  #   ui <- source(system.file("shiny/ui.R", package = "pcurveMix"), local = TRUE)$value
  #   server <- source(system.file("shiny/server.R", package = "pcurveMix"), local = TRUE)$value
  #   # Explicitly bind the package environment to the server function
  #   environment(server) <- asNamespace("pcurveMix")
  #
  check_packages_required_for_shiny()
  l <- capture_progressr_state()
  appDir <- system.file("shiny", package = "pcurveMix")
  pcm_env$shiny_running <- TRUE
  shiny::runApp(appDir, display.mode = "normal")
  pcm_env$shiny_running <- FALSE
  # Restore progressr stuff safely when the app closes
  on.exit({
    # ONLY restore old handlers if they actually existed
    if (!is.null(l$old_handlers) && length(l$old_handlers) > 0) {
      progressr::handlers(l$old_handlers)
    } else {
      # If it was empty/NULL, clear the package's active handlers
      progressr::handlers("txtprogressbar")
    }
    # Restore the global toggle state
    progressr::handlers(global = l$old_global_state)
  }, add = TRUE)

}

# Capture progressr state so that we can return to it
# when shiny quits
capture_progressr_state <- function() {
  old_handlers <- progressr::handlers()
  old_global_state <- progressr::handlers(global = NA) # Returns TRUE/FALSE/NA

  # 2. Set up the environment safely for your app execution
  if (is.na(old_global_state) || !old_global_state) {
    progressr::handlers(global = TRUE)
  }

  # Ensure it defaults to a clean text progress bar for their console
  progressr::handlers("txtprogressbar")  # progressr overrides this in withProgressShiny blocks

  return( list(old_handlers = old_handlers, old_global_state = old_global_state) )
} # capture_progressr_state

# Function to check whether all packages needed for shiny are available.
check_packages_required_for_shiny <- function() {
  # 1. Define all packages required exclusively for the Shiny app
  shiny_deps <- c("bslib", "ggplot2", "knitr", "progressr", "rmarkdown", "shiny", # "shinyalert",
                  "shinyjs", "shinyFeedback", "testthat", "zip")
  # 2. Check which packages are missing
  missing_deps <- shiny_deps[!sapply(shiny_deps, requireNamespace, quietly = TRUE)]

  # 3. Fail gracefully with an explicit installation message
  if (length(missing_deps) > 0) {
    stop(
      "The following packages are required to run the Shiny app but are not installed:\n",
      paste("-", missing_deps, collapse = "\n"),
      "\n\nPlease install them using: install.packages(c(",
      paste0("'", missing_deps, "'", collapse = ", "), "))",
      call. = FALSE
    )
  }
}

# Function to compute a case identifier for use in switch statements.
# Possible identifiers are strings:
#  uncond_2t_h1
case_id <- function(alpha = 1, tails = 2, pi = 1) {

  if (alpha == 1) {
    s <- "uncond"
  } else if (alpha < 1 && alpha > 0) {
    s <- "cond"
  } else {
    stop("alpha must be in the range 0--1")
  }

  if (tails == 2) {
    s <- paste0(s,"_2t")
  } else if (tails == 1) {
    s <- paste0(s,"_1t")
  } else {
    stop("tails must be 1 or 2")
  }

  if (pi == 1) {
    s <- paste0(s,"_h1")
  } else if (pi < 1 && pi >= 0) {
    s <- paste0(s,"_mix")
  } else {
    print( paste("error with pi =",pi) )
    stop("pi must be in the range 0--1")
  }

    return(s)
}

#' Check a vector of p values to see whether they are
#'   all >0 and <=alpha_cutoff as expected.
#' @param ps Vector of p values
#' @param alpha_cutoff Maximum p value allowed in file
#' @param show_error Show error message if any p value is out of
#'  bounds (default TRUE)
#' @returns A list all_in_bounds, n_too_small, etc
#' @export
check_ps <- function(ps, alpha_cutoff, show_error = TRUE) {
  # These p's are simply out of bounds and must be excluded:
  too_small <- ps < 0
  too_large <- (ps > alpha_cutoff) # | (ps == 1)
  n_too_small <- sum(too_small)
  n_too_large <- sum(too_large)
  # These p's are at the extreme 0/1 edges and must be replaced:
  edge_p <- get_ao("edge_p")
  nearly_zero <- ps < edge_p
  nearly_one <- ps > 1 - edge_p
  n_nearly_zero <- sum(nearly_zero)
  n_nearly_one <- sum(nearly_one)
  if (n_nearly_zero > 0) ps[nearly_zero] <- edge_p
  if (n_nearly_one > 0) ps[nearly_one] <- 1 - edge_p
  all_in_bounds <- (n_too_small + n_too_large == 0)
  l <- list(all_in_bounds = all_in_bounds,
            alpha_cutoff = alpha_cutoff,
            n_too_small = n_too_small,
            n_too_large = n_too_large,
            n_nearly_zero = n_nearly_zero,
            n_nearly_one = n_nearly_one,
            ps_too_small = ps[too_small],
            ps_too_large = ps[too_large],
            ps_in_bounds = ps[ !(too_small | too_large) ]
  )
  if (!l$all_in_bounds && show_error) {
    problem_string <- bad_ps_report_string(l)
    if (pcm_env$shiny_is_installed && shiny::isRunning() ) {
      shiny::showModal(shiny::modalDialog(title = "Problematic p values", problem_string, easyClose = TRUE))
    } else {
      print(problem_string)
    }
  }
  return(l)
}

# Construct a string describing the problems found by check_ps
# @param l List produced by check_ps
# @returns String
# DO NOT export
bad_ps_report_string <- function(l) {
  err <- rep(NULL, 4)
  if (l$n_too_small > 0) err[1] <- paste(l$n_too_small,"p's < 0")
  if (l$n_too_large > 0) err[2] <- paste(l$n_too_large,"p's >",l$alpha_cutoff)
  edge_p <- get_ao("edge_p")
  if (l$n_nearly_zero > 0) err[3] <- paste0("Replaced p's in [0,",edge_p,"] with ",edge_p)
  s_edge_p <- paste0("(",edge_p,")")
  if (l$n_nearly_one > 0) err[4] <- paste0("Replaced p's in [1-",s_edge_p,",1] with 1-",s_edge_p)
  if (all(is.null(err))) {
    s <- "Check p's; found no problematic p's"
  } else {
    words <- paste(err, collapse = "; ")
    s <- paste("Check p's found problems:",words)
  }
  return(s)
}

#' Convert a fit_list (output of fit_p_curve) to a data frame with a single row
#'  for convenient accumulation of multiple fit results via rbind
#' @param fit_list A list that was the output of fit_p_curve
#' @returns A data frame with a single row
#' @export
fit_list_to_df <- function(fit_list) {
  fit_list$start <- NULL
  fit_list$start_parm_set <- NULL
  fit_list$check_ps_list <- NULL
  fit_list$pi_se <- fit_list$se["pi"]
  fit_list$mu_se <- fit_list$se["mu"]
  fit_list$sigma_se <- fit_list$se["sigma"]
  fit_list$se <- NULL
  fit_list$pi_lower <- fit_list$conf_int["pi",CI_LOWER_BOUND_LABEL]
  fit_list$pi_upper <- fit_list$conf_int["pi",CI_UPPER_BOUND_LABEL]
  fit_list$mu_lower <- fit_list$conf_int["mu",CI_LOWER_BOUND_LABEL]
  fit_list$mu_upper <- fit_list$conf_int["mu",CI_UPPER_BOUND_LABEL]
  fit_list$sigma_lower <- fit_list$conf_int["sigma",CI_LOWER_BOUND_LABEL]
  fit_list$sigma_upper <- fit_list$conf_int["sigma",CI_UPPER_BOUND_LABEL]
  fit_list$conf_int <- NULL
  fit_list$ks_Dmax <- fit_list$ks$statistic
  fit_list$ks_p_value <- fit_list$ks$p.value
  fit_list$ks_exact <- fit_list$ks$exact
  fit_list$ks <- NULL
  df <- as.data.frame(fit_list)
  return(df)
}

bias_corrected_name <- function(s) {
  return(paste0("bc_",s))
}

quantile_name <- function(base_name) {
  q_name <- paste0("q_", base_name)
}

get_p_seq <- function(alpha) {
  ps <- seq(from = 0.001, to = alpha, length.out = 100)  # NEWJEFF: HARD-CODED
  return(ps)
}

# Function to get analysis option that is determined by shiny when shiny
# is running, and otherwise is determined by package environment.
get_ao <- function(name) {
  # 1. Check if there's a session-level setting (e.g. session$userData)
  # 2. Fallback to package option or package environment default
  shiny_session <- shiny::getDefaultReactiveDomain()
  if (!is.null(shiny_session)) {
    if (!is.null(shiny_session$userData[[name]])) {
      return( shiny_session$userData[[name]]() )  # This is a function call
    }
    if ( !is.null(shiny_session$input[[name]])) {
      return( shiny_session$input[[name]] )  # This is a value
    }
  }
  # Default fallback use package environment value
  return(pcm_env[[name]])
}
