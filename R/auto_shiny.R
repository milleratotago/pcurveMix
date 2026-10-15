# auto_shiny.R

# NEWJEFF: Defaults of parameters taken from pcm_env. These can be modified
# using  man-roxygen templates (do this later: gemini will explain)
# User calls this function and then renders shiny with returned params
#' Function to perform shiny-like analyses on a vector of p's.
#' @param ps Vector of ps to be fit
#' @param want_jackknifing Boolean to indicate that jackknifing should be done
#'  (default = TRUE)
#' @param want_profile_cis Boolean to indicate that profile confidence intervals
#'  should be computed (default = TRUE)
#' @param n_bootstrap_parametric N of samples for parametric bootstrap;
#'  Use 0 to skip this bootstrapping (default = 100)
#' @param n_bootstrap_nonparametric N of samples for nonparametric bootstrap;
#'  Use 0 to skip this bootstrapping (default = 100)
#' @param ps_label A string label to identify this set of p's in the
#'  analysis descriptor table
#' @inheritParams set_globals
#' @inheritParams fit_p_curve
#' @returns List with params for pcurveMix_shiny_report.Rmd
#' @export
auto_shiny_compute <- function(ps, tails, alpha,
                               alpha_sig = 0.05,
                               ps_label = "my_ps",
                               want_jackknifing = TRUE,
                               n_bootstrap_parametric = 100,
                               n_bootstrap_nonparametric = 100,
                               want_profile_cis = TRUE,
                               bias_correct_ci_bounds = pcm_env$bias_correct_ci_bounds,
                               confidence_level = pcm_env$confidence_level,
                               edge_p = pcm_env$edge_p,
                               fast_boot_jack = pcm_env$fast_boot_jack,
                               optim_control = pcm_env$optim_control,
                               optim_starting_parms = pcm_env$optim_starting_parms,
                               p_seq_cdf = pcm_env$p_seq_cdf,
                               p_seq_pdf = pcm_env$p_seq_pdf,
                               profileCI_args = pcm_env$profileCI_args,
                               round_to = pcm_env$round_to,
                               small_p_bin_cutoff = pcm_env$small_p_bin_cutoff) {
  fit_results_list <- fit_p_curve(ps, alpha = alpha, tails = tails)
  real_ps <- fit_results_list$check_ps_list$ps_in_bounds
  full_sample_n <- length(real_ps)
  if (fast_boot_jack) {
    start_list <- list(mu = fit_results_list$mu, sigma = fit_results_list$sigma,
                       pi = fit_results_list$pi)
  } else {
    start_list <- pcm_env$optim_starting_parms
  }
  descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list, file_name = ps_label)
  estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
  est_table_notes <- estimates_table_notes(converged = fit_results_list$converged)

  # Save some values for later:
  est_mu <- estimates_tbl$estimate[estimates_tbl$parameter == "mu"]
  est_sigma <- estimates_tbl$estimate[estimates_tbl$parameter == "sigma"]
  est_pi <- estimates_tbl$estimate[estimates_tbl$parameter == "pi"]

  pdf_plot <- make_pdf_plot(fit_results_list)
  cdf_plot <- make_cdf_plot(fit_results_list)

  if (want_jackknifing) {
    cat("\nJackknifing\n", file = stderr())
    mat_of_ps <- generate_jackknife_subsamples(real_ps)
    progressr::with_progress({
      ests_tbl <- fits_for_matrix(mat_of_ps, start_parms = start_list, alpha = alpha, tails = tails)},
      handlers = progressr::handler_txtprogressbar(file = stderr()) # , title = "Jackknifing")
    )
    jack_pct_converged <- 100 * mean(ests_tbl$converged)
    summaries <- get_parm_summaries(ests_tbl)
    jack_tbl <- make_jackknife_tbl(fit_results_list, summaries, full_sample_n)
    jack_notes <- jackknife_table_notes(full_sample_n, jack_pct_converged)
  } else {
    jack_tbl <- NA
    jack_notes <- NA
  }

  if (n_bootstrap_parametric > 0) {
    cat("\nParametric bootstrapping\n", file = stderr())
    mat_of_ps <- generate_parametric_subsamples(n_bootstrap_parametric, full_sample_n,
                                                fit_results_list$mu, fit_results_list$sigma,
                                                pi = fit_results_list$pi,
                                                alpha = fit_results_list$alpha,
                                                tails = fit_results_list$tails)
    progressr::with_progress({
      ests_tbl <- fits_for_matrix(mat_of_ps, start_parms = start_list, alpha = alpha, tails = tails)},
      handlers = progressr::handler_txtprogressbar(file = stderr() ) # , title = "Parametric bootstrapping")
    )
    boot_pct_converged <- 100 * mean(ests_tbl$converged)
    boot_tbl <- make_boot_summary_tbl(estimates_tbl, ests_tbl)
    boot_notes <- boot_table_notes(full_sample_n, boot_pct_converged)
  } else {
    boot_pct_converged <- NA
    boot_tbl <- NA
    boot_notes <- NA
  }

  if (n_bootstrap_nonparametric > 0) {
    cat("\nNonparametric bootstrapping\n", file = stderr())
    mat_of_ps <- generate_nonparametric_subsamples(n_bootstrap_nonparametric, full_sample_n, real_ps)
    progressr::with_progress({
      ests_tbl <- fits_for_matrix(mat_of_ps, start_parms = start_list, alpha = alpha, tails = tails)},
      handlers = progressr::handler_txtprogressbar(file = stderr()) # , title = "Nonparametric bootstrapping")
    )
    np_boot_pct_converged <- 100 * mean(ests_tbl$converged)
    np_boot_tbl <- make_boot_summary_tbl(estimates_tbl, ests_tbl)
    np_boot_notes <- boot_table_notes(full_sample_n, np_boot_pct_converged)
  } else {
    np_boot_pct_converged <- NA
    np_boot_tbl <- NA
    np_boot_notes <- NA
  }

  if (want_profile_cis) {
    cat("\nProfiling...", file = stderr())
    profileCI_std <- compute_profileCI(fit_results_list)
    cat("...", file = stderr())
    profileCI_power <- compute_profileCI_power(fit_results_list)
    cat(".", file = stderr())
    if (fit_results_list$tails == 2) {
      profileCI_folded_normal_mu <- compute_profileCI_folded(fit_results_list,
                                                             target_folded_normal_mu = TRUE)
      cat(".", file = stderr())
      profileCI_folded_normal_sigma <- compute_profileCI_folded(fit_results_list,
                                                                target_folded_normal_mu = FALSE)
      cat(".", file = stderr())
    } else {
      profileCI_folded_normal_mu <- NA
      profileCI_folded_normal_sigma <- NA
    } # end if tails == 2
    cat("\n", file = stderr())
    profile_ci_tbl <- make_profile_ci_tbl(profileCI_std,
                                          profileCI_power,
                                          profileCI_folded_normal_mu,
                                          profileCI_folded_normal_sigma)
    profile_tbl_notes <- profile_table_notes()
    profile_plots <- make_profile_plots(profileCI_std,
                                        profileCI_power,
                                        profileCI_folded_normal_mu,
                                        profileCI_folded_normal_sigma)
  } else {
    profile_ci_tbl <- NA
    profile_tbl_notes <- NA
    profile_plots <- list(folded_normal_mu = NA, folded_normal_sigma = NA,
                          mu = NA, pi = NA, power = NA, sigma = NA)
  }

  params <- list(boot_notes = boot_notes,
                 boot_tbl = boot_tbl,
                 cdf_plot = cdf_plot,
                 descriptor_tbl = descriptor_tbl,
                 estimates_notes = est_table_notes,
                 estimates_tbl = estimates_tbl,
                 jack_notes = jack_notes,
                 jack_tbl = jack_tbl,
                 n_boot_samples = n_bootstrap_parametric,
                 n_jack_samples = full_sample_n,
                 np_boot_notes = np_boot_notes,
                 np_boot_tbl = np_boot_tbl,
                 np_n_boot_samples = n_bootstrap_nonparametric,
                 p_filename = ps_label,
                 pdf_plot = pdf_plot,
                 want_profile_cis = want_profile_cis,
                 profile_folded_normal_mu_plot = profile_plots$folded_normal_mu,
                 profile_folded_normal_sigma_plot = profile_plots$folded_normal_sigma,
                 profile_mu_plot = profile_plots$mu,
                 profile_pi_plot = profile_plots$pi,
                 profile_power_plot = profile_plots$power,
                 profile_sigma_plot = profile_plots$sigma,
                 profile_tbl = profile_ci_tbl,
                 profile_notes = profile_tbl_notes,
                 round_to = round_to,
                 tails = tails)
  return(params)
}

#' Function to produce a report like shiny
#' @param params Output list produced by auto_shiny function
#' @param output_file_name Name (with path) for the report file.
#' @param output_format String either "html", "pdf", or "docx"
#' @param output_dir Directory where the final output file should go
#'  (default = current working directory)
#' @export
auto_shiny_report <- function(params, output_file_name, output_format, output_dir = ".") {
  output_format <- switch(output_format,
                          html = "html_document",
                          htm = "html_document",
                          pdf = "pdf_document",
                          docx = "word_document") # NEWJEFF: Need otherwise
  # rmd_outfile_name <- paste0(output_directory_name, "/",
  #                            "pcurveMix_report_", time_stamp, ext)
  # NEWJEFF: Check output_file_name extention
  package_path <- system.file(package = "pcurveMix")
  rmd_infile_name = paste0(package_path,"/shiny/pcurveMix_shiny_report.Rmd")
  rmarkdown::render(rmd_infile_name,
                    output_file = output_file_name,
                    output_dir = output_dir,
                    output_format = output_format,
                    params = params,
                    envir = new.env(parent = globalenv()))
}
