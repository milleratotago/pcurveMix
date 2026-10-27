# auto_shiny.R

#' Function to perform shiny-like analyses on a vector of p's. Call this function
#'  and then pass its returned output list to auto_shiny_report.
#' @inheritParams pcurveMix-package
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
#' @returns List with params for pcurveMix_shiny_report.Rmd
#' @export
auto_shiny_compute <- function(ps, tails, alpha,
                               alpha_sig = 0.05,
                               ps_label = "my_ps",
                               want_jackknifing = TRUE,
                               n_bootstrap_parametric = 100,
                               n_bootstrap_nonparametric = 100,
                               want_profile_cis = TRUE
                               # pcm_env parameters must be set with set_globals prior
                               #  to calling this function.
                               # bias_correct_ci_bounds = pcm_env$bias_correct_ci_bounds,
                               # confidence_level = pcm_env$confidence_level,
                               # edge_p = pcm_env$edge_p,
                               # fast_boot_jack = pcm_env$fast_boot_jack,
                               # optim_control = pcm_env$optim_control,
                               # optim_starting_parms = pcm_env$optim_starting_parms,
                               # p_seq_cdf = pcm_env$p_seq_cdf,
                               # p_seq_pdf = pcm_env$p_seq_pdf,
                               # profileCI_args = pcm_env$profileCI_args,
                               # round_to = pcm_env$round_to,
                               # censor_at = pcm_env$censor_at
                               ) {
  fit_results_list <- fit_p_curve(ps, alpha = alpha, tails = tails)
  real_ps <- fit_results_list$check_ps_list$ps_in_bounds
  full_sample_n <- length(real_ps)
  if (pcm_env$fast_boot_jack) {
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

  pdf_plot_list <- make_pdf_plot(fit_results_list)
  pdf_plot <- pdf_plot_list$pdf_plot
  cdf_plot_list <- make_cdf_plot(fit_results_list)
  cdf_plot <- cdf_plot_list$cdf_plot

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
    boot_notes <- boot_table_notes(n_bootstrap_parametric, full_sample_n, boot_pct_converged)
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
    np_boot_notes <- boot_table_notes(n_bootstrap_nonparametric, full_sample_n, np_boot_pct_converged)
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
    profileCI_std <- NA
    profileCI_power <- NA
    profileCI_folded_normal_mu <- NA
    profileCI_folded_normal_sigma <- NA
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
                 pred_cdfs = cdf_plot_list$pred_cdfs,
                 pred_pdfs = pdf_plot_list$pred_pdfs,
                 want_profile_cis = want_profile_cis,
                 profileCI_std = profileCI_std,
                 profileCI_power = profileCI_power,
                 profileCI_folded_normal_mu = profileCI_folded_normal_mu,
                 profileCI_folded_normal_sigma = profileCI_folded_normal_sigma,
                 profile_folded_normal_mu_plot = profile_plots$folded_normal_mu,
                 profile_folded_normal_sigma_plot = profile_plots$folded_normal_sigma,
                 profile_mu_plot = profile_plots$mu,
                 profile_pi_plot = profile_plots$pi,
                 profile_power_plot = profile_plots$power,
                 profile_sigma_plot = profile_plots$sigma,
                 profile_tbl = profile_ci_tbl,
                 profile_notes = profile_tbl_notes,
                 round_to = pcm_env$round_to,
                 tails = tails)
  return(params)
}

#' Function to produce a report like shiny
#' @param params Output list produced by auto_shiny function.
#' @param output_file_name Name (with path) for the report file.
#' @param output_format String either "html", "pdf", or "docx" (default "html").
#' @param output_dir Directory where the final output file should go
#'  (default = current working directory).
#' @export
auto_shiny_report <- function(params, output_file_name, output_format = "html", output_dir = ".") {
  output_format <- get_render_output_format(output_format)
  package_path <- system.file(package = "pcurveMix")
  rmd_infile_name = paste0(package_path,"/shiny/pcurveMix_shiny_report.Rmd")
  rmarkdown::render(rmd_infile_name,
                    output_file = output_file_name,
                    output_dir = output_dir,
                    output_format = output_format,
                    params = params,
                    envir = new.env(parent = globalenv()))
}

#' Function to write output files like shiny (unzipped).
#' @param params Output list produced by auto_shiny function.
#' @param output_dir Directory where the output files should go
#'  (default = current working directory).
#' @param file_name_prefix String prepended to output file names (default = "").
#' @param time_stamp String time_stamp appended to output file names
#'  (default = "", in which case the time_stamp is automatically generated
#'  if generate_time_stamp is TRUE).
#' @param generate_time_stamp Boolean. If no time_stamp is given and this
#'  Boolean is TRUE, generate a time_stamp and append it to
#'  output file names (default = TRUE).
#' @returns A string vector with the names of the output files.
#' @export
auto_shiny_files <- function(params, output_dir = ".", file_name_prefix = "", time_stamp = "",
                             generate_time_stamp = TRUE) {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  if (nchar(time_stamp) == 0 && generate_time_stamp) {
      # Create time stamp to mark output file names
      time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  }
  if (nchar(file_name_prefix) > 0) file_name_prefix <- paste0(file_name_prefix,"_")

  # Helper function to generate file names
  csv_name <- function(contents) {
    s <- paste0(output_dir, "/",file_name_prefix,contents,"_", time_stamp, ".csv")
  }

  # Write CSV files of tables that must be present
  csv_descriptor_outfile_name <- csv_name("descriptor_tbl")
  utils::write.csv(params$descriptor_tbl, csv_descriptor_outfile_name, row.names = FALSE)
  csv_estimates_outfile_name <- csv_name("estimates_tbl")
  utils::write.csv(params$estimates_tbl, csv_estimates_outfile_name, row.names = FALSE)

  # Write CSV files of predicted pdf and cdf values
  csv_pdf_outfile_name <- csv_name("pred_pdf")
  utils::write.csv(params$pred_pdfs, csv_pdf_outfile_name, row.names = FALSE)
  csv_cdf_outfile_name <- csv_name("pred_cdf")
  utils::write.csv(params$pred_cdfs, csv_cdf_outfile_name, row.names = FALSE)

  csv_file_list <- c(csv_descriptor_outfile_name, csv_estimates_outfile_name,
                     csv_pdf_outfile_name, csv_cdf_outfile_name)

  # Write CSV files of tables that may be present
  if (!is.null(params$jack_tbl) && !identical(params$jack_tbl,NA)) {
    csv_jack_outfile_name <- csv_name("jack_tbl")
    utils::write.csv(params$jack_tbl, csv_jack_outfile_name, row.names = FALSE)
    csv_file_list <- c(csv_file_list, csv_jack_outfile_name)
  }
  if (!is.null(params$boot_tbl) && !identical(params$boot_tbl,NA)) {
    csv_boot_outfile_name <- csv_name("boot_tbl")
    utils::write.csv(params$boot_tbl, csv_boot_outfile_name, row.names = FALSE)
    csv_file_list <- c(csv_file_list, csv_boot_outfile_name)
  }
  if (!is.null(params$np_boot_tbl) && !identical(params$np_boot_tbl,NA)) {
    csv_np_boot_outfile_name <- csv_name("np_boot_tbl")
    utils::write.csv(params$np_boot_tbl, csv_np_boot_outfile_name, row.names = FALSE)
    csv_file_list <- c(csv_file_list, csv_np_boot_outfile_name)
  }
  if (!is.null(params$profile_tbl) && !identical(params$profile_tbl,NA)) {
    csv_profile_outfile_name <- csv_name("profile_tbl")
    utils::write.csv(params$profile_tbl, csv_profile_outfile_name, row.names = FALSE)
    csv_file_list <- c(csv_file_list, csv_profile_outfile_name)
  }

  # Write CSV files of profile curves if these were requested:
  if ( !is.null(params$profile_tbl) && !identical(params$profile_tbl,NA) ) {

    csv_profile_mu_outfile_name <- csv_name("profile_mu")
    profile_mu <- data.frame(mu = params$profileCI_std$profile_curves$mu[,1],
                             likelihood = params$profileCI_std$profile_curves$mu[,2])
    utils::write.csv(profile_mu, csv_profile_mu_outfile_name, row.names = FALSE)

    csv_profile_sigma_outfile_name <- csv_name("profile_sigma")
    profile_sigma <- data.frame(sigma = params$profileCI_std$profile_curves$sigma[,1],
                                likelihood = params$profileCI_std$profile_curves$sigma[,2])
    utils::write.csv(profile_sigma, csv_profile_sigma_outfile_name, row.names = FALSE)

    csv_profile_pi_outfile_name <- csv_name("profile_pi")
    profile_pi <- data.frame(pi = params$profileCI_std$profile_curves$pi[,1],
                             likelihood = params$profileCI_std$profile_curves$pi[,2])
    utils::write.csv(profile_pi, csv_profile_pi_outfile_name, row.names = FALSE)

    csv_profile_power_outfile_name <- csv_name("profile_power")
    temp_mat <- extract_profile_plot_columns(params$profileCI_power,1) # 1st profiled parm
    temp_mat[,1] <- reals_to_powers(temp_mat[,1])
    profile_power <- data.frame(power = temp_mat[,1],
                                likelihood = temp_mat[,2])
    utils::write.csv(profile_power, csv_profile_power_outfile_name, row.names = FALSE)

    csv_file_list <- c(csv_file_list, csv_profile_mu_outfile_name, csv_profile_sigma_outfile_name,
                       csv_profile_pi_outfile_name, csv_profile_power_outfile_name)

    if ( !is.null(params$profile_folded_normal_mu_plot) ) {
      csv_profile_folded_normal_mu_outfile_name <- csv_name("profile_folded_normal_mu")
      temp_mat <- extract_profile_plot_columns(params$profileCI_folded_normal_mu,1) # 1st profiled parm
      temp_mat[,1] <- reals_to_mus(temp_mat[,1])
      profile_folded_normal_mu <- data.frame(folded_normal_mu = temp_mat[,1],
                                             likelihood = temp_mat[,2])
      utils::write.csv(profile_folded_normal_mu, csv_profile_folded_normal_mu_outfile_name, row.names = FALSE)

      csv_profile_folded_normal_sigma_outfile_name <- csv_name("profile_folded_normal_sigma")
      temp_mat <- extract_profile_plot_columns(params$profileCI_folded_normal_sigma,1) # 1st profiled parm
      temp_mat[,1] <- reals_to_sigmas(temp_mat[,1])
      profile_folded_normal_sigma <- data.frame(folded_normal_sigma = temp_mat[,1],
                                                likelihood = temp_mat[,2])
      utils::write.csv(profile_folded_normal_sigma, csv_profile_folded_normal_sigma_outfile_name, row.names = FALSE)

      csv_file_list <- c(csv_file_list, csv_profile_folded_normal_mu_outfile_name, csv_profile_folded_normal_sigma_outfile_name)
    } # if ( !is.null(params$profile_folded_normal_mu_plot) )
  } # if ( !identical(params$profile_tbl,NA) )
  return(csv_file_list)
} # auto_shiny_files
