# dev_common_boot_analysis.R

do_bootstrapping <- function() {  # Parametric bootstrapping
  real_ps <- v$fit_results_list$check_ps$ps_in_bound
  full_sample_n <- length(real_ps)
  n_subsamples <- input$np_n_boot_samples
  mat_of_ps <- generate_parametric_subsamples(n_subsamples, full_sample_n) # , real_ps)
  progressr::withProgressShiny(
    message = "Parametric bootstrapping in progress...",
    detail = "Starting...",
    expr = {
      ests_df <- fits_for_matrix(mat_of_ps,
                                  alpha = v$fit_results_list$alpha,
                                  tails = v$fit_results_list$tails,
                                  alpha_sig = v$fit_results_list$alpha_sig,
                                  want_optim_hessian = FALSE,
                                  start_parms = pcm_env$optim_starting_parms,
                                  n_progress_bar_steps = 20)
    }
  )
  v$boot_pct_converged <- 100 * mean(ests_df$converged)
  v$boot_confidence_level <- input$boot_confidence_level / 100
  summaries <- get_parm_summaries(ests_df)
  v$boot_tbl <- bootstrap_computations(v$fit_results_list, summaries, full_sample_n)
  boot_title <- paste0("Parametric bootstrapping analysis (",
                       round(100*v$boot_confidence_level,2),
                       "% confidence)")
  output$boot_title <- renderText(boot_title)
  s1 <- paste0("* n parametric boot samples = ",n_subsamples)
  output$n_boot_samples <- renderText(s1)
  s2 <- paste0("* percent converged OK = ",round(v$boot_pct_converged,2))
  output$boot_pct_converged <- renderText(s2)
  output$boot_tbl <- renderTable(v$boot_tbl, rownames = FALSE)
} # do_bootstrapping

p_mat <- generate_parametric_subsamples(10,100,2,1,0.5)
ests_df <- fits_for_matrix(p_mat, tails = 1)
print( summarize_estimates_mn_sd_quan(ests_df, confidence_level = 95, confidence_quantiles = NA) )
print( summarize_estimates_mn_sd_quan(ests_df, confidence_level = 80, confidence_quantiles = NA) )
print( summarize_estimates_mn_sd_quan(ests_df, confidence_level = -1, confidence_quantiles = NA) )
print( summarize_estimates_mn_sd_quan(ests_df, confidence_level = 50, confidence_quantiles = c(0.01, 0.96)) )


