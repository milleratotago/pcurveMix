# server.R

# Programming notes:
# - server() is not allowed to set global package variables in pcm_env
#   via set_globals() or any other means. If package code needs
#   values that are either in pcm_env or set by shiny user,
#   these values should be retrieved via get_ao(), which checks
#   to see if the values were set in shiny, returning them if so,
#   and returning pcm_env values if not.
#
# - This file uses pcurveMix::: references to unexported pcurveMix
#   constants and functions; the shiny can't see those otherwise.
#
# - Analysis options that cannot yet be set by UI:
#   These must be preset via set_global before starting shiny.
#   - MLSEh & small_rcond: both UNUSED
#   - optim_starting_parms & profileCI_args: must be preset in global pcm_env
#   - For parametric bootstrapping, cond_method & tol used to generate random p's.
#
# - An interesting unexpected behavior of ggplot plots:
#   If you assign different values to local x & y variables, changing them
#   across multiple ggplots, all of the plots end up showing the final x & y
#   variables. Maybe this is because the plots are stored in reactiveValues?


server <- function(input, output, session) {

  # Some initial setup
  {
  # Special handling for scientific notation:
  # This block runs automatically EXACTLY ONCE when the app loads
  observe({
    # Send the value and the appropriate step size back to the UI immediately
    updateNumericInput(session = session, inputId = "edge_p",
      value = pcm_env$edge_p,
      step = 1e-11 # Allows the browser to handle the small scientific decimal
    )
  })

  # Initialize UI analysis options fields with package settings.
  initialize_ui_with_package_vals <- function(package_settings) {
    updateCheckboxInput(session = session, inputId = "bias_correct_ci_bounds", value = package_settings$bias_correct_ci_bounds)
    updateNumericInput(session = session,  inputId = "confidence_level", value = package_settings$confidence_level)
    updateTextInput(session = session, inputId = "text_edge_p",
      value = format(package_settings$edge_p, scientific = TRUE) )

    # Convert optim_control list to string for display in UI
    list_as_string <- deparse(pcm_env$optim_control)
    updateTextAreaInput(session, "text_optim_control", value = list_as_string)

    updateCheckboxInput(session = session, inputId = "fast_boot_jack", value = package_settings$fast_boot_jack)
    updateNumericInput(session = session,  inputId = "round_to", value = package_settings$round_to)
    updateNumericInput(session = session,  inputId = "censor_at", value = package_settings$censor_at)
  }

  # Special functions needed to return values computed from UI settings
  session$userData$edge_p <- eventReactive(input$edge_p, {
    return( as.numeric(input$text_edge_p) )
  })
  session$userData$optim_control <- eventReactive(input$optim_control, {
    result <- tryCatch({
      # Using safe_env avoids security risk here by not allowing
      # evil-doers to inject harmful commands (e.g., via "system()")
      safe_env <- new.env(parent = emptyenv())
      # Explicitly copy over safe primitives and data types
      assign("list", base::list, envir = safe_env)
      eval(parse(text = input$text_optim_control), envir = safe_env )
    }, error = function(e) {
      stop("syntax error in optim_control string:", e$message)
    })
    return( result )
  })

  # Grab the package environment for server use
  # Remember: Some of these variables will be overridden by ui inputs
  pcm_env <- pcurveMix:::pcm_env
  initialize_ui_with_package_vals(pcm_env)

  # Hide some stuff that should not appear initially
  shinyjs::hide("analysis_options_panel")
  shinyjs::disable(id = "btnFit")
  shinyjs::disable(id = "btn_choose_analysis_options")

  # A dynamic list to hold analysis results; fields will be added.
  v <- reactiveValues(fit_completed = FALSE)
  }   # End initial setup

  restart <- function()  {
    v$fit_completed = FALSE
    fields <- names(v)
    fields_to_save <- c("fit_completed", "p_filename", "df")
    for (field in fields) {
      if ( !(field %in% fields_to_save) ) v[[field]] <- NULL
    }
    output$model_fit_title <- renderText(NULL)
    output$parameter_estimates_title <- renderText(NULL)
    output$predicted_pdfs_title <- renderText(NULL)
    output$predicted_cdfs_title <- renderText(NULL)
    output$boot_title <- renderText(NULL)
    output$n_boot_samples <- renderText(NULL)
    output$boot_pct_converged <- renderText(NULL)
    output$descriptor_tbl <- renderTable(NULL, rownames = FALSE)
    output$estimates_tbl <- renderTable(NULL, rownames = FALSE)
    output$boot_tbl <- renderTable(NULL, rownames = FALSE)
    output$pdf_plot <- renderPlot(NULL)
    output$cdf_plot <- renderPlot(NULL)
  } # restart

  # Helper function for bullet-point display of a vector of strings.
  render_strings_as_bullets <- function(my_strings) {
    return( renderUI(  tags$ul( lapply(my_strings, tags$li) )  ) )
  }

  # Helper function to get estimates for jackknife or bootstrap samples
  get_ests <- function(mat_of_ps, title) {
    if (input$fast_boot_jack) {
      start_list <- list(mu = v$fit_list$mu, sigma = v$fit_list$sigma, pi = v$fit_list$pi)
    } else {
      start_list <- pcm_env$optim_starting_parms
    }
    progressr::withProgressShiny(
      message = paste(title,"in progress..."),
      detail = "Starting...",
      expr = {
        ests_tbl <- fits_for_matrix(mat_of_ps,
                                    alpha = v$fit_list$alpha,
                                    tails = v$fit_list$tails,
                                    alpha_sig = v$fit_list$alpha_sig,
                                    want_optim_hessian = FALSE,
                                    start_parms = start_list,
                                    n_progress_bar_steps = 20)
      }
    ) # progressr:withProgressShiny
    return(ests_tbl)
  } # get_ests

  do_jackknifing <- function() {
    if (!input$jackknifing) {
      v$n_jack_samples <- 0
      return()
    }
    real_ps <- v$fit_list$check_ps$ps_in_bound
    full_sample_n <- length(real_ps)
    mat_of_ps <- generate_jackknife_subsamples(real_ps)
    ests_tbl <- get_ests(mat_of_ps, "Jackknifing")
    v$n_jack_samples <- full_sample_n
    v$jack_pct_converged <- 100 * mean(ests_tbl$converged)
    v$jack_confidence_level <- get_globals("confidence_level")
    summaries <- get_parm_summaries(ests_tbl)
    v$jack_tbl <- make_jackknife_tbl(v$fit_list, summaries, full_sample_n)
    jack_title <- paste0("Jackknife analysis (",
                         round(v$jack_confidence_level,input$round_to),
                         "% confidence)")
    output$jackknife_title <- renderText(jack_title)
    output$jackknife_tbl <- renderTable(v$jack_tbl, rownames = FALSE, digits = input$round_to)
    v$jack_notes <- pcurveMix:::jackknife_table_notes(full_sample_n,v$jack_pct_converged)
    output$jackknife_notes <- render_strings_as_bullets(v$jack_notes)
  } # do_jackknifing

  do_bootstrapping <- function() {  # Parametric bootstrapping
    if (!input$parametric_bootstrapping) {
      v$n_boot_samples <- 0
      return()
    }
    mu <- v$fit_list$mu
    sigma <- v$fit_list$sigma
    pi <- v$fit_list$pi
    alpha <- v$fit_list$alpha
    tails <- v$fit_list$tails
    n_ps <- length(v$fit_list$check_ps$ps_in_bound)
    v$n_boot_samples <- input$n_boot_samples
    mat_of_ps <- generate_parametric_subsamples(v$n_boot_samples, n_ps, mu, sigma, pi, alpha, tails)
    ests_tbl <- get_ests(mat_of_ps, "Parametric bootstrapping")
    v$boot_pct_converged <- 100 * mean(ests_tbl$converged)
    v$boot_confidence_level <- get_globals("confidence_level")
    mle_estimates_tbl <- fit_to_estimates_tbl(v$fit_list)
    v$boot_tbl <- make_boot_summary_tbl(mle_estimates_tbl, ests_tbl,
                                        confidence_level = input$confidence_level,
                                        bias_correct_ci_bounds = input$bias_correct_ci_bounds)
    boot_title <- paste0("Parametric bootstrap analysis (",
                         round(v$boot_confidence_level,2),
                         "% confidence)")
    output$boot_title <- renderText(boot_title)
    output$boot_tbl <- renderTable(v$boot_tbl, rownames = FALSE, digits = input$round_to)
    v$boot_notes <- pcurveMix:::boot_table_notes(v$n_boot_samples,n_ps,v$boot_pct_converged)
    output$boot_notes <- render_strings_as_bullets(v$boot_notes)
  } # do_bootstrapping

  do_npbootstrapping <- function() {  # Nonparametric bootstrapping
    if (!input$nonparametric_bootstrapping) {
      v$np_n_boot_samples <- 0
      return()
    }
    real_ps <- v$fit_list$check_ps$ps_in_bound
    full_sample_n <- length(real_ps)
    v$np_n_boot_samples <- input$np_n_boot_samples
    mat_of_ps <- generate_nonparametric_subsamples(v$np_n_boot_samples, full_sample_n, real_ps)
    ests_tbl <- get_ests(mat_of_ps, "Nonparametric bootstrapping")
    v$np_boot_pct_converged <- 100 * mean(ests_tbl$converged)
    v$np_boot_confidence_level <- get_globals("confidence_level")
    mle_estimates_tbl <- fit_to_estimates_tbl(v$fit_list)
    v$np_boot_tbl <- make_boot_summary_tbl(mle_estimates_tbl, ests_tbl,
                                           confidence_level = input$confidence_level,
                                           bias_correct_ci_bounds = input$bias_correct_ci_bounds)
    npboot_title <- paste0("Nonparametric bootstrap analysis (",
                           round(v$np_boot_confidence_level,2),
                           "% confidence)")
    output$np_boot_title <- renderText(npboot_title)
    output$np_boot_tbl <- renderTable(v$np_boot_tbl, rownames = FALSE, digits = input$round_to)
    v$np_boot_notes <- pcurveMix:::boot_table_notes(v$np_n_boot_samples,full_sample_n,v$boot_pct_converged)
    output$np_boot_notes <- render_strings_as_bullets(v$np_boot_notes)
  } # do_npbootstrapping

  # Helper function to display profiling notifications
  profiling_notification <- function(notif_id, parameter_name) {
    showNotification(
      paste("Profiling",parameter_name,"..."),
      id = notif_id,
      duration = NULL,
      closeButton = TRUE,
      type = "message"
    )
  }

  profile_manager <- function(fit_list) {
    if (!input$profile_ci) {
      v$want_profile_cis <- FALSE  # Needed to pass to Rmd
      return(NULL)
    } else {
      v$want_profile_cis <- TRUE
    }
    # Computations:
    v$profile_ci_confidence_level <- input$confidence_level / 100  # profileCI package wants 0-1 confidence level
    notif_id <- "profileCI_std_notif_id" # Use the same id for all notifications to overwrite them

    profiling_notification(notif_id, "mu, sigma, and pi")
    v$profileCI_std <- compute_profileCI(fit_list, confidence_level = v$profile_ci_confidence_level)

    profiling_notification(notif_id, "power")
    v$profileCI_power <- compute_profileCI_power(fit_list, confidence_level = v$profile_ci_confidence_level)

    if (fit_list$tails == 2) {
      profiling_notification(notif_id, pcurveMix:::FOLDED_NORMAL_MU_LABEL)
      v$profileCI_folded_normal_mu <- compute_profileCI_folded(fit_list, TRUE, confidence_level = v$profile_ci_confidence_level)

      profiling_notification(notif_id, pcurveMix:::FOLDED_NORMAL_SIGMA_LABEL)
      v$profileCI_folded_normal_sigma <- compute_profileCI_folded(fit_list, FALSE, confidence_level = v$profile_ci_confidence_level)
    } else {
      v$profileCI_folded_normal_mu <- NA
      v$profileCI_folded_normal_sigma <- NA
    } # if tails == 2
    # Show results in UI mainPanel
    removeNotification(notif_id)
    profileCI_title <- paste0("Profile CIs (",
                              round(100*v$profile_ci_confidence_level,input$round_to),
                              "% confidence)")
    output$profileCI_title <- renderText(profileCI_title)
    ci_tbl <- make_profile_ci_tbl(v$profileCI_std,v$profileCI_power,
                                  v$profileCI_folded_normal_mu,v$profileCI_folded_normal_sigma)
    v$profile_tbl <- ci_tbl
    v$profile_notes <- profile_table_notes()
    output$profileCI_tbl <- renderTable(ci_tbl, rownames = FALSE, digits = input$round_to)
    output$profile_notes <- render_strings_as_bullets( pcurveMix:::profile_table_notes() )

    l <- make_profile_plots(v$profileCI_std, v$profileCI_power, v$profileCI_folded_normal_mu, v$profileCI_folded_normal_sigma)
    output$profile_mu_plot <- renderPlot(l$mu)
    output$profile_sigma_plot <- renderPlot(l$sigma)
    output$profile_pi_plot <- renderPlot(l$pi)
    output$profile_power_plot <- renderPlot(l$power)
    if (fit_list$tails == 2) {
      output$profile_folded_normal_mu_plot <- renderPlot(l$folded_normal_mu)
      output$profile_folded_normal_sigma_plot <- renderPlot(l$folded_normal_sigma)
    }
    v$profile_mu_plot <- l$mu
    v$profile_sigma_plot <- l$sigma
    v$profile_pi_plot <- l$pi
    v$profile_power_plot <- l$power
    if (fit_list$tails == 2) {
      v$profile_folded_normal_mu_plot <- l$folded_normal_mu
      v$profile_folded_normal_sigma_plot <- l$folded_normal_sigma
    }
  } # profile_manager

  onStop(function() {
    # Nothing yet
  })

  observeEvent(input$p_file, {
    # req() prevents the code from running on app launch when input is NULL
    req(input$p_file)

    v$p_filename <- input$p_file$name
    v$df <- read.csv(input$p_file$datapath)
    check_input()
  })

  # observeEvent(input$btn_choose_analysis_options, {
  #   req(input$btn_choose_analysis_options)
  #   shinyjs::show("analysis_options_panel")
  # })

  observeEvent(input$btnUploadDemo, {
    demo_file_name <- pcurveMix:::DEMO_DATA_FILE_NAME
    full_file_path <- system.file("extdata", demo_file_name, package = "pcurveMix")
    v$p_filename <- demo_file_name
    v$df <- read.csv(full_file_path)
    check_input()
  })

  check_input <- function() {
    if ("p" %in% names(v$df)) {
      n_ps <- length(v$df$p)
      ps_min <- round(min(v$df$p),4)
      ps_max <- round(max(v$df$p),4)
      # showModal(modalDialog(
      #   title = "Upload successful",
      #   paste("File has",n_ps,"p's; min =",ps_min," & max =",ps_max),
      #   easyClose = TRUE,
      #   footer = modalButton("Choose analysis options")
      # ))
      # id <- "upload_notification"
      # showNotification(
      #   paste("Successful upload of",n_ps,"p's; min =",ps_min," & max =",ps_max),
      #   duration = 10,  # seconds
      #   closeButton = TRUE,
      #   type = "message",
      #   id = id, # Explicit ID stops rapid dismissal/conflict
      #   # Add a large, high-contrast dismiss button at the bottom
      #   action = actionButton(
      #     "dismiss_btn",
      #     "OK"
      #     # class = "btn-danger btn-sm",
      #     # style = "width: 100%; font-weight: bold; border-radius: 4px;"
      #   )
      # ) # showNotification
      output$report_on_ps <- renderText(paste("Successful upload of",n_ps,"p's; min =",ps_min," & max =",ps_max))
      shinyjs::show("analysis_options_panel")
      shinyjs::enable(id = "btnFit")
      # Following line jumps to analysis options; problematic because it hides input p's tails & alpha
      # shinyjs::runjs('document.getElementById("analysis_options_panel").scrollIntoView({behavior: "smooth", block: "start"});')
    } else {
      shiny::showModal(shiny::modalDialog(title = "No p values found",
                                          "Error: The file must contain a column named 'p'",
                                          easyClose = TRUE))
    } # else

    # Listen specifically for the custom dismiss button click
    observeEvent(input$dismiss_btn, {
      removeNotification(id)
#    }, once = TRUE) # once = TRUE ensures the observer cleans itself up
    }, ignoreInit = TRUE) # skips execution on app startup, but continues listening indefinitely for future user actions
    shinyjs::enable(id = "btn_choose_analysis_options")
  }   # end check_input

  get_alpha_cutoff <- function() {
    if (input$custom_cutoff2 == "custom") {
      req(input$custom_cutoff3)
      return(input$custom_cutoff3)
    } else {
      return(as.numeric(input$custom_cutoff2))
    }
  }

  observeEvent(input$btnFit, {
      restart()
      shinyjs::show("results_panel")
      shinyjs::show("download_panel")
      shinyjs::runjs('document.getElementById("results_panel").scrollIntoView({behavior: "smooth", block: "start"});')
      p_vec_to_fit <- v$df$p

      # output$model_fit_title <- renderText("Maximum-likelihood Fitting Summary")
      output$parameter_estimates_title <- renderText("Parameter estimates:")
      output$predicted_pdfs_title <- renderText("Observed/predicted PDFs:")
      output$predicted_cdfs_title <- renderText("Observed/predicted CDFs:")

      tails <- get_tails()
      alpha_cutoff <- get_alpha_cutoff()
      alpha_sig <- input$alpha_sig
      # if (input$specify_starting_values) {
      #   start_list <- list(mu = input$start_mu, sigma = input$start_sigma, pi = input$start_pi)
      # } else {
      #   start_list <- pcm_env$optim_starting_parms
      # }
      start_list <- pcm_env$optim_starting_parms

      v$fit_list <- pcurveMix::fit_p_curve(p_vec_to_fit, alpha = alpha_cutoff, tails = tails, alpha_sig = alpha_sig, start_parms = start_list)
      ps_in_bounds <- v$fit_list$check_ps_list$ps_in_bounds
      n_ps <- length(ps_in_bounds)
      v$descriptor_tbl <- pcurveMix::fit_to_descriptor_tbl(v$fit_list, file_name = v$p_filename)
      output$descriptor_tbl <- renderTable(v$descriptor_tbl, rownames = FALSE)
      v$estimates_tbl <- pcurveMix::fit_to_estimates_tbl(v$fit_list)
      # v$estimates_tbl[,-1] <- round(v$estimates_tbl[,-1],pcm_env$round_to) # Round numeric columns to avoid line wrapping
      output$estimates_tbl <- renderTable(v$estimates_tbl, rownames = FALSE, digits = input$round_to)
      v$estimates_notes <- pcurveMix:::estimates_table_notes(v$fit_list$converged)
      output$estimates_notes <- render_strings_as_bullets(v$estimates_notes)

      do_jackknifing()
      do_bootstrapping()
      do_npbootstrapping()

      profile_manager(v$fit_list)

      p_seq <- pcurveMix:::get_p_seq(v$fit_list$alpha)
      p_hist_bin_width <- v$fit_list$alpha / 50  # NEWJEFF: HARD-CODED
      v$p_seq_pdf <- p_seq # pcurveMix:::pcm_env$p_seq_pdf
      v$p_seq_cdf <- p_seq # pcurveMix:::pcm_env$p_seq_cdf
      v$pred_pdfs <- pdf(v$p_seq_pdf, mu = v$fit_list$mu, sigma = v$fit_list$sigma, pi = v$fit_list$pi,
                         alpha = alpha_cutoff, tails = tails)
      v$pred_cdfs <- cdf(v$p_seq_cdf, mu = v$fit_list$mu, sigma = v$fit_list$sigma, pi = v$fit_list$pi,
                         alpha = alpha_cutoff, tails = tails)

      v$pdf_plot <- ggplot2::ggplot() +
        ggplot2::geom_histogram(ggplot2::aes(x = ps_in_bounds, y = ggplot2::after_stat(density)), binwidth = p_hist_bin_width) +
        ggplot2::geom_line(ggplot2::aes(x = v$p_seq_pdf, y = v$pred_pdfs), color = "red") +
        ggplot2::labs(title = "Observed (black) vs predicted (red) PDFs",
                      x = "p value",
                      y = "density")
      output$pdf_plot <- renderPlot(v$pdf_plot)

      df2 <- data.frame(p = ps_in_bounds)
      v$cdf_plot <- ggplot2::ggplot() +
        ggplot2::stat_ecdf(data = df2, ggplot2::aes(x = p), geom = "step") +
        ggplot2::geom_line(ggplot2::aes(x = v$p_seq_cdf, y = v$pred_cdfs), color = "red") +
        ggplot2::labs(title = "Observed (black) vs predicted (red) CDFs",
                      x = "p value",
                      y = "cumulative proportion")
      output$cdf_plot <- renderPlot(v$cdf_plot)
      v$fit_completed <- TRUE
  }) # end observeEvent fit modelbutton

  # source("btn_gen_report.R")
  output$btnReport <- downloadHandler(

    filename = function() {
      in_file_name <- tools::file_path_sans_ext(basename(v$p_filename))
      outfile_name <- paste0(in_file_name,"_pcurveMix_report_",
                            format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".zip")
      return(outfile_name)
    },

    content = function(file) {
      # Check that data have been processed
      if (!v$fit_completed) {
        showNotification("You must fit the model before downloading the results.",
                         closeButton = TRUE)
      } else {
        id <- showNotification("Preparing report & csv files...", duration = NULL)
        # Path is relative to the Rmd folder
        output_directory_name <- "outputs"
        v_as_list <- reactiveValuesToList(v)
        # print(v_as_list)
        # # Create time stamp to mark output file names
        time_stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
        all_file_paths <- auto_shiny_files(v_as_list, output_directory_name, time_stamp = time_stamp)
        tails <- get_tails()

        # Render the rmd into the directory as well
        rmd = "pcurveMix_shiny_report.Rmd"
        params = list(
          boot_notes = v$boot_notes,
          boot_tbl = v$boot_tbl,
          cdf_plot = v$cdf_plot,
          descriptor_tbl = v$descriptor_tbl,
          doc_header = "Fit Ulrich & Miller (2026) p-Curve Mixture Model",
          estimates_notes = v$estimates_notes,
          estimates_tbl = v$estimates_tbl,
          jack_notes = v$jack_notes,
          jack_tbl = v$jack_tbl,
          n_boot_samples = v$n_boot_samples,
          n_jack_samples = v$n_jack_samples,
          np_boot_notes = v$np_boot_notes,
          np_boot_tbl = v$np_boot_tbl,
          np_n_boot_samples = v$np_n_boot_samples,
          p_filename = v$p_filename,
          pdf_plot = v$pdf_plot,
          want_profile_cis = v$want_profile_cis,
          profile_mu_plot = v$profile_mu_plot,
          profile_pi_plot = v$profile_pi_plot,
          profile_power_plot = v$profile_power_plot,
          profile_sigma_plot = v$profile_sigma_plot,
          profile_tbl = v$profile_tbl,
          profile_notes = v$profile_notes,
          round_to = input$round_to,
          tails = tails
        )
        if (tails == 2) {
          params <- c(params,
                      profile_folded_normal_mu_plot = v$profile_folded_normal_mu_plot,
                      profile_folded_normal_sigma_plot = v$profile_folded_normal_sigma_plot)
        }
        # Set the file extension based on user choice.
        # This is not needed for render, but it is needed to identify the
        # Rmd output file to zipr
        ext <- switch(input$rmd_format,
                      html = ".html",
                      htm = ".htm",
                      pdf = ".pdf",
                      docx = ".docx",
                      ".html")
        output_format <- get_render_output_format(input$rmd_format)
        rmd_outfile_name <- paste0(output_directory_name, "/",
                                   "pcurveMix_report_", time_stamp,ext)
        rmarkdown::render(rmd,
                          output_file = paste0(rmd_outfile_name),
                          output_format = output_format,
                          params = params,
                          envir = new.env(parent = globalenv()))

        all_file_paths <- c(all_file_paths, rmd_outfile_name)

        removeNotification(id)

        # Zip using the filename returned by function filename
        zip::zipr(file, all_file_paths)
        file.remove(all_file_paths)
        # A notification here appears before the user selects the download location.
        # Arranging for the notification to appear after the download finishes
        #  is prohibitively complicated and out of scope.
        # showNotification("Note that you can select a folder for the download. After download finishes, you can perform another analysis or quit.", duration = 45,
        #                  closeButton = TRUE)
      } # end of else after if (!v$fit_completed)
    },  # end content function

    # Required
    contentType = "application/zip"

  ) # end downloadHandler for btn_gen_report

  get_tails <- function() {
    selected_tails <- input$tails
    if (selected_tails == "1-tailed") {
      n_tails <- 1
    } else if (selected_tails == "2-tailed") {
      n_tails <- 2
    } else {
      stop("Unrecognized number of tails")
    }
    return(n_tails)
  }

  observeEvent(input$btnquit, {
    stopApp()
  })

} # end server function
