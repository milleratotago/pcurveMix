# server.R

# Note: This file uses pcurveMix::: references to unexported pcurveMix
#  constants and functions; the shiny can't see those otherwise.

# # Install pcurveMix from GitHub if necessary
# if (!require("remotes")) install.packages("remotes")
# if (!require(pcurveMix)) remotes::install_github("milleratotago/pcurveMix")
# remotes::install_github("milleratotago/pcurveMix")  # Reinstall if newer version but requires internet connection.
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(knitr)) install.packages('knitr')
if (!require(shinyFeedback)) install.packages('shinyFeedback')  # showNotification
if (!require(profileCI)) install.packages('profileCI')

server <- function(input, output) {

  v <- reactiveValues(fit_completed = FALSE,
                      p_filename = NULL,
                      fit_results_list = NULL,
                      p_seq_pdf = NULL,
                      p_seq_cdf = NULL,
                      pred_pdfs = NULL,
                      pred_cdfs = NULL,
                      descriptor_tbl = NULL,
                      estimates_tbl = NULL,
                      boot_tbl = NULL,
                      boot_pct_converged = NULL,
                      pdf_plot = NULL,
                      cdf_plot = NULL,
                      profile_list = NULL)

  restart <- function()  {
    v$fit_completed = FALSE
    v$p_filename <- NULL
    v$fit_results_list <- NULL
    v$p_seq_pdf <- NULL
    v$p_seq_cdf <- NULL
    v$pred_pdfs <- NULL
    v$pred_cdfs <- NULL
    v$descriptor_tbl <- NULL
    v$estimates_tbl <- NULL
    v$boot_tbl <- NULL
    v$boot_pct_converged <- NULL
    v$pdf_plot <- NULL
    v$cdf_plot <- NULL
    output$model_fit_title <- renderText(NULL)
    output$parameter_estimates_title <- renderText(NULL)
    output$predicted_pdfs_title <- renderText(NULL)
    output$predicted_cdfs_title <- renderText(NULL)
    output$bootstrap_title <- renderText(NULL)
    output$n_boot_samples <- renderText(NULL)
    output$boot_pct_converged <- renderText(NULL)
    output$descriptor_tbl <- renderTable(NULL, rownames = FALSE)
    output$estimates_tbl <- renderTable(NULL, rownames = FALSE)
    output$boot_tbl <- renderTable(NULL, rownames = FALSE)
    output$pdf_plot <- renderPlot(NULL)
    output$cdf_plot <- renderPlot(NULL)
  } # restart

  observeEvent(input$btnFit, {
    restart()
    if (input$use_demo) {
      package_path <- system.file(package = "pcurveMix")
      v$p_filename <- "sample_ps.csv"
      full_p_filename <- paste0(package_path,"/extdata/",v$p_filename)
    } else {
      v$p_filename <- input$p_file$name
      full_p_filename <- input$p_file$datapath
    }
    if (is.null(full_p_filename)) {
      showNotification("You must upload a file of p's before fitting the model.",
                       closeButton = TRUE)
    } else {
      df <- read.csv(full_p_filename)
      p_vec_to_fit <- df$p

      output$model_fit_title <- renderText("Maximum-likelihood Fitting Summary")
      output$parameter_estimates_title <- renderText("Parameter estimates:")
      output$predicted_pdfs_title <- renderText("Observed/predicted PDFs:")
      output$predicted_cdfs_title <- renderText("Observed/predicted CDFs:")

      tails <- get_tails()
      alpha_cutoff <- input$custom_cutoff
      alpha_sig <- input$alpha_sig
      start_list <- list(mu = input$start_mu, sigma = input$start_sigma, pi = input$start_pi)

      v$fit_results_list <- pcurveMix::fit_p_curve(p_vec_to_fit, alpha = alpha_cutoff, tails = tails, alpha_sig = alpha_sig, start_parms = start_list)
      ps_in_bounds <- v$fit_results_list$check_ps_list$ps_in_bounds
      n_ps <- length(ps_in_bounds)
      v$descriptor_tbl <- pcurveMix::fit_to_descriptor_tbl(v$fit_results_list, file_name = v$p_filename)
      output$descriptor_tbl <- renderTable(v$descriptor_tbl, rownames = FALSE)
      v$estimates_tbl <- pcurveMix::fit_to_estimates_tbl(v$fit_results_list)
      if (input$parametric_bootstrapping) {
        v$n_boot_samples <- input$n_boot_samples
        v$boot_ci_confidence_level <- input$boot_confidence_level / 100
      } else {
        v$n_boot_samples <- 0
      }
      if (v$n_boot_samples > 0) {
        notif_id <- "bootstrap_notif_id"
        # NewJeff: It is possible to update the notification by showing one
        # with the same id but a different message reflecting progress.
        showNotification(
          "Bootstrapping in progress ...",
          id = notif_id,
          duration = NULL,
          closeButton = TRUE,
          type = "message"
        )
        boot_df <- pcurveMix::bootstrap(n_ps, v$fit_results_list, v$n_boot_samples) # , alpha = alpha_cutoff, tails = tails, alpha_sig = alpha_sig)
        boot_tail_prob <- (1 - v$boot_ci_confidence_level)/2
        boot_list <- make_bootstrap_summary_list(boot_df, v$estimates_tbl,
                                                 boot_ci_limits = c(boot_tail_prob, 1-boot_tail_prob) )
        removeNotification(notif_id)
        v$boot_pct_converged <- boot_list$pct_converged
        v$boot_tbl <- boot_list$boot_tbl
        v$boot_tbl[,-1] <- round(v$boot_tbl[,-1],3) # Round numeric columns to avoid line wrapping
        boot_title <- paste0("Parametric bootstrapping analysis (",
                             round(100*v$boot_ci_confidence_level,2),
                             "% confidence)")
        output$bootstrap_title <- renderText(boot_title)
        s1 <- paste0("* n bootstrap samples = ",v$n_boot_samples)
        output$n_boot_samples <- renderText(s1)
        s2 <- paste0("* percent converged OK = ",round(v$boot_pct_converged,2))
        output$boot_pct_converged <- renderText(s2)
        output$bootstrap_tbl <- renderTable(v$boot_tbl, rownames = FALSE)
      }
      profile_manager(v$fit_results_list)
      v$estimates_tbl[,-1] <- round(v$estimates_tbl[,-1],3) # Round numeric columns to avoid line wrapping
      output$estimates_tbl <- renderTable(v$estimates_tbl, rownames = FALSE)

      v$p_seq_pdf <- pcurveMix:::pcm_env$p_seq_pdf
      v$p_seq_cdf <- pcurveMix:::pcm_env$p_seq_cdf
      v$pred_pdfs <- pdf(v$p_seq_pdf, mu = v$fit_results_list$mu, sigma = v$fit_results_list$sigma, pi = v$fit_results_list$pi,
                         alpha = alpha_cutoff, tails = tails)
      v$pred_cdfs <- cdf(v$p_seq_cdf, mu = v$fit_results_list$mu, sigma = v$fit_results_list$sigma, pi = v$fit_results_list$pi,
                         alpha = alpha_cutoff, tails = tails)

      v$pdf_plot <- ggplot() +
        geom_histogram(aes(x = ps_in_bounds, y = after_stat(density)), binwidth = 0.02) +
        geom_line(aes(x = v$p_seq_pdf, y = v$pred_pdfs), color = "red") +
        labs(title = "Observed (black) vs predicted (red) PDFs",
             x = "p value",
             y = "density")
      output$pdf_plot <- renderPlot(v$pdf_plot)

      df2 <- data.frame(p = ps_in_bounds)
      v$cdf_plot <- ggplot() +
        stat_ecdf(data = df2, aes(x = p), geom = "step") +
        geom_line(aes(x = v$p_seq_cdf, y = v$pred_cdfs), color = "red") +
        labs(title = "Observed (black) vs predicted (red) CDFs",
             x = "p value",
             y = "cumulative proportion")
      output$cdf_plot <- renderPlot(v$cdf_plot)
      v$fit_completed <- TRUE
    } # end else (file name not null)
  }) # end observeEvent fit modelbutton

  profile_manager <- function(fit_list) {
    if (!input$profile_ci) {
      v$profile_analysis <- 0  # Needed to pass to Rmd
      return(NULL)
    } else {
      v$profile_analysis <- 1
    }
    # Computations:
    v$profile_ci_confidence_level <- input$profile_confidence_level / 100
    notif_id <- "profileCI_std_notif_id"
    showNotification(
      "Profiling mu, sigma, and pi ...",
      id = notif_id,
      duration = NULL,
      closeButton = TRUE,
      type = "message"
    )
    v$profileCI_std <- pcurveMix::compute_profileCI(fit_list, level = v$profile_ci_confidence_level)
    showNotification(
      "Profiling power ...",
      id = notif_id,
      duration = NULL,
      closeButton = TRUE,
      type = "message"
    )
    v$profileCI_power <- compute_profileCI_power(fit_list, level = v$profile_ci_confidence_level)
    if (fit_list$tails == 2) {
      showNotification(
        paste("Profiling",pcurveMix:::FOLDED_NORMAL_MU_LABEL,"..."),
        id = notif_id,
        duration = NULL,
        closeButton = TRUE,
        type = "message"
      )
      v$profileCI_folded_normal_mu <- compute_profileCI_folded(fit_list, TRUE, level = v$profile_ci_confidence_level)
      showNotification(
        paste("Profiling",pcurveMix:::FOLDED_NORMAL_SIGMA_LABEL,"..."),
        id = notif_id,
        duration = NULL,
        closeButton = TRUE,
        type = "message"
      )
      v$profileCI_folded_normal_sigma <- compute_profileCI_folded(fit_list, FALSE, level = v$profile_ci_confidence_level)
    } # if tails == 2
    # Show results in UI mainPanel
    removeNotification(notif_id)
    profileCI_title <- paste0("Profile CIs (",
                              round(100*v$profile_ci_confidence_level,2),
                              "% confidence)")
    output$profileCI_title <- renderText(profileCI_title)
    # tbl <- v$profileCI_std$tabl
    # v$profileCI_tbl <- tbl
    ci_tbl <- data.frame(Parameter = c("mu", "sigma", "pi"))
    ci_tbl <- cbind(ci_tbl,v$profileCI_std$bounds_matrix)
    names(ci_tbl) <- c("Parameter", pcurveMix:::CI_LOWER_BOUND_LABEL, pcurveMix:::CI_UPPER_BOUND_LABEL)

    power_row <- data.frame(Parameter = "power",
                            c2 = v$profileCI_power$table$`95% CI lower`,
                            c3 = v$profileCI_power$table$`95% CI upper`)
    names(power_row) <- c("Parameter", pcurveMix:::CI_LOWER_BOUND_LABEL, pcurveMix:::CI_UPPER_BOUND_LABEL)
    ci_tbl <- rbind(ci_tbl, power_row)

    if (fit_list$tails == 2) {
      folded_normal_mu_row <- data.frame(Parameter = pcurveMix:::FOLDED_NORMAL_MU_LABEL,
                                         c2 = v$profileCI_folded_normal_mu$table$`95% CI lower`,
                                         c3 = v$profileCI_folded_normal_mu$table$`95% CI upper`)
      names(folded_normal_mu_row) <- c("Parameter", pcurveMix:::CI_LOWER_BOUND_LABEL, pcurveMix:::CI_UPPER_BOUND_LABEL)
      ci_tbl <- rbind(ci_tbl, folded_normal_mu_row)

      folded_normal_sigma_row <- data.frame(Parameter = pcurveMix:::FOLDED_NORMAL_SIGMA_LABEL,
                                            c2 = v$profileCI_folded_normal_sigma$table$`95% CI lower`,
                                            c3 = v$profileCI_folded_normal_sigma$table$`95% CI upper`)
      names(folded_normal_sigma_row) <- c("Parameter", pcurveMix:::CI_LOWER_BOUND_LABEL, pcurveMix:::CI_UPPER_BOUND_LABEL)
      ci_tbl <- rbind(ci_tbl, folded_normal_sigma_row)
    } # if tails == 2
    output$profileCI_tbl <- renderTable(ci_tbl, rownames = FALSE)

    # ProfileCI plots
    # output$profile_mu_title <- renderText("profile for mu")

    # Interesting: you can't re-use plain x & y across multiple ggplots.
    # If you do, all plots show the final x & y values.
    mu_x <- v$profileCI_std$profile_curves$mu[,1]
    mu_y <- v$profileCI_std$profile_curves$mu[,2]
    v$profile_mu_plot <- ggplot() +
      geom_line(aes(x = mu_x, y = mu_y), color = "black") +
      labs(title = "profile for mu",
           x = "mu",
           y = pcurveMix:::LIKELIHOOD_LABEL)
    output$profile_mu_plot <- renderPlot(v$profile_mu_plot)

    sigma_x <- v$profileCI_std$profile_curves$sigma[,1]
    sigma_y <- v$profileCI_std$profile_curves$sigma[,2]
    v$profile_sigma_plot <- ggplot() +
      geom_line(aes(x = sigma_x, y = sigma_y), color = "black") +
      labs(title = "profile for sigma",
           x = "sigma",
           y = pcurveMix:::LIKELIHOOD_LABEL)
    output$profile_sigma_plot <- renderPlot(v$profile_sigma_plot)

    pi_x <- v$profileCI_std$profile_curves$pi[,1]
    pi_y <- v$profileCI_std$profile_curves$pi[,2]
    v$profile_pi_plot <- ggplot() +
      geom_line(aes(x = pi_x, y = pi_y), color = "black") +
      labs(title = "profile for pi",
           x = "pi",
           y = pcurveMix:::LIKELIHOOD_LABEL)
    output$profile_pi_plot <- renderPlot(v$profile_pi_plot)

    profile_curves <- as.matrix(attr(v$profileCI_power$profile,"for_plot")[["logit_relative_power"]])
    power_x <- pcurveMix:::reals_to_powers(profile_curves[,1])
    power_y <- profile_curves[,2]
    v$profile_power_plot <- ggplot() +
      geom_line(aes(x = power_x, y = power_y), color = "black") +
      labs(title = "profile for power",
           x = "power",
           y = pcurveMix:::LIKELIHOOD_LABEL)
    output$profile_power_plot <- renderPlot(v$profile_power_plot)

    if (fit_list$tails == 2) {
      profile_curves <- as.matrix(attr(v$profileCI_folded_normal_mu$profile,"for_plot")[["log_folded_normal_mu"]])
      folded_normal_mus_x <- pcurveMix:::reals_to_mus(profile_curves[,1])
      folded_normal_mus_y <- profile_curves[,2]
      v$profile_folded_normal_mu_plot <- ggplot() +
        geom_line(aes(x = folded_normal_mus_x, y = folded_normal_mus_y), color = "black") +
        labs(title = paste("profile for",pcurveMix:::FOLDED_NORMAL_MU_LABEL),
             x = pcurveMix:::FOLDED_NORMAL_MU_LABEL,
             y = pcurveMix:::LIKELIHOOD_LABEL)
      output$profile_folded_normal_mu_plot <- renderPlot(v$profile_folded_normal_mu_plot)

      profile_curves <- as.matrix(attr(v$profileCI_folded_normal_sigma$profile,"for_plot")[["log_folded_normal_sigma"]])
      folded_normal_sigma_x <- pcurveMix:::reals_to_sigmas(profile_curves[,1])
      folded_normal_sigma_y <- profile_curves[,2]
      v$profile_folded_normal_sigma_plot <- ggplot() +
        geom_line(aes(x = folded_normal_sigma_x, y = folded_normal_sigma_y), color = "black") +
        labs(title = paste("profile for",pcurveMix:::FOLDED_NORMAL_SIGMA_LABEL),
             x = pcurveMix:::FOLDED_NORMAL_SIGMA_LABEL,
             y = pcurveMix:::LIKELIHOOD_LABEL)
      output$profile_folded_normal_sigma_plot <- renderPlot(v$profile_folded_normal_sigma_plot)
    } # if tails == 2
  } # profile_manager

  # source("btn_gen_report.R")
  output$btnReport <- downloadHandler(

    filename = function() {
      outfile_name = paste0("pcurveMix_report_",
                            format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".zip")
      return(outfile_name)
    },

    content = function(file) {
      # Check that data have been processed
      if (!v$fit_completed) {
        showNotification("You must fit the model before downloading the results.",
                         closeButton = TRUE)
      } else {
        tails <- get_tails()

        # Create time stamp to mark output file names
        time_stamp <- timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

        id <- showNotification("Preparing report & csv files...", duration = NULL)
        # Path is relative to the Rmd folder
        output_directory_name <- "outputs"
        if (!dir.exists(output_directory_name)) dir.create(output_directory_name)

        # Write CSV files of predicted pdf and cdf values
        csv_pdf_outfile_name <- paste0(output_directory_name, "/",
                                       "pred_pdf_", time_stamp, ".csv")
        pred_pdf <- data.frame(p = v$p_seq_pdf, pdf = v$pred_pdfs)
        write.csv(pred_pdf, csv_pdf_outfile_name, row.names = FALSE)
        csv_cdf_outfile_name <- paste0(output_directory_name, "/",
                                       "pred_cdf_", time_stamp, ".csv")
        pred_cdf <- data.frame(p = v$p_seq_cdf, cdf = v$pred_cdfs)
        write.csv(pred_cdf, csv_cdf_outfile_name, row.names = FALSE)

        # Write CSV files of profile curves if these were requested:
        if (input$profile_ci) {

          csv_profile_mu_outfile_name <- paste0(output_directory_name, "/",
                                                "profile_mu_", time_stamp, ".csv")
          profile_mu <- data.frame(mu = v$profileCI_std$profile_curves$mu[,1],
                                   likelihood = v$profileCI_std$profile_curves$mu[,2])
          write.csv(profile_mu, csv_profile_mu_outfile_name, row.names = FALSE)

          csv_profile_sigma_outfile_name <- paste0(output_directory_name, "/",
                                                   "profile_sigma_", time_stamp, ".csv")
          profile_sigma <- data.frame(sigma = v$profileCI_std$profile_curves$sigma[,1],
                                      likelihood = v$profileCI_std$profile_curves$sigma[,2])
          write.csv(profile_sigma, csv_profile_sigma_outfile_name, row.names = FALSE)

          csv_profile_pi_outfile_name <- paste0(output_directory_name, "/",
                                                "profile_pi_", time_stamp, ".csv")
          profile_pi <- data.frame(pi = v$profileCI_std$profile_curves$pi[,1],
                                   likelihood = v$profileCI_std$profile_curves$pi[,2])
          write.csv(profile_pi, csv_profile_pi_outfile_name, row.names = FALSE)

          # NEWJEFF: I WANT POWER, FOLDED_MU, FOLDED_SIGMA in natural units
          csv_profile_power_outfile_name <- paste0(output_directory_name, "/",
                                                   "profile_power_", time_stamp, ".csv")
          temp_mat <- extract_profile_plot_columns(v$profileCI_power,1) # 1st profiled parm
          temp_mat[,1] <- pcurveMix:::reals_to_powers(temp_mat[,1])
          profile_power <- data.frame(power = temp_mat[,1],
                                      likelihood = temp_mat[,2])
          write.csv(profile_power, csv_profile_power_outfile_name, row.names = FALSE)

          if (tails == 2) {
            csv_profile_folded_normal_mu_outfile_name <- paste0(output_directory_name, "/",
                                                                "profile_folded_normal_mu_", time_stamp, ".csv")
            temp_mat <- extract_profile_plot_columns(v$profileCI_folded_normal_mu,1) # 1st profiled parm
            temp_mat[,1] <- pcurveMix:::reals_to_mus(temp_mat[,1])
            profile_folded_normal_mu <- data.frame(folded_normal_mu = temp_mat[,1],
                                                   likelihood = temp_mat[,2])
            write.csv(profile_folded_normal_mu, csv_profile_folded_normal_mu_outfile_name, row.names = FALSE)

            csv_profile_folded_normal_sigma_outfile_name <- paste0(output_directory_name, "/",
                                                                   "profile_folded_normal_sigma_", time_stamp, ".csv")
            temp_mat <- extract_profile_plot_columns(v$profileCI_folded_normal_sigma,1) # 1st profiled parm
            temp_mat[,1] <- pcurveMix:::reals_to_sigmas(temp_mat[,1])
            profile_folded_normal_sigma <- data.frame(folded_normal_sigma = temp_mat[,1],
                                                      likelihood = temp_mat[,2])
            write.csv(profile_folded_normal_sigma, csv_profile_folded_normal_sigma_outfile_name, row.names = FALSE)
          } # if tails == 2
        } # if (input$profile_ci)

        # Render the rmd into the directory as well
        rmd = "pcurveMix_shiny_report.Rmd"
        params = list(
          p_filename = v$p_filename,
          tails = tails,
          descriptor_tbl = v$descriptor_tbl,
          estimates_tbl = v$estimates_tbl,
          pdf_plot = v$pdf_plot,
          cdf_plot = v$cdf_plot,
          n_boot_samples = v$n_boot_samples,
          boot_pct_converged = v$boot_pct_converged,
          boot_tbl = v$boot_tbl,
          profile_analysis = v$profile_analysis,
          profile_tbl = v$profile_tbl,
          profile_mu_plot = v$profile_mu_plot,
          profile_sigma_plot = v$profile_sigma_plot,
          profile_pi_plot = v$profile_pi_plot,
          profile_power_plot = v$profile_power_plot
        )
        if (tails == 2) {
          params <- c(params,
                      profile_folded_normal_mu_plot = v$profile_folded_normal_mu_plot,
                      profile_folded_normal_sigma_plot = v$profile_folded_normal_sigma_plot)
        }
        # Dynamically set the file extension based on user choice
        ext <- switch(input$rmd_format,
                      html = ".html",
                      pdf = ".pdf",
                      docx = ".docx")
        output_format <- switch(input$rmd_format,
                                html = "html_document",
                                pdf = "pdf_document",
                                docx = "word_document")
        rmd_outfile_name <- paste0(output_directory_name, "/",
                                   "pcurveMix_report_", time_stamp, ext)
        rmarkdown::render(rmd,
                          output_file = paste0(rmd_outfile_name),
                          output_format = output_format,
                          params = params,
                          envir = new.env(parent = globalenv()))

        all_file_paths <- c(csv_pdf_outfile_name, csv_cdf_outfile_name, rmd_outfile_name)
        if (input$profile_ci) {
          all_file_paths <- c(csv_pdf_outfile_name, csv_cdf_outfile_name,
                              csv_profile_mu_outfile_name, csv_profile_sigma_outfile_name,
                              csv_profile_pi_outfile_name, csv_profile_power_outfile_name,
                              rmd_outfile_name)
          if (tails == 2) {  # folded normal only for 2-tailed
            all_file_paths <- c(all_file_paths, csv_profile_folded_normal_mu_outfile_name, csv_profile_folded_normal_sigma_outfile_name)
          }
        } else {
          all_file_paths <- c(csv_pdf_outfile_name, csv_cdf_outfile_name, rmd_outfile_name)
        }

        removeNotification(id)

        # Zip using the filename returned by function filename
        zip::zipr(file, all_file_paths)
        file.remove(all_file_paths)
        showNotification("Note that you can select a folder for the download. After download finishes, you can perform another analysis or quit.", duration = 45,
                         closeButton = TRUE)
      } # end of else
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
