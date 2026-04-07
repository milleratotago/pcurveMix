# server.R: Define server logic required to draw a histogram

# Install pcurveMix from GitHub if necessary
if (!require("remotes")) install.packages("remotes")
if (!require(pcurveMix)) remotes::install_github("milleratotago/pcurveMix")
# remotes::install_github("milleratotago/pcurveMix")  # Reinstall if newer version but requires internet connection.
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(knitr)) install.packages('knitr')
if (!require(shinyFeedback)) install.packages('shinyFeedback')  # showNotification

server <- function(input, output) {

  v <- reactiveValues(fit_completed = FALSE,
                      p_filename = NULL,
                      fit_results_list = NULL,
                      p_seq = NULL,
                      pred_pdfs = NULL,
                      pred_cdfs = NULL,
                      descriptor_tbl = NULL,
                      estimates_tbl = NULL,
                      boot_tbl = NULL,
                      boot_pct_converged = NULL,
                      pdf_plot = NULL,
                      cdf_plot = NULL)

  restart <- function()  {
    v$fit_completed = FALSE
    v$p_filename <- NULL
    v$fit_results_list <- NULL
    v$p_seq <- NULL
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
      showNotification("You must upload a file of p's before fitting the model.")
    } else {
      df <- read.csv(full_p_filename)
      p_vec_to_fit <- df$p

      output$model_fit_title <- renderText("Maximum-likelihood Fitting Summary")
      output$parameter_estimates_title <- renderText("Parameter estimates:")
      output$predicted_pdfs_title <- renderText("Observed/predicted PDFs:")
      output$predicted_cdfs_title <- renderText("Observed/predicted CDFs:")

      tails <- get_tails()
      alpha_cutoff <- as.numeric(input$custom_cutoff)
      alpha_sig <- as.numeric(input$alpha_sig)
      # Note: triple colon operator allows app to see un-exported package functions.
      l <- pcurveMix:::check_ps(p_vec_to_fit, alpha_cutoff = alpha_cutoff)
      if (!l$all_in_bounds) {
        problem_string <- pcurveMix:::bad_ps_report_string(l)
        showNotification(problem_string, type = "warning", duration = 15)
        p_vec_to_fit <- l$ps_in_bounds
      }
      n_ps <- length(p_vec_to_fit)

      v$fit_results_list <- pcurveMix::fit_p_curve(p_vec_to_fit, alpha = alpha_cutoff, tails = tails, alpha_sig = alpha_sig)
      v$descriptor_tbl <- pcurveMix::fit_to_descriptor_tbl(v$fit_results_list, file_name = v$p_filename)
      output$descriptor_tbl <- renderTable(v$descriptor_tbl, rownames = FALSE)
      v$estimates_tbl <- pcurveMix::fit_to_estimates_tbl(v$fit_results_list)
      v$n_boot_samples <- as.numeric(input$n_boot_samples)
      if (v$n_boot_samples > 0) {
        boot_df <- bootstrap(n_ps, v$fit_results_list, v$n_boot_samples, alpha = alpha_cutoff, tails = tails)
        boot_list <- make_bootstrap_summary_list(boot_df, v$estimates_tbl)
        v$boot_pct_converged <- boot_list$pct_converged
        v$boot_tbl <- boot_list$boot_tbl
        v$boot_tbl[,-1] <- round(v$boot_tbl[,-1],3) # Round numeric columns to avoid line wrapping
        output$bootstrap_title <- renderText("Additional bootstrapping analysis")
        s1 <- paste0("* n bootstrap samples = ",v$n_boot_samples)
        output$n_boot_samples <- renderText(s1)
        s2 <- paste0("* percent converged = ",round(v$boot_pct_converged,2))
        output$boot_pct_converged <- renderText(s2)
        output$bootstrap_tbl <- renderTable(v$boot_tbl, rownames = FALSE)
      }
      v$estimates_tbl[,-1] <- round(v$estimates_tbl[,-1],3) # Round numeric columns to avoid line wrapping
      output$estimates_tbl <- renderTable(v$estimates_tbl, rownames = FALSE)

      v$p_seq <- seq(0.001,min(0.9999,alpha_cutoff),0.001)
      v$pred_pdfs <- pdf(v$p_seq, mu = v$fit_results_list$mu, sigma = v$fit_results_list$sigma, pi = v$fit_results_list$pi,
                         alpha = alpha_cutoff, tails = tails)
      v$pred_cdfs <- cdf(v$p_seq, mu = v$fit_results_list$mu, sigma = v$fit_results_list$sigma, pi = v$fit_results_list$pi,
                         alpha = alpha_cutoff, tails = tails)

      v$pdf_plot <- ggplot() +
        geom_histogram(aes(x = p_vec_to_fit, y = after_stat(density)), binwidth = 0.02) +
        geom_line(aes(x = v$p_seq, y = v$pred_pdfs), color = "red") +
        labs(title = "Observed (black) vs predicted (red) PDFs",
             x = "p value",
             y = "density")
      output$pdf_plot <- renderPlot(v$pdf_plot)

      df2 <- data.frame(p = p_vec_to_fit)
      v$cdf_plot <- ggplot() +
        stat_ecdf(data = df2, aes(x = p), geom = "step") +
        geom_line(aes(x = v$p_seq, y = v$pred_cdfs), color = "red") +
        labs(title = "Observed (black) vs predicted (red) CDFs",
             x = "p value",
             y = "cumulative proportion")
      output$cdf_plot <- renderPlot(v$cdf_plot)
      v$fit_completed <- TRUE
    } # end else (file name not null)
  }) # end observeEvent fit modelbutton

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
        showNotification("You must fit the model before downloading the results.")
      } else {
        # Create time stamp to mark output file names
        time_stamp <- timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

        # Path is relative to the Rmd folder
        output_directory_name <- "outputs"
        if (!dir.exists(output_directory_name)) dir.create(output_directory_name)

        # Write CSV file of predicted pdf and cdf values
        csv_outfile_name <- paste0(output_directory_name, "/",
                                   "pred_pdf_cdf_", time_stamp, ".csv")
        pred_pdf_cdf <- data.frame(p = v$p_seq, pdf = v$pred_pdfs, cdf = v$pred_cdfs)
        write.csv(pred_pdf_cdf, csv_outfile_name, row.names = FALSE)

        # Render the rmd into the directory as well
        rmd = "pcurveMix_shiny_report.Rmd"
        params = list(
          p_filename = v$p_filename,
          descriptor_tbl = v$descriptor_tbl,
          estimates_tbl = v$estimates_tbl,
          pdf_plot = v$pdf_plot,
          cdf_plot = v$cdf_plot,
          n_boot_samples = v$n_boot_samples,
          boot_pct_converged = v$boot_pct_converged,
          boot_tbl = v$boot_tbl)
        rmd_outfile_name <- paste0(output_directory_name, "/",
                                   "pcurveMix_report_", time_stamp, ".docx")
        rmarkdown::render(rmd,
                          output_file = paste0(rmd_outfile_name),
                          params = params,
                          envir = new.env(parent = globalenv()))

        all_file_paths <- c(csv_outfile_name, rmd_outfile_name)

        # Zip using the filename returned by function filename
        zip::zipr(file, all_file_paths)
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

} # end server function
