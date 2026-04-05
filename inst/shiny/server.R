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
                      fit_results_list = NA,
                      p_seq = NA,
                      pred_pdfs = NA,
                      pred_cdfs = NA,
                      descriptor_tbl = NA,
                      estimates_tbl = NA,
                      pdf_plot = NA,
                      cdf_plot = NA)

  package_path <- system.file(package = "pcurveMix")
  s <- paste0(package_path,"/extdata")
  output$shiny_examples_path <- renderText(s)

  observeEvent(input$btnFit, {
    v$p_filename <- input$p_file$name
    full_p_filename <- input$p_file$datapath
    if (is.null(full_p_filename)) {
      showNotification("You must upload a file of p's before fitting the model.")
    } else {
      df <- read.csv(full_p_filename)
      p_vec_to_fit <- df$p

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
      n_boot_samples <- as.numeric(input$n_boot_samples)
      if (n_boot_samples > 0) {
        boot_df <- bootstrap(n_ps, v$fit_results_list, n_boot_samples, alpha = alpha_cutoff, tails = tails)
        boot_tbl <- bootstrap_summary(boot_df)
        v$estimates_tbl <- merge_tables(v$estimates_tbl, boot_tbl)
      }
      v$estimates_tbl[,-1] <- round(v$estimates_tbl[,-1],3) # Round numeric columns to avoid line wrapping
      output$estimates_tbl <- renderTable(v$estimates_tbl, rownames = FALSE)

      v$p_seq <- seq(0.001,min(0.9999,alpha_cutoff),0.001)
      v$pred_pdfs <- pdf(v$p_seq, mu = v$fit_results_list$mu, sigma = v$fit_results_list$sigma, pi = v$fit_results_list$pi,
                         alpha = alpha_cutoff, tails = tails)
      v$pred_cdfs <- cdf(v$p_seq, mu = v$fit_results_list$mu, sigma = v$fit_results_list$sigma, pi = v$fit_results_list$pi,
                         alpha = alpha_cutoff, tails = tails)

      # pdf_plot <- ggplot() +
      #   geom_histogram(aes(x = sample_ps, y = after_stat(density)), binwidth = 0.02) +
      #   geom_line(aes(x = v$p_seq, y = pred_pdfs), color = "red")

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
        params = list(p_filename = v$p_filename,
          descriptor_tbl = v$descriptor_tbl, estimates_tbl = v$estimates_tbl,
          pdf_plot = v$pdf_plot, cdf_plot = v$cdf_plot)
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
