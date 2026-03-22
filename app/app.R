#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
# Install pcurveMix from GitHub if necessary
if (!require(pcurveMix)) remotes::install_github("milleratotago/pcurveMix")
library(pcurveMix)
if (!require(shinyjs)) install.packages('shinyjs')
library(shinyjs)
if (!require(knitr)) install.packages('knitr')
library(knitr)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

# # Load up the code script files
# script_files <- list.files("R")
# for (script in script_files)
# {
#   file_name <- paste0("R/", script)
#   source(file_name)
# }

# Load the script that defines object ui
source("ui.R")

# Define server logic required to draw a histogram
server <- function(input, output) {

  v <- reactiveValues(fit_completed = FALSE,
                      descriptor_tbl = NA,
                      estimates_tbl = NA,
                      pdf_plot = NA,
                      cdf_plot = NA)

  # observeEvent(input$cond,)
  observeEvent(input$btnFit, {
    p_filename <- input$p_file$datapath
    df <- read.csv(p_filename)

    # print(df) # NEWJEFF

    # Describe input p's:
    n_ps <- nrow(df)
    min_p <- min(df$p)
    max_p <- max(df$p)
    # cat("File has",n_ps,"p values with min=",min_p,"and max=",max_p,"\n")
    output$input_ps <- renderText({
      paste("File has",n_ps,"p values with min =",min_p,"and max =",max_p)
    })

    tails <- get_tails()
    # cat("found",tails,"tails.\n")

    alpha_cutoff <- get_cutoff()
    if (sum(df$p>alpha_cutoff) > 0) stop("Cannot fit data with p values greater than cutoff")

    fit_results_list <- fit_p_curve(df$p, alpha = alpha_cutoff, tails = tails)
    v$descriptor_tbl <- fit_to_descriptor_tbl(fit_results_list)
    output$descriptor_tbl <- renderTable(v$descriptor_tbl, rownames = FALSE)
    v$estimates_tbl <- fit_to_estimates_tbl(fit_results_list)
    n_boot_samples <- as.numeric(input$n_boot_samples)
    if (n_boot_samples > 0) {
      boot_df <- bootstrap(n_ps, fit_results_list, n_boot_samples, alpha = alpha_cutoff, tails = tails)
      boot_tbl <- bootstrap_summary(boot_df)
      v$estimates_tbl <- merge_tables(v$estimates_tbl, boot_tbl)
    }
    v$estimates_tbl[,-1] <- round(v$estimates_tbl[,-1],3) # Round numeric columns to avoid line wrapping
    output$estimates_tbl <- renderTable(v$estimates_tbl, rownames = FALSE)

    p <- seq(0.001,alpha_cutoff,0.002)
    pred_pdfs <- pdf(p, mu = fit_results_list$mu, sigma = fit_results_list$sigma, pi = fit_results_list$pi,
                     alpha = alpha_cutoff, tails = tails)
    pred_cdfs <- cdf(p, mu = fit_results_list$mu, sigma = fit_results_list$sigma, pi = fit_results_list$pi,
                     alpha = alpha_cutoff, tails = tails)
    output$pdf_plot <- renderPlot({
      hist(df$p, freq = FALSE, xlim = c(0, alpha_cutoff) )
      lines(p, pred_pdfs, col = "red")
    })

    cdf_df <- data.frame(x = df$p)  # NEWJEFF
    v$cdf_plot <- ggplot() +
      stat_ecdf(data = cdf_df, aes(x = x), geom = "step") +
      geom_line(aes(x = p, y = pred_cdfs), color = "red") +
      labs(title = "Observed (black) vs predicted (red) CDFs",
           x = "p value",
           y = "cumulative proportion")
    output$cdf_plot <- renderPlot(v$cdf_plot)
    # output$cdf_plot <- renderPlot({
    #   empirical <- ecdf(df$p)
    #   plot(empirical, main = "Observed vs Predicted CDF", xlab = "p value", ylab = "CDF(p)",
    #        xlim = c(0,1), ylim = c(0,alpha_cutoff))
    #   pred_cdfs <- cdf(p, mu = fit_results_list$mu, sigma = fit_results_list$sigma, pi = fit_results_list$pi,
    #                    alpha = alpha_cutoff, tails = tails)
    #   lines(p, pred_cdfs, col = "red", lty = 1)
    #   legend(
    #     "bottomright",
    #     legend = c("observed", "predicted"),
    #     lwd = 2,
    #     lty = c(1, 1),
    #     col = c("black", "red")
    #   )
    # })
    v$fit_completed <- TRUE
  }) # end observeEvent fit modelbutton

  # source("btn_gen_report.R")
  output$btnReport <- downloadHandler(

    filename = function()
    {
      outfile_name = paste0("pcurveMix_report_",
                            format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".zip")
      print(outfile_name)

      return(outfile_name)
    },

    content = function(file)
    {

      # !!! ToDo check that data have been processed

      # #   # Returns a list of all files generated
      #
      # Create time stamp to mark output file names
      time_stamp <- timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
      #
      # #output_directory_name <- paste("outputs_", time_stamp, sep = "")
      # output_directory_name <- "outputs"
      # dir.create(output_directory_name)
      #
      # full_outfile_name <- paste0(output_directory_name, "/",
      #                             "hs_admissions_recommendations_", time_stamp, ".csv")
      #
      # write.csv(v$applicant_all_df, full_outfile_name)

      # =============================================
      # Render the rmd into the directory as well
      # =============================================
      rmd = "pcurveMix_report.Rmd"
      params = list(
        descriptor_tbl = v$descriptor_tbl, estimates_tbl = v$estimates_tbl,
        pdf_plot = v$pdf_plot, cdf_plot = v$cdf_plot)

      # Path is relative to the Rmd folder
      output_directory_name <- "outputs"
      dir.create(output_directory_name)
      rmd_outfile_name <- paste0(output_directory_name, "/",
                                 "pcurveMix_report_", time_stamp, ".docx")
      rmarkdown::render(rmd,
                        output_file = paste0(rmd_outfile_name),
                        params = params,
                        envir = new.env(parent = globalenv()))


      # all_files <- list.files(output_directory_name)
      # all_file_paths <- paste0(output_directory_name, "/", all_files)

      all_file_paths <- c(rmd_outfile_name) # c(full_outfile_name, rmd_outfile_name)

      # Zip using the filename returned by function filename
      zip::zipr(file, all_file_paths)
    },

    # Required
    contentType = "application/zip"

  ) # end downloadHandler for btn_gen_report

  # observeEvent(input$btnReport, {
  #   # =============================================
  #   # Render the rmd into the directory as well
  #   # =============================================
  #   if (!v$fit_completed) {
  #     showNotification("Must fit model before generating report")
  #   } else {
  #     rmd = "pcurveMix_report.Rmd"
  #     params = list(
  #       descriptor_tbl = v$descriptor_tbl, combined_tbl = v$combined_tbl,
  #       pdf_plot = v$pdf_plot, cdf_plot = v$cdf_plot)
  #
  #     # Path is relative to the Rmd folder
  #     output_directory_name <- "outputs"
  #     dir.create(output_directory_name)
  #     rmd_outfile_name <- paste0(output_directory_name, "/",
  #                                "pcurveMix_report_", time_stamp, ".docx")
  #     rmarkdown::render(rmd,
  #                       output_file = paste0("../", rmd_outfile_name),
  #                       params = params,
  #                       envir = new.env(parent = globalenv()))
  #   } # end of else
  # }) # observeEvent(input$btnReport

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

  # maybe show the custom cutoff
  get_cutoff <- function() {
    # selected_cutoff <- input$cutoff
    # if (selected_cutoff == "p < 0.05") {
    #   cutoff <- 0.05
    # } else if (selected_cutoff == "p < 0.1") {
    #   cutoff <- 0.1
    # } else if (selected_cutoff == "p < custom cutoff") {
    #   cutoff <- as.numeric(input$custom_cutoff)
    #   # NEWJEFF: Check it
    # } else if (selected_cutoff == "p < 1 (unselected)") {
    #   cutoff <- 1
    # } else {
    #   stop("Unrecognized cutoff")
    # }
    cutoff <- as.numeric(input$custom_cutoff)
    return(cutoff)
  } # get_cutoff

  # show_fit_results <- function(res) {
  #   output$parameter_estimates <- renderText({
  #     paste0("Estimates: pi = ",res$pi,", mu = ",res$mu,", sigma = ",res$sigma)
  #   })
  # } # show_fit_resullts
}

# Run the application
shinyApp(ui = ui, server = server)
