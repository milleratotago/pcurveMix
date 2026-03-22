
# Helper function for ui
inline_numericInput=function(ni){
  tags$div( class="form-inline",ni)
}

button_width <- "250px"

tags$head(tags$style(
  "#side_panel{padding-left:10px;}
  .form-group {margin-bottom: 5px;}
  .form-inline .form-control {width: 100%}
  .shiny-notification
  {
    color:#ffffff;
    background-color:#112446;
    font-size: 150%;
    width: 200%;
    position: fixed;
    top: 90%;
    left: 50%;
  }
  hr{border-top: 2px solid #aaaaaa;}"
))


#=========================================================================
# ui
#=========================================================================
ui <- tagList(

  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),

  fluidPage(
    fluidRow(
      column(12, titlePanel("Fit Ulrich & Miller (2026) p-curve mixture model"))
    ),
    sidebarLayout(
      sidebarPanel(width = 4, id = "pcm_sidebar",
                   fluidRow(
                     column(12, fileInput("p_file",
                                          label = "Choose a CSV file with a column of p values:" ,
                                          multiple=FALSE,
                                          accept = c("csv"),
                                          placeholder = ""))
                   ),
                   fluidRow(
                     column(12, radioButtons("tails",
                                             "File has 1- or 2-tailed p's?",
                                             choices = c("1-tailed", "2-tailed"),
                                             selected = "2-tailed"),
                     )
                   ),
                   fluidRow(
                     column(12, textInput("custom_cutoff","Upper p cutoff:", value = "0.1")
                     ),
                   ),
                   fluidRow(
                     column(12, textInput("n_boot_samples","N bootstrap samples:", value = "200")
                     ),
                   ),
                   hr(),
                   fluidRow(
                     column(12, actionButton("btnFit","Fit model"))
                   ),
                   h4(),
                   fluidRow(
                     column(12, downloadButton("btnReport","Save results"))
                   )
      ), # end sidebar panel
      mainPanel(width = 8,
                h2("Model fit:"),
                fluidRow(
                  column(12, tableOutput("descriptor_tbl"))
                ),
                h2("Parameter estimates:"),
                fluidRow(
                  column(12, tableOutput("estimates_tbl"))
                ),
                h2("Observed/predicted PDFs:"),
                fluidRow(
                  column(12, plotOutput("pdf_plot"))
                ),
                h2("Observed/predicted CDFs:"),
                fluidRow(
                  column(12, plotOutput("cdf_plot"))
                )
      ) # end mainPanel
    ) # end sidebarlayout

  ) # end fluidPage

) # taglist

