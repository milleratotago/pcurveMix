

if (!require(shiny)) install.packages('shiny')
if (!require(shinyjs)) install.packages('shinyjs')  # show & hide, for example.
if (!require(shinyFeedback)) install.packages('shinyFeedback')  # showNotification

# Helper function for ui
inline_numericInput=function(ni){
  tags$div( class="form-inline",ni)
}

button_width <- "250px"


#=========================================================================
# ui
#=========================================================================
ui <- tagList(

  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),

  fluidPage(

    tags$head(tags$style(
      "#side_panel{padding-left:10px;}
      .form-group {margin-bottom: 5px;}
      .form-inline .form-control {width: 100%}
      .shiny-notification
      {
      color:#ffffff;
      background-color:#112446;
      font-size: 120%;
      width: 25%;
      position: fixed;
      top: 90%;
      left: 2%;
      }
      hr{border-top: 10px solid #050505;}"
    )),

    tags$style(HTML("#shiny_examples_path { font-style: italic; }")),

    #   # Next puts text input box next to label
    #   tags$style(HTML("
    #   .shiny-input-container {
    #     display: flex;
    #     flex-direction: row;
    #     align-items: center;
    #   }
    #   .shiny-input-container label {
    #     margin-right: 10px;
    #   }
    # ")),

    # CSS to make the label span wide and input box narrow
    tags$style(HTML("
      .custom-text-input { width: 100%; }
      .custom-text-input label { width: 100%; font-weight: bold; }
      .custom-text-input input { width: 25%; }
    ")),

    fluidRow(
      column(12, titlePanel("Fit Ulrich & Miller (2026) p-curve mixture model"))
    ),
    sidebarLayout(

      # Setup panel:
      sidebarPanel(width = 4, id = "pcm_sidebar",
                   h1("Setup for fitting"),
                   h4(),
                   checkboxInput("use_demo", "Use demo file of p values", FALSE),
                   conditionalPanel(
                     condition = "input.use_demo == false",
                     fileInput("p_file", "Upload CSV file with column of p value", accept = ".csv")
                   ),
                   # div(style="height: 70px;",fileInput("p_file",
                   #                                     label = "Choose a CSV file with a column of p values:" ,
                   #                                     multiple=FALSE,
                   #                                     accept = c(".csv"),
                   #                                     placeholder = "")),
                   # h5("Paste the following into the browse box to use an example CSV file:"),
                   # fluidRow(
                   #   column(12, textOutput("shiny_examples_path"))
                   # ),
                   h4(),
                   fluidRow(
                     column(12, radioButtons("tails",
                                             "File has 1- or 2-tailed p's?",
                                             choices = c("1-tailed", "2-tailed"),
                                             selected = "2-tailed"),
                     )
                   ),
                   div(class = "custom-text-input",
                       fluidRow(
                         column(12, textInput("custom_cutoff","Upper p cutoff for inclusion in file ('alpha'):", value = "1")
                         ),
                       ),
                       fluidRow(
                         column(12, textInput("alpha_sig","Alpha level to use for power computations ('alpha_sig'):", value = "0.05")
                         ),
                       ),
                       fluidRow(
                         column(12, textInput("n_boot_samples","N samples for parametric bootstrap analysis (recommended minimum 2000 for real analyses):", value = "100")
                         ),
                       ),
                   ),
                   hr(),
                   fluidRow(
                     column(12, actionButton("btnFit","Fit the model"))
                   ),
                   h4(),
                   fluidRow(
                     column(12, downloadButton("btnReport","Download results"))
                   )
      ), # end sidebar panel

      # Results panel:
      mainPanel(width = 8,
                # h2("Model fit:"),
                fluidRow(
                  column(12, h1(textOutput("model_fit_title")))
                ),
                fluidRow(
                  column(12, tableOutput("descriptor_tbl"))
                ),
                # fluidRow(
                #   column(12, tableOutput("bootstrap_convergence_tbl"))
                # ),
                # h2("Parameter estimates:"),
                fluidRow(
                  column(12, h3(textOutput("parameter_estimates_title")))
                ),
                fluidRow(
                  column(12, tableOutput("estimates_tbl"))
                ),
                div(
                  fluidRow(
                    column(12, h4(textOutput("bootstrap_title")))
                  ),
                  fluidRow(
                    column(12,
                           h5(
                             div(
                               textOutput("n_boot_samples"),
                               style =  "margin-top: -6px; margin-bottom: -20px; padding: 0;"
                             )
                           )
                    )
                  ),
                  fluidRow(
                    column(12, h5(textOutput("boot_pct_converged")))
                  ),
                  fluidRow(
                    column(12, tableOutput("bootstrap_tbl"))
                  )
                  , style = "margin-left: 35px;"),
                fluidRow(
                  column(12, h3(textOutput("predicted_pdfs_title")))
                ),
                fluidRow(
                  column(12, plotOutput("pdf_plot"))
                ),
                # h2("Observed/predicted CDFs:"),
                fluidRow(
                  column(12, h3(textOutput("predicted_cdfs_title")))
                ),
                fluidRow(
                  column(12, plotOutput("cdf_plot"))
                )
      ) # end mainPanel
    ) # end sidebarlayout

  ) # end fluidPage

) # taglist

