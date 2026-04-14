

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
      width: 32%;
      position: fixed;
      top: 80%;
      left: 1%;
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
      .custom-text-input input { width: 45%; }
    ")),

    tags$style(HTML("
                    #start_mu { width: 60px; }
                    #start_sigma { width: 60px; }
                    #start_pi { width: 60px; }
                    ")),

    fluidPage(title = "pcurveMix"),

    fluidRow(
      column(12,
             titlePanel(div("Fit Ulrich & Miller (2026) p-Curve Mixture Model", style = "text-align: center;")
             )
      )
    ),
    sidebarLayout(

      # Setup panel:
      sidebarPanel(width = 4, id = "pcm_sidebar",
                   h1("Setup for Fitting"),
                   h4(),
                   checkboxInput("use_demo", label = strong("Use demo file of p values"), FALSE),
                   conditionalPanel(
                     condition = "input.use_demo == false",
                     fileInput("p_file", "Upload CSV file with column of p value", accept = ".csv")
                   ),
                   h4(),
                   fluidRow(
                     column(12, radioButtons("tails",
                                             "File has 1- or 2-tailed p's?",
                                             choices = c("1-tailed", "2-tailed"),
                                             selected = "2-tailed"),
                     )
                   ),
                   h4(),
                   div(class = "custom-text-input",
                       fluidRow(
                         column(12, numericInput("custom_cutoff","Upper p cutoff for inclusion in file ('alpha'):", value = "1",
                                                 min = 0, max = 1, step = 0.05)
                         ),
                       ),
                       h4(),
                       fluidRow(
                         column(12, numericInput("alpha_sig","Alpha level to use for power computations ('alpha_sig'):", value = "0.05",
                                                 min = 0, max = 1, step = 0.01)
                         ),
                       ),
                       h4(),
                       fluidRow(
                         column(12, numericInput("n_boot_samples","N parametric bootstrap samples (recommended min 2000 for real analyses):",
                                                 value = "100", min = 0, step = 100)
                         ),
                       ),
                   ),
                   h4(),
                   checkboxInput("adjust_starting_values", label = strong("Change default starting parameter values for optim() search:"), FALSE),
                   conditionalPanel(
                     condition = "input.adjust_starting_values == true",
                     fluidRow(
                       column(4, numericInput("start_mu","mu",2, min = 0, max = 20, step = 0.1)),
                       column(4, numericInput("start_sigma","sigma",2, min = 1e-6, max = 20, step = 0.1)),
                       column(4, numericInput(inputId = "start_pi", label = "pi", value = 0.5, min = 0, max = 20, step = 0.1))
                     ),
                   ),
                   # h5("Starting parameter values for optim() search:", style = "font-weight: bold;"),
                   # fluidRow(
                    # column(4, numericInput("start_mu","mu",2, min = 0, max = 20, step = 0.1)),
                    # column(4, numericInput("start_sigma","sigma",2, min = 1e-6, max = 20, step = 0.1)),
                    # column(4, numericInput(inputId = "start_pi", label = "pi", value = 0.5, min = 0, max = 20, step = 0.1))
                   # ),
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

