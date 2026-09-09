
# Packages required for shiny are all listed under "suggests"
# in the DESCRIPTION file

# if (!require(shiny)) install.packages('shiny')
# if (!require(shinyjs)) install.packages('shinyjs')  # show & hide, for example.
# if (!require(shinyFeedback)) install.packages('shinyFeedback')  # showNotification
# if (!require(bslib)) install.packages('bslib')  # layout_columns

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
      width: 65%;
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
                   h2("Setup for Fitting"),
                   h4(),
                   checkboxInput("use_demo", label = strong("Use demo file of p values"), FALSE),
                   conditionalPanel(
                     condition = "input.use_demo == false",
                     fileInput("p_file", "Upload CSV file with column of p value", accept = ".csv")
                   ),
                   # h4(),
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
                       hr(style = "border-top: 2px solid #808080;"),
                       h4(),
                       fluidRow(
                         column(12, numericInput("alpha_sig","Alpha level to use for power computations ('alpha_sig'):", value = "0.05",
                                                 min = 0, max = 1, step = 0.01)
                         ),
                       ),
                       hr(style = "border-top: 2px solid #808080;"),
                   ), # div(class = "custom-text-input",

                   h4(), # I tried very (!!!) hard to indent the numericInput but never succeeded.
                   # Gemini suggested using bslib & layout_columns but these did not work
                   checkboxInput("parametric_bootstrapping", label = strong("Compute parametric bootstrap confidence intervals"), FALSE),
                   conditionalPanel(
                     condition = "input.parametric_bootstrapping == true",
                     fluidRow(
                       column(6, numericInput("boot_confidence_level",
                                              "% confidence (1-100)",
                                              value = "95", min = 10, max = 100, step = 1)
                       ),
                       column(6, numericInput("n_boot_samples",
                                              "N bootstrap samples (recommended min 2000 for real analyses):",
                                              value = "100", min = 0, step = 100)
                       )
                     )
                   ),
                   hr(style = "border-top: 2px solid #808080;"),

                   h4(),
                   checkboxInput("profile_ci", label = strong("Compute profile confidence intervals"), FALSE),
                   conditionalPanel(
                     condition = "input.profile_ci == true",
                     fluidRow(
                       column(6, numericInput("profile_confidence_level",
                                              "% confidence (1-100)",
                                              value = "95", min = 10, max = 100, step = 1)
                       )
                     ) # fluidrow
                   ), # profiles checked
                   hr(style = "border-top: 2px solid #808080;"),

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
                     column(12, actionButton("btnFit","Fit model & compute requested CIs"))
                   ),
                   hr(),
                   h4(),
                   fluidRow(
                     column(6, # User selection for the format
                            radioButtons("rmd_format", "Select document download format:",
                                         choices = c("HTML" = "html",
                                                     "PDF" = "pdf",
                                                     "Word (DOCX)" = "docx")),
                     ),
                     column(4, downloadButton("btnReport","Download results"))
                   ),
                   hr(),
                   h4(),
                   fluidRow(
                     column(12, actionButton("btnquit","Quit"))
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

                ##### Obs/pred PDF/CDF plots
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
                ),

                #### Bootstrap results
                div(
                  fluidRow(
                    column(12, h3(textOutput("bootstrap_title")))
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
                  , style = "margin-left: 0px;"
                ),  # end of div

                # Profile confidence interval results
                div(
                  fluidRow(
                    column(12, h3(textOutput("profileCI_title")))
                  ),
                  fluidRow(
                    column(12, tableOutput("profileCI_tbl"))
                  ),
                  # fluidRow(
                  #   column(12, h3(textOutput("profile_mu_title")))  # NEWJEFF: Unused
                  # ),
                  fluidRow(
                    column(12, plotOutput("profile_mu_plot"))
                  ),
                  fluidRow(
                    column(12, plotOutput("profile_sigma_plot"))
                  ),
                  fluidRow(
                    column(12, plotOutput("profile_pi_plot"))
                  ),
                  fluidRow(
                    column(12, plotOutput("profile_power_plot"))
                  ),
                  fluidRow(
                    column(12, plotOutput("profile_folded_normal_mu_plot"))
                  ),
                  fluidRow(
                    column(12, plotOutput("profile_folded_normal_sigma_plot"))
                  ),
                  style = "margin-left: 0px;"
                ),  # end of div

                fluidRow(
                  column(12, h3(verbatimTextOutput("optim_failed_output")))
                )
      ) # end mainPanel
    ) # end sidebarlayout

  ) # end fluidPage

) # taglist

