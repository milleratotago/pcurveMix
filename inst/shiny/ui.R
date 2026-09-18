
# Packages required for shiny are all listed under "suggests"
# in the DESCRIPTION file. When the shiny app starts, it checks
# to make sure these are all present and tells user to install
# them if they are not.

# Header ====

# Helper function for ui
inline_numericInput=function(ni){
  tags$div( class="form-inline",ni)
}

# Reusable helper function for accepting horizontal numeric inputs
inlineNumericInput <- function(inputId, label, value, min = NA, max = NA,
                               step = NA, width = "70px", label_width = "120px") {
  div(
    style = "display: flex; align-items: center; margin-bottom: 10px;",
    tags$label(
      label,
      `for` = inputId,
      style = paste0("width: ", label_width, "; margin-bottom: 0; margin-right: 10px; font-weight: bold;")
    ),
    numericInput(
      inputId = inputId,
      label = NULL,
      value = value,
      min = min,
      max = max,
      step = step,
      width = width
    )
  )
}

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
      hr{border-top: 10px solid #050505;}
      input[type='number'] { text-align: right; }"  # align numbers in numeric input boxes
    )),

    tags$style(HTML("#shiny_examples_path { font-style: italic; }")),

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
    ), # fluidRow


    # wellPanel: Specify p values ====
    wellPanel(
      h2("p values to be fit:"),
      ## Input file ====
      checkboxInput("use_demo", label = strong("Use demo file of p values"), FALSE),
      conditionalPanel(
        condition = "input.use_demo == false",
        fileInput("p_file", "Upload CSV file with p value column labelled 'p'", accept = ".csv")
      ),

      radioButtons("tails", "File has 1- or 2-tailed p's?",
                   choices = c("1-tailed", "2-tailed"), selected = "2-tailed"),
      inlineNumericInput("custom_cutoff", "Upper p cutoff for inclusion in file ('alpha'):",
                         value = "1", min = 0, max = 1, step = 0.05, label_width = "400px")
    ), # wellPanel

    wellPanel(
      h2("Analysis options:"),
      inlineNumericInput("confidence_level", "Confidence level for confidence intervals:",
                         value = "95", min = 0, max = 100, step = 6,
                         label_width = "400px"),

      checkboxInput("jackknifing", label = strong("Compute jackknifing for bias correction & confidence interval"), TRUE),

      checkboxInput("parametric_bootstrapping", label = strong("Compute parametric bootstrap confidence intervals"), TRUE),
      conditionalPanel(
        condition = "input.parametric_bootstrapping == true",
        inlineNumericInput("n_boot_samples", "...N bootstrap samples (recommended min 2000 for real analyses):",
                           value = "100", min = 0, step = 100,
                           label_width = "400px")
      ),

      checkboxInput("nonparametric_bootstrapping", label = strong("Compute nonparametric bootstrap confidence intervals"), TRUE),
      conditionalPanel(
        condition = "input.nonparametric_bootstrapping == true",
        inlineNumericInput("np_n_boot_samples", "...N bootstrap samples (recommended min 2000 for real analyses):",
                           value = "100", min = 0, step = 100,
                           label_width = "400px")
      ), # conditionalPanel

      checkboxInput("profile_ci", label = strong("Compute profile confidence intervals"), TRUE),

      inlineNumericInput("alpha_sig", "Alpha level to use for power computations ('alpha_sig'):",
                         value = "0.05", min = 0, max = 1, step = 0.01, label_width = "400px"),

      ## Adjust starting values ====
      h4(),
      checkboxInput("specify_starting_values", label = strong("Use a specific set of starting parameter values for optim() search:"), FALSE),
      conditionalPanel(
        condition = "input.specify_starting_values == true",
        div(
          style = "display: flex; align-items: center; flex-wrap: wrap; gap: 10px;",
          inlineNumericInput("start_mu", "mu =", value = START_MU_DEFAULT,
                             min = 0, max = 20, step = 0.1, label_width = "40px"),
          inlineNumericInput("start_sigma", "sigma =", value = START_SIGMA_DEFAULT,
                             min = 1e-6, max = 20, step = 0.1, label_width = "60px"),
          inlineNumericInput("start_pi", "pi =", value = START_PI_DEFAULT,
                             min = 0, max = 1, step = 0.1, label_width = "40px")
        ), # div
      ), # conditionalPanel
      ## end Adjust starting values ====

    # end Specify p values panel
      actionButton("btnFit","Fit model & compute requested CIs")
    ), # wellPanel Analysis options
    #                                                                ====
    # wellPanel: Results ====
    wellPanel(
      #                                                              ====

      # ML estimates table & predicted/observed pdfs/cdfs ====
      h1(textOutput("model_fit_title")),
      tableOutput("descriptor_tbl"),
      h3(textOutput("parameter_estimates_title")),
      tableOutput("estimates_tbl"),
      uiOutput("estimates_notes"),

      h3(textOutput("predicted_pdfs_title")),
      plotOutput("pdf_plot"),
      h3(textOutput("predicted_cdfs_title")),
      plotOutput("cdf_plot"),
      # end ML estimates table & predicted/observed pdfs/cdfs ====


      #                                                              ====
      # Jackknife results ====
      conditionalPanel(
        hr(),
        condition = "input.jackknifing == true",
        h3(textOutput("jackknife_title")),
        tableOutput("jackknife_tbl"),
        uiOutput("jackknife_notes")
      ),  # end of conditionalPanel
      # end Jackknife results ====

      #                                                              ====
      # Bootstrap results (parametric) results ====
      conditionalPanel(
        condition = "input.parametric_bootstrapping == true",
        h3(textOutput("boot_title")),
        # h5(textOutput("n_boot_samples")),
        # h5(textOutput("boot_pct_converged")),
        tableOutput("boot_tbl"),
        uiOutput("boot_notes")
      ),  # end of conditionalPanel
      # end Bootstrap results (parametric) ====

      #                                                              ====
      # Bootstrap results (nonparametric) results ====
      conditionalPanel(
        condition = "input.nonparametric_bootstrapping == true",
        h3(textOutput("np_boot_title")),
        # h5(textOutput("np_n_boot_samples")),
        # h5(textOutput("np_boot_pct_converged")),
        tableOutput("np_boot_tbl"),
        uiOutput("np_boot_notes")
      ),  # end of conditionalPanel
      # end Bootstrap results (nonparametric) ====

      #                                                              ====
      # Profile confidence interval results ====
      conditionalPanel(
        condition = "input.profile_ci",
        h3(textOutput("profileCI_title")),
        tableOutput("profileCI_tbl"),
        uiOutput("profile_notes"),
        plotOutput("profile_mu_plot"),
        plotOutput("profile_sigma_plot"),
        plotOutput("profile_pi_plot"),
        plotOutput("profile_power_plot"),
        plotOutput("profile_folded_normal_mu_plot"),
        plotOutput("profile_folded_normal_sigma_plot")
      ),
      # end Profile confidence interval results ====

      h3(verbatimTextOutput("optim_failed_output")), # NEWJEFF: Replace with showmessage

      #                                                              ====
      # Download & Quit buttons ====
      hr(),
      h4(),
      column(6, # User selection for the format
             radioButtons("rmd_format", "Select document download format:",
                          choices = c("HTML" = "html",
                                      "PDF" = "pdf",
                                      "Word (DOCX)" = "docx")),
      ),

      # This was previously a fluidRow with 2 4-column buttons
      # but they appeared above/below rather than left/right;
      # I don't know why.
      div(
        style = "display: flex; gap: 20px;",
        downloadButton("btnReport","Download results"),
        actionButton("btnquit","Quit")
      ) # div
      # end Download & Quit buttons ====

    ) # wellPanel
    # end Results panel ====

  ) # end fluidPage

) # taglist

# ... ====
