# Shiny formatting

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(width = 6,
           numericInput(inputId = "num1",
                        label = "First Number",
                        value = 10)
    ),
    column(width = 6,
           numericInput(inputId = "num2",
                        label = "Second Number",
                        value = 20)
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)


library(shiny)
library(bslib)

ui <- fluidPage(
  fluidRow(
    layout_columns(
      col_widths = c(-1, 5, 6), # Explicitly gives each element half the row width
      numericInput("num1", "First Number", value = 10),
      numericInput("num2", "Second Number", value = 20)
    )
  )
)
server <- function(input, output, session) {}
shinyApp(ui, server)

######################################

library(shiny)
library(bslib)

ui <- fluidPage(
  # Your existing fluidRow setup
  fluidRow(
    h3("My Section Title", class = "p-3"),

    # layout_columns lives directly inside your row
    layout_columns(
      # -3 creates the indent, remaining 9 columns are split evenly (3, 3, 3)
      col_widths = breakpoints(md = c(1, 3, 4, 4)),
      span(),
      selectInput("ctrl1", "Choose Option", choices = c("A", "B")),
      numericInput("ctrl2", "Enter Value", value = 100),
      actionButton("submit", "Submit", class = "btn-primary mt-4")
    )
  )
)

server <- function(input, output, session) {}
shinyApp(ui, server)
