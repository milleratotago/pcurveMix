
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Load the script that defines user-interface screen.
source("ui.R")

# Load the script that defines the computations & output.
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
