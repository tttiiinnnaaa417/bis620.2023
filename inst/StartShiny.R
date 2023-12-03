
## A function to start the Shiny App

launchShinyApp <- function() {
  # Load the shiny library
  library(shiny)
  
  # Define the path to the Shiny app within the package
  appDir <- system.file("shinyApp", package = "bis620.2023")
  
  # Check if the app directory exists
  if (!file.exists(appDir)) {
    stop("Shiny app directory not found in the package.")
  }
  
  # Run the Shiny app
  runApp(appDir)
}