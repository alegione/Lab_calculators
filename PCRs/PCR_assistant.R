

library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "PCR assistant", windowTitle = "PCR assistant"),
  
  mainPanel(width = 20,
            tabsetPanel(
              tabPanel(title = "PCR Protocol set up", icon = NULL
                       
              ),
              tabPanel(title = "Primer dilutions", icon = NULL
                       
              ),
              tabPanel(title = "dNTP dilutions", icon = NULL
                       
              ),
              tabPanel(title = "Documentation", icon = NULL,
                p(h4("PCR Assistant")),
                HTML("This app includes an array of quick calculators for basic PCR work
                  <br>The formulas for each tool are included in their respective tabs
                  <br>
                  <br>Please report errors or request changes:
                  <br>Github: <a href = https://github.com/Alegione>Github.com/Alegione</a>
                  <br>Twitter: <a href = https://Twitter.com/Alegione>Twitter.com/Alegione</a>"
                )   
              )
            )
  )
  
)

server <- function(input, output) {
  # SERVER code for Protocol tab  

   
  
}
# Run the application 
shinyApp(ui = ui, server = server)
