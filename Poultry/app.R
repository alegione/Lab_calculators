#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Virus titration calculators"),

   mainPanel(width = 20,
    tabsetPanel(
      tabPanel("PFU",
               verticalLayout(
                 h1('Plaque forming units'),
                 h2('Input data'),
                 fluidRow(
                   column(3,
                          numericInput(inputId = "Plate1", label = "Plate 1", value = 0, min = 0, max = 1000, step = 1)
                   ),
                   column(3, 
                          numericInput(inputId = "Plate2", label = "Plate 2", value = 0, min = 0, max = 1000, step = 1)
                   ),
                   column(3,
                          numericInput(inputId = "Plate3", label = "Plate 3 *", value = -1, min = -1, max = 1000, step = 1)
                   )
                 ),
                 fluidRow(
                   column(3,
                          numericInput(inputId = "Volume", label = "Inoculum (ul)", value = 0.400, min = 0, max = 2, step = 0.001)
                   ),
                   column(4,
                          numericInput(inputId = "DilutionFactor", label = "Dilution factor (10^x)", value = -3, min = -10, max = 0, step = 1)
                   )
                 )
               ),
               verticalLayout(
                 hr(),
                 paste("*To only average the values of two plates, set 'Plate 3' value to -1 (default)")
               ),
               verticalLayout(
                 h2('Results'),
                 verbatimTextOutput(outputId = "Average_PFU"),
                 verbatimTextOutput(outputId = "PFU_per_mL")
               ),
               verticalLayout(
                  HTML("<br><u><b>Equation for calculation: </b></u>
                  <br> <br>
                  <b> PFU/mL = mean PFU / ( dilution x inoculum volume )</b>
                  <br>
                  where: <br>
                  mean PFU = the average plaque forming units across all plates counted <br>
                  dilution = the dilution factor of the well being used for mean PFU (eg. 10^-3) <br>
                  inoculum volume = the original volume of inoculum added to the well in millilitres (eg 0.400 mL) <br>")
               )
      ),
     tabPanel("TCID50",
              verticalLayout(
                h1('Median Tissue Culture Infectious Dose'),
                paste("Code still to come")
                )
            ),
     tabPanel("EID50",
              verticalLayout(
                h1('Median Egg Infectious Dose'),
                paste("Code still to come")
              )
            ),
     tabPanel("LD50",
              verticalLayout(
                h1('Median Lethal Dose'),
                paste("Code still to come")
              )
            ),
     tabPanel(title = "Documentation",
              p(h4("Virus titration calculators")),
              HTML("This app includes an array of quick calculators for virology work
                    <br>The formulas for each tool are included in their respective tabs,
                    <br>
                    <br>Please report errors or request changes:
                    <br>Github: <a href = https://github.com/Alegione>Github.com/Alegione</a>
                    <br>Twitter: <a href = https://Twitter.com/Alegione>Twitter.com/Alegione</a>")
            )
      )
   )
)

server <- function(input, output) {
  output$PFU_per_mL <- renderText({
    if (input$Plate1 == 0 || input$Plate2 == 0){
      paste0("Values of at least two counts (eg Plate 1 and Plate 2) must be greater than zero")
    } else if (input$Plate3 == -1) {
      paste0("Titre = ", ((input$Plate1 + input$Plate2) / 2) / (10^input$DilutionFactor * input$Volume),
            " PFU/mL\n\t10^",  round(x = log10(((input$Plate1 + input$Plate2) / 2) / (10^input$DilutionFactor * input$Volume)), 2), " PFU/mL")
    } else {
      paste0("Titre = ", ((input$Plate1 + input$Plate2 + input$Plate3) / 3) / (10^input$DilutionFactor * input$Volume),
            " PFU/mL\n\t10^", round(x = log10(((input$Plate1 + input$Plate2 + input$Plate3) / 3) / (10^input$DilutionFactor * input$Volume)), 2), " PFU/mL")
    }
  })
  output$Average_PFU <- renderText({
    if (input$Plate1 == 0 || input$Plate2 == 0){
      paste("")
    } else if (input$Plate3 == -1) {
      paste0("Average PFU = ", round(x = ((input$Plate1 + input$Plate2) / 2), 2), " PFU")
    } else {
      paste0("Average PFU = ", round(x = ((input$Plate1 + input$Plate2 + input$Plate3) / 3), 2), " PFU")
    }
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
