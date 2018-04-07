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
                          h1('CODE NOT TESTED'),
                          h2('Input data'),
                          fluidRow(
                            column(3,
                                   numericInput(inputId = "VolumeTCID50", label = "Inoculum (ul)", value = 0.400, min = 0, max = 2, step = 0.001)
                            ),

                            column(3,
                                   numericInput(inputId = "WellsTCID50", label = "Wells inoculated per dilution", value = 4, min = 2, max = 12, step = 1)
                            )
                          ),
                          fluidRow(
                            column(3,
                                   numericInput(inputId = "DilutionLowTCID50", label = "Starting dilution (10^x)", value = -1, min = -20, max = 0, step = 1)
                            ),
                            column(3,
                                   numericInput(inputId = "DilutionHighTCID50", label = "Final dilution (10^x)", value = -4, min = -20, max = 0, step = 1)
                            ),
                            column(4,
                                   numericInput(inputId = "DilutionFactorTCID50", label = "Dilution factor", value = 10, min = 0, max = 1000, step = 1)
                            )
                          )
                        ),
                        uiOutput("DilutionSeries"),
                        verticalLayout(
                          h2('Results'),
                          verbatimTextOutput(outputId = "TCID50_text"),
                          verbatimTextOutput(outputId = "TCID50_per_mL_text")
                        ),
                        verticalLayout(
                          HTML("<br><u><b>Equation for calculation (Spearman-Karber formula): </b></u>
                               <br> <br>
                               <b> TCID50 = 10 ^ (highest dilution + dilution step * ( 0.5 - ((1 / wells per dilution) * sum of the negative wells)))</b></p>
                               TCID50/mL = TCID50 / inoculum volume (mL)
                               where: <br>
                               highest dilution = The value of the highest exponent as an absolute integer <br>
                                <p style='margin-left: 110px'>e.g. in a serial ten fold dilution from 10^-1 to 10^-6, the value would be 6</p>
                               wells per dilution = The total number of wells inoculated at each dilution</p>
                               sum of the negative wells = The total number of wells that are free of cytopathic effect (i.e. 'negative' wells)</p>")
                        #https://www.google.com.au/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=0ahUKEwiF8N22-KbaAhXGVrwKHdB6CvgQFgg0MAE&url=https%3A%2F%2Fwww.klinikum.uni-heidelberg.de%2Ffileadmin%2Finst_hygiene%2Fmolekulare_virologie%2FDownloads%2FTCID50_calculator_v2_17-01-20_MB.xlsx&usg=AOvVaw2FecwX0Pz6446j5PoL1S29
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
# SERVER code for PFU tab  
  # Average the PFU from either two or three plates, depedending on the value of the plate 3 variable
  AveragePFUreactive <- reactive({
    if (input$Plate1 != 0 && input$Plate2 != 0){
      if (input$Plate3 == -1) {
        ((input$Plate1 + input$Plate2) / 2)
      } else {
        ((input$Plate1 + input$Plate2 + input$Plate3) / 3)
      }
    } else {
      0
    }
  })
  PFU_per_ml_reactive <- reactive({
    if (AveragePFUreactive() != 0){
      AveragePFUreactive() / (10^input$DilutionFactor * input$Volume)
    } else {
      0
    }
  })
  output$PFU_per_mL <- renderText({
    if (input$Plate1 == 0 || input$Plate2 == 0){
      paste0("Values of at least two counts (eg Plate 1 and Plate 2) must be greater than zero")
    } else { 
      paste0("Titre = ", round(x = PFU_per_ml_reactive(), 2), " PFU/mL\n\t10^",  round(x = log10(PFU_per_ml_reactive()), 2), " PFU/mL")
    }
  })
  output$Average_PFU <- renderText({
    if (input$Plate1 == 0 || input$Plate2 == 0){
      paste("")
    } else if (input$Plate3 == -1) {
      paste0("Average PFU = ", round(x = AveragePFUreactive(), 2), " PFU per well")
    } else {
      paste0("Average PFU = ", round(x = AveragePFUreactive(), 2), " PFU per well")
    }
  })
#SERVER code for TCID50
  output$DilutionSeries <- renderUI({
    tags <- tagList()
    for (i in seq(from = abs(input$DilutionLowTCID50), to = abs(input$DilutionHighTCID50), by = log10(input$DilutionFactorTCID50))) {
      tags[[i]] <- numericInput(inputId = paste0('n', i), label = paste0('10^-', i, " (n / ", input$WellsTCID50, ")"), value = input$WellsTCID50, min = 0, max = input$WellsTCID50, step = 1)
    }
    flowLayout(tags, cellArgs = list())
  })
  TCID50_negatives <- reactive({
    negative <- 0
    for (i in seq(from = abs(input$DilutionLowTCID50), to = abs(input$DilutionHighTCID50), by = log10(input$DilutionFactorTCID50))) {
      val <- paste0("n",i)
      negative <- negative + (4 - (input[[val]]))
    }
    negative
  })
  TCID50_reactive <- reactive({
    abs(input$DilutionHighTCID50) + log10(input$DilutionFactorTCID50) * ( 0.5 - (1 / input$WellsTCID50) * TCID50_negatives())
  })
  output$TCID50_text <- renderText({
    paste0("TCID50 = 10^", TCID50_reactive())
  })
  output$TCID50_per_mL_text <- renderText({
    paste0("Titre = 10^", round(log10((10^TCID50_reactive()) / input$VolumeTCID50), 2), " TCID50/mL")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
