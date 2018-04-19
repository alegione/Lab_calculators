

library(shiny)
library(tidyverse)
library(shinyjs)


# jsCode <- 'shinyjs.winprint = function(){
# window.print();
# }'
# 
# ui <- shinyUI(fluidPage(
#   useShinyjs(),
#   extendShinyjs(text = jsCode),
#   actionButton("print", "PRINT")
# ))
# 
# 
# server <- shinyServer(function(input, output) {
# 
#   observeEvent(input$print, {
#     js$winprint()
#   })
# })

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
              tabPanel(title = "dNTP dilutions", icon = NULL,
                       verticalLayout(
                         h3("Concentration of stock dNTPs"),
                         fluidRow(
                           column(width = 3,
                                  numericInput(inputId = "dATP_stock", label = "dATP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "dTTP_stock", label = "dTTP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "dCTP_stock", label = "dCTP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "dGTP_stock", label = "dGTP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           )
                         )
                       ),
                       verticalLayout(
                         h3("Desired concentration of each dNTP in working solution"),
                         fluidRow(
                           column(width = 3,
                                  numericInput(inputId = "dATP_working", label = "dATP (mM)", value = 1.25, min = 0, max = 1000, step = 0.01)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "dTTP_working", label = "dTTP (mM)", value = 1.25, min = 0, max = 1000, step = 0.01)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "dCTP_working", label = "dCTP (mM)", value = 1.25, min = 0, max = 1000, step = 0.01)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "dGTP_working", label = "dGTP (mM)", value = 1.25, min = 0, max = 1000, step = 0.01)
                           )
                         )
                       ),
                       verticalLayout(
                         h3("Desired volume of working solution"),
                         numericInput(inputId = "Working_volume", label = "Working volume (uL)", value = 800, min = 0, max = 10000, step = 1)
                       ),
                       verticalLayout(
                         h3("Therefore:"),
                         column(width = 10,
                                textOutput(outputId = "dATP_volume"),
                                textOutput(outputId = "dTTP_volume"),
                                textOutput(outputId = "dCTP_volume"),
                                textOutput(outputId = "dGTP_volume"),
                                textOutput(outputId = "water_volume")
                         )
                       )

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
  # SERVER code for dNTP tab  
  vol_ATP <- reactive({
    input$Working_volume * input$dATP_working / input$dATP_stock
  })
  vol_TTP <- reactive({
    input$Working_volume * input$dTTP_working / input$dTTP_stock
  })
  vol_CTP <- reactive({
    input$Working_volume * input$dCTP_working / input$dCTP_stock
  })
  vol_GTP <- reactive({
    input$Working_volume * input$dGTP_working / input$dGTP_stock
  })
  
  output$dATP_volume <- renderText({
    paste("Add", vol_ATP(), "ul of dATP stock")
  })
  output$dTTP_volume <- renderText({
    paste("Add", vol_TTP(), "ul of dTTP stock")
  })
  output$dCTP_volume <- renderText({
    paste("Add", vol_CTP(), "ul of dCTP stock")
  })
  output$dGTP_volume <- renderText({
    paste("Add", vol_GTP(), "ul of dGTP stock")
  })
  output$water_volume <- renderText({
    vol_water <- input$Working_volume - vol_ATP() - vol_TTP() - vol_CTP() - vol_GTP()
    paste("Add", vol_water, "ul molecular biology grade water")
  })
   
  
}
# Run the application 
shinyApp(ui = ui, server = server)
