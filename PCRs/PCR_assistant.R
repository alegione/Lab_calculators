

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
                         ),
                         numericInput(inputId = "dNTP_equimolar_stock", label = "Adjust for equimolar dNTPs (mM)", min = 0, max = 1000, value = 100, step = 0.1)
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
                         ),
                         numericInput(inputId = "dNTP_equimolar_working", label = "Adjust for equimolar dNTPs (mM)", min = 0, max = 1000, value = 1.25, step = 0.05)
                       ),
                       verticalLayout(
                         h3("Desired volume of working solution"),
                         numericInput(inputId = "dNTP_Working_volume", label = "Working volume (uL)", value = 800, min = 0, max = 10000, step = 1)
                       ),
                       verticalLayout(
                         h3("Therefore:"),
                         column(width = 10,
                                textOutput(outputId = "dATP_volume"),
                                textOutput(outputId = "dTTP_volume"),
                                textOutput(outputId = "dCTP_volume"),
                                textOutput(outputId = "dGTP_volume"),
                                textOutput(outputId = "dNTP_water_volume")
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

server <- function(input, output, session) {
  # SERVER code for dNTP tab
  observeEvent(input$dNTP_equimolar_stock, {
    updateNumericInput(session, "dATP_stock", value = input$dNTP_equimolar_stock)
    updateNumericInput(session, "dTTP_stock", value = input$dNTP_equimolar_stock)
    updateNumericInput(session, "dCTP_stock", value = input$dNTP_equimolar_stock)
    updateNumericInput(session, "dGTP_stock", value = input$dNTP_equimolar_stock)
  })
  observeEvent(input$dNTP_equimolar_working, {
    updateNumericInput(session, "dATP_working", value = input$dNTP_equimolar_working)
    updateNumericInput(session, "dTTP_working", value = input$dNTP_equimolar_working)
    updateNumericInput(session, "dCTP_working", value = input$dNTP_equimolar_working)
    updateNumericInput(session, "dGTP_working", value = input$dNTP_equimolar_working)
  })
  
  vol_ATP <- reactive({
    input$dNTP_Working_volume * input$dATP_working / input$dATP_stock
  })
  vol_TTP <- reactive({
    input$dNTP_Working_volume * input$dTTP_working / input$dTTP_stock
  })
  vol_CTP <- reactive({
    input$dNTP_Working_volume * input$dCTP_working / input$dCTP_stock
  })
  vol_GTP <- reactive({
    input$dNTP_Working_volume * input$dGTP_working / input$dGTP_stock
  })
  
  output$dATP_volume <- renderText({
    paste("Add", round(x = vol_ATP(), 2), "ul of", input$dATP_stock, "mM dATP stock")
  })
  output$dTTP_volume <- renderText({
    paste("Add", round(x = vol_TTP(), 2), "ul of", input$dTTP_stock, "mM dTTP stock")
  })
  output$dCTP_volume <- renderText({
    paste("Add", round(x = vol_CTP(), 2), "ul of", input$dCTP_stock, "mM dCTP stock")
  })
  output$dGTP_volume <- renderText({
    paste("Add", round(x = vol_GTP(), 2), "ul of", input$dGTP_stock, "mM dGTP stock")
  })
  output$dNTP_water_volume <- renderText({
    dNTP_vol_water <- input$dNTP_Working_volume - vol_ATP() - vol_TTP() - vol_CTP() - vol_GTP()
    paste("Add", round(x = dNTP_vol_water, 2), "ul molecular biology grade water")
  })
   
  
}
# Run the application 
shinyApp(ui = ui, server = server)
