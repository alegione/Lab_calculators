

library(shiny)
library(tidyverse)
library(shinyjs)
library(data.table)
#library(gridExtra)
library(xlsx)


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
  useShinyjs(),
  inlineCSS(list('.form-control' = 'padding:5px; text-align: center;')),

# Application title
  titlePanel(title = "PCR assistant", windowTitle = "PCR assistant"),
  
  mainPanel(width = 20,
            tabsetPanel(
              tabPanel(title = "PCR Protocol set up", icon = NULL,
                       h3("PCR set up tool"),
                       fixedRow(
                         column(width = 2,
                                numericInput(inputId = "PCR_tube_volume", label = "Reaction volume", value = 25, min = 0, max = 200, step = 1)
                         ),
                         column(width = 2,
                                numericInput(inputId = "Template_volume", label = "Template volume", value = 5, min = 0, max = 200, step = 0.1)
                         ),
                         column(width = 2,
                                numericInput(inputId = "PCR_samples", label = "PCR samples", value = 8, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 2,
                                numericInput(inputId = "PCR_extra", label = "Extra volume (%)", value = 10, min = 0, max = 100, step = 1)
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                h4("Reagent")
                         ),
                         column(width = 2,
                                h4("Working Stock")
                         ),
                         column(width = 2, offset = 1,
                                h4("PCR conc.")
                         ),
                         column(width = 2, offset = 1,
                                h4("uL/reaction")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "P1_check", label = strong("Forward primer"), value = TRUE)
                                
                         ),
                         column(width = 2,
                                numericInput(inputId = "Fprimer_working_concentration", label = NULL, value = 10, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "Fprimer_PCR_concentration", label = NULL, value = 1, min = 0, max = 10, step = 0.01)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("Fprimer_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "P2_check", label = strong("Reverse primer"), value = TRUE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "Rprimer_working_concentration", label = NULL, value = 10, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "Rprimer_PCR_concentration", label = NULL, value = 1, min = 0, max = 10, step = 0.01)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("Rprimer_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "P3_check", label = strong("Third primer/probe"), value = FALSE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "Probe_working_concentration", label = NULL, value = 10, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "Probe_PCR_concentration", label = NULL, value = 1, min = 0, max = 10, step = 0.01)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("Probe_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "MgCl_check", label = strong("Magnesium Chloride"), value = TRUE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "MgCl_working_concentration", label = NULL, value = 25, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("mM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "MgCl_PCR_concentration", label = NULL, value = 2, min = 0, max = 10, step = 0.01)
                         ),
                         column(width = 1, 
                                p("mM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("MgCl_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "dNTP_check", label = strong("dNTPs"), value = TRUE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "dNTP_working_concentration", label = NULL, value = 1.25, min = 0, max = 50, step = 0.01)
                         ),
                         column(width = 1, 
                                p("mM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "dNTP_PCR_concentration", label = NULL, value = 200, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("dNTP_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "buffer_check", label = strong("PCR Buffer"), value = TRUE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "buffer_working_concentration", label = NULL, value = 5, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("X", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "buffer_PCR_concentration", label = NULL, value = 1, min = 0, max = 10, step = 0.01)
                         ),
                         column(width = 1, 
                                p("X", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("buffer_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "enzyme_check", label = strong("PCR Enzyme"), value = TRUE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "enzyme_working_concentration", label = NULL, value = 5, min = 0, max = 100, step = 0.01)
                         ),
                         column(width = 1, 
                                p("Units/uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "enzyme_PCR_concentration", label = NULL, value = 0.05, min = 0, max = 10, step = 0.001)
                         ),
                         column(width = 1, 
                                p("Units/uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("enzyme_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 3,
                                checkboxInput(inputId = "dye_check", label = strong("DNA dye"), value = FALSE)
                         ),
                         column(width = 2,
                                numericInput(inputId = "dye_working_concentration", label = NULL, value = 50, min = 0, max = 1000, step = 1)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 2,
                                numericInput(inputId = "dye_PCR_concentration", label = NULL, value = 5, min = 0, max = 100, step = 0.1)
                         ),
                         column(width = 1, 
                                p("uM", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         ),
                         column(width = 1, align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("dye_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       fixedRow(
                         column(width = 2,
                                strong("Water")
                         ),
                         column(width = 1, offset = 7,  align = "right",
                                div(style="padding-top:5px;padding-bottom:5px;padding-left;0px",
                                    textOutput("water_PCR_volume_sample")
                                )
                         ),
                         column(width = 1, 
                                p("uL", style="padding-top:5px;padding-bottom:5px;padding-left;0px")
                         )
                       ),
                       tags$hr(style="border-color: black;"),
                       fixedRow(
                         column(width = 6,
                                tableOutput(outputId = "PCR_setup_table")
                         ),
                         column(width = 4,
                                em(textOutput("PCR_volume_instructions")),
                                downloadButton(outputId = "PCR_protocol_download_tsv", label = "Save protocol to sheet (tsv)"),
                                p(),
                                downloadButton(outputId = "PCR_protocol_download_xlsx", label = "Save protocol to excel (xlsx)")
                         )
                       )
              ),
              tabPanel(title = "Primer dilutions", icon = NULL,
                       verticalLayout(
                         h3("Making up working stock from new primers"),
                         p("Concentration of lyophilised primer (nmol)"),
                         fluidRow(
                           column(width = 2, offset = 1,
                                  numericInput(inputId = "primer_neat_concentration", label = NULL, value = 30, min = 0, max = 100, step = 0.1)
                           )
                         ),
                         br(),
                         p("Desired concentration of stock solution (uM)"),
                         fluidRow(
                           column(width = 2, offset = 1,
                                  numericInput(inputId = "primer_stock_concentration", label = NULL, value = 100, min = 0, max = 1000, step = 1)                           )
                         ),
                         verbatimTextOutput("primer_stock_water"),
                         br(),
                         p("Desired composition of working solution"),
                         fluidRow(
                           column(width = 3,
                                  numericInput(inputId = "primer_working_concentration", label = "Concentation (uM)", value = 100, min = 0, max = 1000, step = 1)
                           ),
                           column(width = 3,
                                  numericInput(inputId = "primer_working_volume", label = "Volume (uL)", value = 1000, min = 0, max = 10000, step = 1)
                           )
                          ),
                         br(),
                         verbatimTextOutput("primer_working_water")
                       )
              ),
              tabPanel(title = "dNTP dilutions", icon = NULL,
                       verticalLayout(
                         h3("Concentration of stock dNTPs"),
                         fluidRow(
                           column(width = 2,
                                  numericInput(inputId = "dATP_stock", label = "dATP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           ),
                           column(width = 2,
                                  numericInput(inputId = "dTTP_stock", label = "dTTP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           ),
                           column(width = 2,
                                  numericInput(inputId = "dCTP_stock", label = "dCTP (mM)", value = 100, min = 0, max = 1000, step = 0.1)
                           ),
                           column(width = 2,
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

Volume1Calc <- function(C1,C2,V2) {
  (C2 * V2) / C1
  }

server <- function(input, output, session) {
  # SERVER code for PCR protocol tab
  Fprimer_vol_reactive <- reactive({
    if (input$P1_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$Fprimer_working_concentration, C2 = input$Fprimer_PCR_concentration)
    } else {
      0
    }
  })
  Rprimer_vol_reactive <- reactive({
    if (input$P2_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$Rprimer_working_concentration, C2 = input$Rprimer_PCR_concentration)
    } else {
      0
    }
  })
  Probe_vol_reactive <- reactive({
    if (input$P3_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$Probe_working_concentration, C2 = input$Probe_PCR_concentration)
    } else {
      0
    }
  })
  MgCl_vol_reactive <- reactive({
    if (input$MgCl_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$MgCl_working_concentration, C2 = input$MgCl_PCR_concentration)
    } else {
      0
    }
  })
  dNTP_vol_reactive <- reactive({
    if (input$dNTP_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$dNTP_working_concentration * 1000, C2 = input$dNTP_PCR_concentration)
    } else {
      0
    }
  })
  buffer_vol_reactive <- reactive({
    if (input$buffer_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$buffer_working_concentration, C2 = input$buffer_PCR_concentration)
    } else {
      0
    }    
  })
  enzyme_vol_reactive <- reactive({
    if (input$enzyme_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$enzyme_working_concentration, C2 = input$enzyme_PCR_concentration)
    } else {
      0
    } 
  })
  dye_vol_reactive <- reactive({
    if (input$dye_check == TRUE) {
      Volume1Calc(V2 = input$PCR_tube_volume, C1 = input$dye_working_concentration, C2 = input$dye_PCR_concentration)
    } else {
      0
    }   
  })
  water_vol_reactive <- reactive({
    vol <- input$PCR_tube_volume - input$Template_volume
    vol <- vol - Fprimer_vol_reactive()
    vol <- vol - Rprimer_vol_reactive()
    vol <- vol - Probe_vol_reactive()
    vol <- vol - MgCl_vol_reactive()
    vol <- vol - dNTP_vol_reactive()
    vol <- vol - buffer_vol_reactive()
    vol <- vol - enzyme_vol_reactive()
    vol <- vol - dye_vol_reactive()
    vol
  })
  output$Fprimer_PCR_volume_sample <- renderText({
    Fprimer_vol_reactive()
  })
  output$Rprimer_PCR_volume_sample <- renderText({
    Rprimer_vol_reactive()
  })
  output$Probe_PCR_volume_sample <- renderText({
    Probe_vol_reactive()
  })
  output$MgCl_PCR_volume_sample <- renderText({
    MgCl_vol_reactive()
  })
  output$dNTP_PCR_volume_sample <- renderText({
    dNTP_vol_reactive()
  })
  output$buffer_PCR_volume_sample <- renderText({
    buffer_vol_reactive()
  })
  output$enzyme_PCR_volume_sample <- renderText({
    enzyme_vol_reactive()
  })
  output$dye_PCR_volume_sample <- renderText({
    dye_vol_reactive()
  })
  output$water_PCR_volume_sample <- renderText({
    water_vol_reactive()
  })
  PCR_multiplier <- reactive({
    if (input$PCR_extra != 0) {
      input$PCR_samples * ( 1 + (input$PCR_extra / 100))
    } else {
      input$PCR_samples
    }
  })
  PCR_table_reactive <- reactive({
    sum_PCR_Vol <- sum(Fprimer_vol_reactive(), Rprimer_vol_reactive(),Probe_vol_reactive(), MgCl_vol_reactive(), dNTP_vol_reactive(), buffer_vol_reactive(), enzyme_vol_reactive(), dye_vol_reactive(), water_vol_reactive(), na.rm = TRUE) * PCR_multiplier()
    PCRtable <- data.table(
      Reagent = c(if (Fprimer_vol_reactive() > 0) {"Forward Primer"},
                  if (Rprimer_vol_reactive() > 0) {"Reverse Primer"},
                  if (Probe_vol_reactive() > 0) {"Probe/Primer"}, 
                  if (MgCl_vol_reactive() > 0) {"MgCl"},
                  if (dNTP_vol_reactive() > 0) {"dNTP"},
                  if (buffer_vol_reactive() > 0) {"Buffer"},
                  if (enzyme_vol_reactive() > 0) {"Enzyme"},
                  if (dye_vol_reactive() > 0) {"Dye"},
                  if (water_vol_reactive() > 0) {"Water"}, "Total"),
      Conc. = c(if (Fprimer_vol_reactive() > 0) {paste(input$Fprimer_PCR_concentration, "uM")},
                if (Rprimer_vol_reactive() > 0) {paste(input$Rprimer_PCR_concentration, "uM")},
                if (Probe_vol_reactive() > 0) {paste(input$Probe_PCR_concentration, "uM")},
                if (MgCl_vol_reactive() > 0) {paste(input$MgCl_PCR_concentration, "mM")},
                if (dNTP_vol_reactive() > 0) {paste(input$dNTP_PCR_concentration, "uM")},
                if (buffer_vol_reactive() > 0) {paste(input$buffer_PCR_concentration, "X")},
                if (enzyme_vol_reactive() > 0) {paste(input$enzyme_PCR_concentration, "U/uL")},
                if (dye_vol_reactive() > 0) {paste(input$dye_PCR_concentration, "uM")},"",""),
      Volume = c(c(if (Fprimer_vol_reactive() > 0) {Fprimer_vol_reactive()},
                   if (Rprimer_vol_reactive() > 0) {Rprimer_vol_reactive()},
                   if (Probe_vol_reactive() > 0) {Probe_vol_reactive()},
                   if (MgCl_vol_reactive() > 0) {MgCl_vol_reactive()},
                   if (dNTP_vol_reactive() > 0) {dNTP_vol_reactive()},
                   if (buffer_vol_reactive() > 0) {buffer_vol_reactive()},
                   if (enzyme_vol_reactive() > 0) {enzyme_vol_reactive()},
                   if (dye_vol_reactive() > 0) {dye_vol_reactive()},
                   if (water_vol_reactive() > 0) {water_vol_reactive()}) * PCR_multiplier(),
                 c(sum_PCR_Vol)),
      uL = c("uL")
    )
  })
  
  output$PCR_setup_table <- renderTable({
    PCR_table_reactive()
  })
  
  
  
  output$PCR_volume_instructions <- renderText({
    paste("Add", input$PCR_tube_volume - input$Template_volume, "uL of mastermix per tube")
  })
  
  output$PCR_protocol_download_tsv <- downloadHandler(
    filename = function() {
      paste("PCR_protocol_", Sys.Date(), ".tsv", sep = "")
    },
    content = function(file) {
      write_tsv(PCR_table_reactive(), file)
    }
  )
  
  output$PCR_protocol_download_xlsx <- downloadHandler(
    filename = function() {
      paste("PCR_protocol_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(PCR_table_reactive(), file, sheetName = "PCR_protocol", row.names = FALSE)
    }
  )
  # SERVER code for primer tab
  output$primer_stock_water <- renderText({
    primer_neat_to_stock <- (input$primer_neat_concentration * 1000) / input$primer_stock_concentration
    paste("Add", primer_neat_to_stock, "uL of water to make", input$primer_stock_concentration, "uM (pmol/uL) primer stock")
  })
  output$primer_working_water <- renderText({
    primer_stock_to_working <- input$primer_working_concentration * input$primer_working_volume / input$primer_stock_concentration
  })

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
    paste("Add", round(x = vol_ATP(), 2), "uL of", input$dATP_stock, "mM dATP stock")
  })
  output$dTTP_volume <- renderText({
    paste("Add", round(x = vol_TTP(), 2), "uL of", input$dTTP_stock, "mM dTTP stock")
  })
  output$dCTP_volume <- renderText({
    paste("Add", round(x = vol_CTP(), 2), "uL of", input$dCTP_stock, "mM dCTP stock")
  })
  output$dGTP_volume <- renderText({
    paste("Add", round(x = vol_GTP(), 2), "uL of", input$dGTP_stock, "mM dGTP stock")
  })
  output$dNTP_water_volume <- renderText({
    dNTP_vol_water <- input$dNTP_Working_volume - vol_ATP() - vol_TTP() - vol_CTP() - vol_GTP()
    paste("Add", round(x = dNTP_vol_water, 2), "uL molecular biology grade water")
  })
   
  
}
# Run the application 
shinyApp(ui = ui, server = server)
