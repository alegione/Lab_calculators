

library(shiny)
library(tidyverse)
library(data.table)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(title = "Virus titration calculators", windowTitle = "Virus titration calculators"),
  
  mainPanel(width = 20,
            tabsetPanel(
              tabPanel("PFU",
                       verticalLayout(
                         h1('Plaque forming units'),
                         h2('Input data'),
                         fluidRow(
                           column(3,
                                  numericInput(inputId = "PFUPlate1", label = "Plate 1", value = 0, min = 0, max = 1000, step = 1)
                           ),
                           column(3, 
                                  numericInput(inputId = "PFUPlate2", label = "Plate 2", value = 0, min = 0, max = 1000, step = 1)
                           ),
                           column(3,
                                  numericInput(inputId = "PFUPlate3", label = "Plate 3 *", value = -1, min = -1, max = 1000, step = 1)
                           )
                         ),
                         fluidRow(
                           column(3,
                                  numericInput(inputId = "PFUVolume", label = "Inoculum (mL)", value = 0.400, min = 0, max = 2, step = 0.001)
                           ),
                           column(4,
                                  numericInput(inputId = "PFUDilutionFactor", label = "Dilution factor (10^x)", value = -3, min = -10, max = 0, step = 1)
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
                          h2('Input data'),
                          fluidRow(
                            column(3,
                                   numericInput(inputId = "VolumeTCID50", label = "Inoculum (mL)", value = 0.400, min = 0, max = 2, step = 0.001)
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
                       verticalLayout(
                         h3('Number of infected wells')
                       ),
                       uiOutput("DilutionSeries"),
                        verticalLayout(
                          h2('Results'),
                          radioButtons(inputId = "TCID50_Method", label = "Method", choices = c("Spearman-Karber", "Reed & Muench"), selected = "Spearman-Karber"),
                          verbatimTextOutput(outputId = "TCID50_text"),
                          verbatimTextOutput(outputId = "TCID50_per_mL_text"),
                          column(width = 6,
                                tableOutput(outputId = "PCR_setup_table")
                          ),
                        verticalLayout(
                          htmlOutput(outputId = "TCID50_Method_text")
                                                  #https://www.google.com.au/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=0ahUKEwiF8N22-KbaAhXGVrwKHdB6CvgQFgg0MAE&url=https%3A%2F%2Fwww.klinikum.uni-heidelberg.de%2Ffileadmin%2Finst_hygiene%2Fmolekulare_virologie%2FDownloads%2FTCID50_calculator_v2_17-01-20_MB.xlsx&usg=AOvVaw2FecwX0Pz6446j5PoL1S29
                          )
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
                            <br>The formulas for each tool are included in their respective tabs
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

    # Average the PFU from either two or three plates, depending on the value of the plate 3 variable
  AveragePFUreactive <- reactive({
    if (input$PFUPlate1 != 0 && input$PFUPlate2 != 0){
      if (input$PFUPlate3 == -1) {
        ((input$PFUPlate1 + input$PFUPlate2) / 2)
      } else {
        ((input$PFUPlate1 + input$PFUPlate2 + input$PFUPlate3) / 3)
      }
    } else {
      0
    }
  })
  PFU_per_ml_reactive <- reactive({
    if (AveragePFUreactive() != 0){
      AveragePFUreactive() / (10^input$PFUDilutionFactor * input$PFUVolume)
    } else {
      0
    }
  })
  output$PFU_per_mL <- renderText({
    if (input$PFUPlate1 == 0 || input$PFUPlate2 == 0){
      paste0("Values of at least two counts (eg Plate 1 and Plate 2) must be greater than zero")
    } else { 
      paste0("Titre = ", round(x = PFU_per_ml_reactive(), 2), " PFU/mL\n\t10^",  round(x = log10(PFU_per_ml_reactive()), 2), " PFU/mL")
    }
  })
  output$Average_PFU <- renderText({
    if (input$PFUPlate1 == 0 || input$PFUPlate2 == 0){
      paste("")
    } else if (input$PFUPlate3 == -1) {
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
      negative <- negative + (input$WellsTCID50 - (input[[val]]))
    }
    negative
  })
  
   TCID50_midpoint <- reactive({
     Reed_Muench_table <- data.table(
                                    Dilutions = c(seq(from = input$DilutionLowTCID50, to = input$DilutionHighTCID50, by = -log10(input$DilutionFactorTCID50)))
                                    )
     for (i in seq(from = 1, to = nrow(Reed_Muench_table), by = 1)) {
       val <- paste0("n",abs(Reed_Muench_table$Dilutions[i]))
       print(paste0("val = ", val))
       Reed_Muench_table$Dead[i] <- (input[[val]])
     }
     for (i in seq(from = 1, to = nrow(Reed_Muench_table), by = 1)) {
       Reed_Muench_table$Alive[i] <- input$WellsTCID50 - Reed_Muench_table$Dead[i]
       if (i == 1) {
         Reed_Muench_table$Accum_Alive[i] <- Reed_Muench_table$Alive[i]
       } else {
         Reed_Muench_table$Accum_Alive[i] <- Reed_Muench_table$Accum_Alive[i-1] + Reed_Muench_table$Alive[i]
       }
       # switch <- TRUE
     }
     for (i in seq(from = nrow(Reed_Muench_table), to = 1, by = -1)) {
       if (i == nrow(Reed_Muench_table)) {
         Reed_Muench_table$Accum_Dead[i] <- Reed_Muench_table$Dead[i]
       } else {
         Reed_Muench_table$Accum_Dead[i] <- Reed_Muench_table$Accum_Dead[i+1] + Reed_Muench_table$Dead[i]
       }
       Reed_Muench_table$PercentDead[i] <- round(x = (100 * (Reed_Muench_table$Accum_Dead[i] / ( Reed_Muench_table$Accum_Dead[i] + Reed_Muench_table$Accum_Alive[i]))), digits = 2)
       # switch <- TRUE
     } 
     print(Reed_Muench_table)
    for (i in seq(from = 1, to = nrow(Reed_Muench_table), by = 1)) {
      j <- i + 1
      val_High <- round(x = Reed_Muench_table$PercentDead[i], digits = 0)
      val_Low <- round(x = Reed_Muench_table$PercentDead[i+1], digits = 0)
      # check that you haven't got to the end
      if ( j > nrow(Reed_Muench_table)) {
        returnlist <- c("ERROR 1")
        break
      }
        
      if ( val_High > 50 && val_Low < 50) {
        returnlist <- c(val_High,val_Low, Reed_Muench_table$Dilution[i])
        break
      }
    }
    returnlist
  })


  TCID50_reactive <- reactive({
  if ( input$TCID50_Method == "Spearman-Karber" ) {
        abs(input$DilutionHighTCID50) + (0.5 * log10(input$DilutionFactorTCID50)) - ((log10(input$DilutionFactorTCID50) * TCID50_negatives()) / input$WellsTCID50)
    } else if (input$TCID50_Method == "Reed & Muench") {
      if (TCID50_midpoint()[1] == "ERROR 1") {
        "ERROR 1 - No values below 50%"
      } else {
        highprop <- as.numeric(TCID50_midpoint()[1])
        lowprop <- as.numeric(TCID50_midpoint()[2])
        highdilution <- as.numeric(TCID50_midpoint()[3])
        PD <- (highprop - 50)/(highprop - lowprop)
        abs(highdilution - (PD * log10(input$DilutionFactorTCID50)))
      }
        
    }
  })
  output$TCID50_text <- renderText({
    if ( is.numeric(TCID50_reactive())) {
      paste0("TCID50 = 10^", round(x = TCID50_reactive(), digits = 1))
    } else {
      "ERROR (There must be one dilution either side of 50%)"
    }
  })
  output$TCID50_per_mL_text <- renderText({
    if ( is.numeric(TCID50_reactive())) {
      paste0("Titre = 10^", round(log10((10^TCID50_reactive()) / input$VolumeTCID50), 1), " TCID50/mL")
    } else {
      "ERROR (There must be one dilution either side of 50%)"
    }
  })
  IC50results_table_reactive <- reactive({
    highprop <- as.numeric(TCID50_midpoint()[1])
    lowprop <- as.numeric(TCID50_midpoint()[2])
    highdilution <- as.numeric(TCID50_midpoint()[3])
    PD <- (highprop - 50)/(highprop - lowprop)
    ID50 <- abs(highdilution - (PD * log10(input$DilutionFactorTCID50)))
    
      if ( input$TCID50_Method == "Spearman-Karber" ) {
        Datatable <- data.table(
          variables = c("Dilution Level (x)",
                        "interval (d)",
                        "sum of uninfected hosts",
                         "Hosts per diltuion (n)"
                        ),
          values = c(abs(input$DilutionHighTCID50),
                        log10(input$DilutionFactorTCID50),
                        TCID50_negatives(),
                         input$WellsTCID50
                     )
        )
      } else {
        Datatable <- data.table(
          variables = c("% infected above 50%",
                        "% infected below 50%",
                        "PD",
                        "log dilution above 50",
                        "PD x log dilution factor"
                       ),
          values = c(highprop,
                     lowprop,
                     PD,
                     highdilution,
                     ID50
                     )
        )
      }
  })
  
  output$PCR_setup_table <- renderTable({
    IC50results_table_reactive()
  })
  
  
  
  
  output$TCID50_Method_text <- renderText({
    if ( input$TCID50_Method == "Spearman-Karber" ) {
         HTML("<br><u><b>Equation for calculation (Spearman-Karber formula): </b></u>
         <br> <br>
         <b> TCID50 = 10 ^ (highest dilution + dilution step * ( 0.5 - ((1 / wells per dilution) * sum of the negative wells)))</b></p>
         TCID50/mL = TCID50 / inoculum volume (mL)<p>
         where: <br>
         highest dilution = The value of the highest exponent as an absolute integer <br>
         <p style='margin-left: 110px'>e.g. in a serial ten fold dilution from 10^-1 to 10^-6, the value would be 6</p>
         wells per dilution = The total number of wells inoculated at each dilution</p>
         sum of the negative wells = The total number of wells that are free of cytopathic effect (i.e. 'negative' wells)</p>")
    } else {
      HTML("<br><u><b>Equation for calculation (Reed and Muench formula): </b></u>
            <br> <br>
            <b> PD = (%>50% - 50%) / (%>50% - %<50%)</b></p>
          
            <b> TCID50 = 10 ^ (log10 dilution above 50% infected - (PD * log10 dilution factor))</b></p>
            TCID50/mL = TCID50 / inoculum volume (mL)<p>
            where: <br>
            PD = proportionate distance between two dilutions<br>
            %>50% = the percentage of wells infected at the dilution above 50%<br>
            %<50% = the percentage of wells infected at the dilution below 50%<br>
            highest dilution = The value of the highest exponent as an absolute integer <br>
            <p style='margin-left: 110px'>e.g. in a serial ten fold dilution where the two values either side of 50% were 10^-3 to 10^-4, the value would be 3</p>")

      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
