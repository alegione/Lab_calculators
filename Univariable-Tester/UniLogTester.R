library(tidyverse)
library(xlsx)
library(DT)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Univariable Tester", windowTitle = "Univariable Tester"),
  
  mainPanel(width = 20,
            tabsetPanel(
              tabPanel(title = "Data Sheet", icon = NULL,
                       sidebarLayout(
                         sidebarPanel(width = 3,
                           fileInput(inputId = "DataTableLoad",
                                     label = "Load excel sheet",
                                     multiple = FALSE
                                     )
                           ),
                         mainPanel(
                           dataTableOutput("DataTableShow")
                           )
                       )
              ),
              tabPanel(title = "Data Analysis", icon = NULL,
                       sidebarLayout(
                         sidebarPanel(
                         ),
                         mainPanel(
                         )
                       )
              ),
              tabPanel(title = "Forest Plots", icon = NULL,
                       sidebarLayout(
                         sidebarPanel(
                         ),
                         mainPanel(
                         )
                       )
              )
            )
  )
              
                       
                       
  
  
)



server <- function(input, output, session) {
  TableData <- reactive({
    inFile <- input$DataTableLoad
    if (is.null(inFile)) {
      return(NULL)
    }
    tbl <- read.xlsx(file = inFile$datapath, sheetIndex = 1, header=TRUE, as.data.frame = TRUE)
    return(tbl)
  })
  #SERVER code for data display
  
  output$DataTableShow <- DT::renderDataTable(TableData())
  
}


# Run the application 
shinyApp(ui = ui, server = server)