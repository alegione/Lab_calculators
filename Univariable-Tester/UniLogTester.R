library(tidyverse)
library(xlsx)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Univariable Tester", windowTitle = "Univariable Tester"),
  
  mainPanel(width = 20,
            tabsetPanel(
              tabPanel(title = "Data Sheet", icon = NULL,
                       sidebarLayout(
                         sidebarPanel(
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
  
}


# Run the application 
shinyApp(ui = ui, server = server)