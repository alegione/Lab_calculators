library(tidyverse)
library(xlsx)
library(DT)
library(exact2x2)


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
                                     ),
                           # br(),
                           # actionButton(inputId = "TestTable",
                           #              label = "Load test data"
                           #              ),
                           NULL
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
              ),
              tabPanel(title = "2x2 table", icon = NULL,
                       fluidRow(
                         column(width = 2, offset = 3, h4("Outcome"))
                       ),
                       fluidRow(
                         column(width = 2, offset = 2, "Positive"),
                         column(width = 2, "Negative"),
                         column(width = 2, "Total"),
                         NULL
                       ),
                       fluidRow(
                         column(width = 1, h4("Variable")),
                         column(width = 1, "Positive"),
                         column(width = 2, numericInput(inputId = "cellA",label = NULL, value = 0,min = 0,step = 1)),
                         column(width = 2, numericInput(inputId = "cellB",label = NULL, value = 0,min = 0,step = 1)),
                         column(width = 2, textOutput(outputId = "Total_1")),
                         NULL
                       ),
                       fluidRow(
                         column(width = 1, offset = 1, "Negative"),
                         column(width = 2, numericInput(inputId = "cellC",label = NULL, value = 0,min = 0,step = 1)),
                         column(width = 2, numericInput(inputId = "cellD",label = NULL, value = 0,min = 0,step = 1)),
                         column(width = 2, textOutput(outputId = "Total_2")),
                         NULL
                       ),
                       fluidRow(
                         column(width = 2, offset = 2, textOutput(outputId = "Total_3")),
                         column(width = 2, textOutput(outputId = "Total_4")),
                         column(width = 2, textOutput(outputId = "Total_5")),
                         NULL
                       ),
                       
                       NULL
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
  
  
  # eventReactive(input$TestTable, {
  #   TableData <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt", sep  =",", header = T)
  #   output$DataTableShow <- DT::renderDataTable(TableData())
  # })
  #SERVER code for data display
  
  output$DataTableShow <- DT::renderDataTable(TableData())
  
  
  
  #SERVER code for 2x2 table
  
  output$Total_1 <- reactive({
    (input$cellA + input$cellB)
  })
  output$Total_2 <- reactive({
    (input$cellC + input$cellD)
  })
  output$Total_3 <- reactive({
    (input$cellA + input$cellC)
  })
  output$Total_4 <- reactive({
    (input$cellB + input$cellD)
  })
  output$Total_5 <- reactive({
    (input$cellA + input$cellB + input$cellC + input$cellD)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


# _____________________________
# 
# 2x2 contingency table
# _____________________________
# 
# Case     Control     Sum
# Exposed           99           8     107
# Not exposed       88          43     131
# --                --          --      --
#   Sum              187          51     238
# 
# _____________________________
# 
# Statistics
# _____________________________
# 
# 
# a= 99 
# b= 8 
# c= 88 
# d= 43
# 
# p1=a/(a+b)= 0.9252 
# p2=c/(c+d)= 0.6718 
# 
# _____________________________
# 
# Risk difference
# _____________________________
# 
# Risk difference = RD = p1-p2 = 0.2535
# Standard error = SE.RD = sqrt(p1*(1-p1)/(a+b)+p2*(1-p2)/(c+d)) = 0.2535
# Lower 95%-confidence limit: = RD - 1.96 * SE.RD = 0.1589
# Upper 95%-confidence limit: = RD + 1.96 * SE.RD = 0.3481
# 
# The estimated risk difference is 25.3% (CI_95%: [15.9;34.8]).
# 
# _____________________________
# 
# Risk ratio
# _____________________________
# 
# Risk ratio = RR = p1/p2 = 1.3773
# Standard error = SE.RR = sqrt((1-p1)/a+(1-p2)/c)= 1.3773
# Lower 95%-confidence limit: = RR * exp(- 1.96 * SE.RR) = 1.2079
# Upper 95%-confidence limit: = RR * exp(1.96 * SE.RR) = 1.5705
# 
# The estimated risk ratio is 1.377(CI_95%: [1.208;1.571]).
# 
# _____________________________
# 
# Odds ratio
# _____________________________
# 
# Odds ratio = OR = p1/p2 = 6.0469
# Standard error = SE.OR = sqrt((1/a+1/b+1/c+1/d)) = 6.0469
# Lower 95%-confidence limit: = OR * exp(- 1.96 * SE.OR) = 2.6969
# Upper 95%-confidence limit: = OR * exp(1.96 * SE.OR) = 13.5582
# 
# The estimated odds ratio is 6.047(CI_95%: [2.697;13.558]).
# 
# _____________________________
# 
# Chi-square test
# _____________________________
# 
# 
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  table2x2
# X-squared = 20.995, df = 1, p-value = 0.000004605
# 
# 
# _____________________________
# 
# Fisher's exact test
# _____________________________
# 
# 
# 	Fisher's Exact Test for Count Data
# 
# data:  table2x2
# p-value = 0.000001204
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   2.608378 15.606815
# sample estimates:
#   odds ratio 
# 6.004836


#########################################
# p = ggplot(data=RR_data,
#            aes(x = Group,y = RiskRatio, ymin = LowerLimit, ymax = UpperLimit ))+
#   geom_pointrange(aes(col=Group))+
#   geom_hline(aes(fill=Group),yintercept =1, linetype=2)+
#   xlab('Group')+ ylab("Risk Ratio (95% Confidence Interval)")+
#   geom_errorbar(aes(ymin=LowerLimit, ymax=UpperLimit,col=Group),width=0.5,cex=1)+ 
#   facet_wrap(~Condition,strip.position="left",nrow=9,scales = "free_y") +
#   theme(plot.title=element_text(size=16,face="bold"),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.x=element_text(face="bold"),
#         axis.title=element_text(size=12,face="bold"),
#         strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
#   coord_flip()
# p
# p + 
#   scale_y_log10(breaks=c(0.5,1,2),position="top",limits=c(0.5,2)) +
#   guides(col = guide_legend(reverse = TRUE))
