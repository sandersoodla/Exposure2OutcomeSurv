#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Start condition to target condition overview"),

    
    sidebarLayout(
        sidebarPanel(
            numericInput("startConditionId", "Start Condition concept id", value = "40481087"),
            textOutput("startConditionText"),
            numericInput("targetConditionId", "Target Condition concept id", value = "372328"),
            textOutput("targetConditionText"),
        ),

        mainPanel(
            #tableOutput("summary"),
            textOutput("percentageOfTargetText"),
            textOutput("percentageOfTarget1"),
            textOutput("percentageOfTarget3"),
            textOutput("percentageOfTarget5"),
            textOutput("demographicOverviewText"),
            textOutput("trajectoryDisplayText"),
            tableOutput("startToTargetCondition")
        )
    )
)

source("scripts/getConditionInfo.R")
source("scripts/conditionToCondition.R")

# Define server logic 
server <- function(input, output, session) {
  
    connection <- DatabaseConnector::connect(dbms = "sqlite", server = "c:/temp/EunomiaData/GiBleed_5.3.sqlite")
    
    output$startConditionText <- renderText(paste(input$startConditionId, getConditionInfo(connection, input$startConditionId)))
    
    output$targetConditionText <- renderText(paste(input$targetConditionId, getConditionInfo(connection, input$targetConditionId)))
    
    
    startToTargetConditionDF <- reactive({
      req(input$startConditionId)
      req(input$targetConditionId)
      
      createStartToTargetConditionDF4(connection, input$startConditionId, input$targetConditionId)
    })
    
    resultPercentages <- reactive({
      calculateStartToTargetPercentages(startToTargetConditionDF())
    })

    #output$summary <- renderTable(resultSummary())
    output$percentageOfTargetText <- renderText("Chance of target condition from start condition")
    output$percentageOfTarget1 <- renderText(paste("in 1 year:", resultPercentages()$target_percentage_1y, "%"))
    output$percentageOfTarget3 <- renderText(paste("in 3 years:", resultPercentages()$target_percentage_3y, "%"))
    output$percentageOfTarget5 <- renderText(paste("in 5 years:", resultPercentages()$target_percentage_5y, "%"))
    
    output$demographicOverviewText <- renderText("Demographic overview")
    
    output$trajectoryDisplayText <- renderText("Trajectories for start condition")
    
    output$startToTargetCondition <- renderTable(head(startToTargetConditionDF(), 100))
    
    
    session$onSessionEnded(function() {
      # disconnect DB
      DatabaseConnector::disconnect(connection)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
