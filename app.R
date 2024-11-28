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
library(DT)
library(ggplot2)

# Define UI for application 
ui <- fluidPage(
    theme = bs_theme(bootswatch = "cerulean"),

    # Application title
    titlePanel("Start condition to target condition overview"),

    
    sidebarLayout(
        sidebarPanel(
            numericInput("startConditionId", "Start Condition concept id", value = "40481087", min = 0),
            textOutput("startConditionText"),
            hr(),
            numericInput("targetConditionId", "Target Condition concept id", value = "372328", min = 0),
            textOutput("targetConditionText"),
            actionButton("getData", "Get stats"),
            hr(),
            selectizeInput(
              "selectedPatient",
              "Select a Patient ID:",
              choices = NULL  # Initialize with no choices
            )
        ),

        mainPanel(
            textOutput("percentageOfTargetText"),
            textOutput("percentageOfTarget1"),
            textOutput("percentageOfTarget3"),
            textOutput("percentageOfTarget5"),
            textOutput("demographicOverviewText"),
            
            tabsetPanel(
              tabPanel("Trajectories containing start condition", DTOutput("trajectoryTable")),
              tabPanel("Patient Timeline", plotOutput("patientTimeline")),
              tabPanel("Start to target", DTOutput("startToTargetConditionTable"))
            ),
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
    
    
    # Reactive expression to fetch data when the button is clicked
    trajectoriesData <- eventReactive(input$getData, {
      req(input$startConditionId > 0)
      req(input$targetConditionId > 0)
      
      df <- getTrajectoriesForCondition(connection, input$startConditionId)
      return(df)
    })
    
    startToTargetConditionDF <- eventReactive(input$getData, {
      req(input$startConditionId > 0)
      req(input$targetConditionId > 0)
      
      df <- createStartToTargetConditionDF4(connection, input$startConditionId, input$targetConditionId)
      return(df)
    })
    
    resultPercentages <- reactive({
      calculateStartToTargetPercentages(startToTargetConditionDF())
    })

    output$percentageOfTargetText <- renderText("Chance of target condition from start condition")
    output$percentageOfTarget1 <- renderText(paste("in 1 year:", resultPercentages()$target_percentage_1y, "%"))
    output$percentageOfTarget3 <- renderText(paste("in 3 years:", resultPercentages()$target_percentage_3y, "%"))
    output$percentageOfTarget5 <- renderText(paste("in 5 years:", resultPercentages()$target_percentage_5y, "%"))
    
    output$demographicOverviewText <- renderText("Demographic overview")
    
    
    # Render the trajectories table
    output$trajectoryTable <- renderDT({
      req(trajectoriesData())
      datatable(
        trajectoriesData(),
        options = list(pageLength = 10),
        filter = 'top'
      )
    })
    
    # Render the start to target condition table
    output$startToTargetConditionTable <- renderDT({
      req(startToTargetConditionDF())
      datatable(
        startToTargetConditionDF(),
        options = list(pageLength = 10),
        filter = 'top'
      )
    })
    
    # Update the patient selector when trajectories Data changes
    observeEvent(trajectoriesData(), {
      updateSelectizeInput(
        session,
        "selectedPatient",
        choices = unique(trajectoriesData()$PERSON_ID),
        server = TRUE
      )
    })
    
    # Render the patient timeline plot
    output$patientTimeline <- renderPlot({
      req(trajectoriesData())
      req(input$selectedPatient)
      patientData <- trajectoriesData() %>%
        filter(PERSON_ID == input$selectedPatient)
      
      ggplot(patientData, aes(x = CONDITION_START_DATE, y = CONCEPT_NAME)) +
        geom_point() +
        labs(
          title = paste("Condition timeline for Patient", input$selectedPatient),
          x = "Date",
          y = "Condition"
        ) +
        theme_minimal()
    })
    
    
    session$onSessionEnded(function() {
      # disconnect DB
      DatabaseConnector::disconnect(connection)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
