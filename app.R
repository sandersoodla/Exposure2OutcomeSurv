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

    fluidRow(
        column(width = 11,
            # Application title
            titlePanel("Start condition to target condition overview")
        ),
        column(width = 1,
            textOutput("dbName"),
            textOutput("personCount")
        )
    ),

    
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
            ),
            width = 3
        ),

        mainPanel(
            textOutput("percentageOfTargetText"),
            textOutput("percentageOfTarget1"),
            textOutput("percentageOfTarget3"),
            textOutput("percentageOfTarget5"),
            
            tabsetPanel(
                tabPanel("Demographic overview", 
                    fluidRow(
                      column(6, plotOutput("patientPyramid1")),
                      column(6, plotOutput("patientPyramid2"))
                    ),
                ),
                tabPanel("Trajectories containing start condition", DTOutput("trajectoryTable")),
                tabPanel("Patient Timeline", plotOutput("patientTimeline")),
                tabPanel("Start to target", DTOutput("startToTargetConditionTable"))
            ),
            width = 9
        )
    )
)





source("scripts/getConditionInfo.R")
source("scripts/conditionToCondition.R")
source("scripts/demographicAnalysis.R")
source("scripts/getMetadata.R")



# Define server logic 
server <- function(input, output, session) {
  
    DATABASE <- paste(Sys.getenv("DB_HOST"),"/",Sys.getenv("DB_NAME"),sep='')
    DB_USER <- Sys.getenv('DB_USERNAME')
    DB_PASSWORD <- Sys.getenv('DB_PASSWORD')
    DB_PORT <- Sys.getenv('DB_PORT')
    
    
    #connectionDetails <- DatabaseConnector::createConnectionDetails(
    #  dbms = "your_dbms",
    #  server = "your_server_address",
    #  user = "your_username",
    #  password = "your_password",
    #  schema = "your_cdm_schema",
    #  port = 1
    #)
    
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "duckdb", server = "c:/temp/EunomiaData/GiBleed_5.3.duckdb")
    connection <- DatabaseConnector::connect(connectionDetails)
    
    cdm <- CDMConnector::cdmFromCon(connection, cdmSchema = NULL)
    
    
    metadata <- getMetadata(cdm)
    
    output$dbName <- renderText(metadata$dbName)
    output$personCount <- renderText(paste("n = ", metadata$personCount))
    
    output$startConditionText <- renderText(paste(input$startConditionId, getConditionInfo(cdm, input$startConditionId)))
    
    output$targetConditionText <- renderText(paste(input$targetConditionId, getConditionInfo(cdm, input$targetConditionId)))
    
    
    # Reactive expression to fetch data when the button is clicked
    trajectoriesData <- eventReactive(input$getData, {
      req(input$startConditionId > 0)
      req(input$targetConditionId > 0)
      
      df <- getTrajectoriesForCondition(cdm, input$startConditionId)
      return(df)
    })
    
    startToTargetConditionDF <- eventReactive(trajectoriesData(), {
      req(trajectoriesData())
      
      df <- createStartToTargetConditionDF4(trajectoriesData(), input$startConditionId, input$targetConditionId)
      return(df)
    })
    
    resultPercentages <- reactive({
      calculateStartToTargetPercentages(startToTargetConditionDF())
    })

    output$percentageOfTargetText <- renderText("Chance of target condition from start condition")
    output$percentageOfTarget1 <- renderText(paste("in 1 year:", resultPercentages()$target_percentage_1y, "%"))
    output$percentageOfTarget3 <- renderText(paste("in 3 years:", resultPercentages()$target_percentage_3y, "%"))
    output$percentageOfTarget5 <- renderText(paste("in 5 years:", resultPercentages()$target_percentage_5y, "%"))
    
    
    
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
        choices = unique(trajectoriesData()$person_id),
        server = TRUE
      )
    })
    
    # Render the patient timeline plot
    output$patientTimeline <- renderPlot({
      req(trajectoriesData())
      req(input$selectedPatient)
      patientData <- trajectoriesData() %>%
        filter(person_id == input$selectedPatient)
      
      ggplot(patientData, aes(x = CONDITION_START_DATE, y = CONCEPT_NAME)) +
        geom_point() +
        labs(
          title = paste("Condition timeline for Patient", input$selectedPatient),
          x = "Date",
          y = "Condition"
        ) +
        theme_minimal()
    })
    
    # Render the patient pyramid plots
    output$patientPyramid1 <- renderPlot({
      req(trajectoriesData())
      req(input$startConditionId > 0)

      createPopulationPyramidForCondition(cdm, input$startConditionId)
      
    })
    
    output$patientPyramid2 <- renderPlot({
      req(trajectoriesData())
      req(input$targetConditionId > 0)
      
      createPopulationPyramidForCondition(cdm, input$targetConditionId)
      
    })
    
    
    
    session$onSessionEnded(function() {
      # disconnect DB
      CDMConnector::cdmDisconnect(cdm)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
