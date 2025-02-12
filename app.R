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
      selectizeInput(
        "startConditionId",
        "Start Condition concept",
        choices = NULL,
        selected = "",
        multiple = FALSE,
        options = list(placeholder = "Type to search conditions")
      ),
      textOutput("startConditionText"),
      hr(),
      selectizeInput(
        "targetConditionId",
        "Target Condition concept",
        choices = NULL,
        selected = "",
        multiple = FALSE,
        options = list(placeholder = "Type to search conditions")
      ),
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
      uiOutput("percentageOutputs"),
      
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
source("scripts/conceptRelations.R")



# Define server logic 
server <- function(input, output, session) {
  
  #################### CONNECTION SETUP ###########
  
  DATABASE <- paste(Sys.getenv("DB_HOST"),"/",Sys.getenv("DB_NAME"),sep='')
  DB_USER <- Sys.getenv('DB_USERNAME')
  DB_PASSWORD <- Sys.getenv('DB_PASSWORD')
  DB_PORT <- Sys.getenv('DB_PORT')
  
  CDM_SCHEMA <- Sys.getenv("CDM_SCHEMA")
  WRITE_SCHEMA <- Sys.getenv("WRITE_SCHEMA")
  
  #connectionDetails <- DatabaseConnector::createConnectionDetails(
  #  dbms = "your_dbms",
  #  server = "your_server_address",
  #  user = "your_username",
  #  password = "your_password",
  #  schema = "your_cdm_schema",
  #  port = 1
  #)
  
  #connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "duckdb", server = "c:/temp/EunomiaData/GiBleed_5.3.duckdb")
  #connection <- DatabaseConnector::connect(connectionDetails)
  
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = "c:/temp/EunomiaData/GiBleed_5.3.duckdb")
  
  cdm <- CDMConnector::cdmFromCon(connection, cdmSchema = CDM_SCHEMA, writeSchema = WRITE_SCHEMA)
  
  
  
  ############################# OUTPUTS ##############
  
  
  # METADATA
  metadata <- getMetadata(cdm)
  
  output$dbName <- renderText(metadata$dbName)
  output$personCount <- renderText(paste("n = ", metadata$personCount))
  
  
  ######### CONDITION INPUT
  
  lastStartCondition <- reactiveVal("")
  lastTargetCondition <- reactiveVal("")
  
  # Fetch all condition concepts for initial choices using getAllConditions function
  allConditionConcepts <- getAllConditions(cdm)
  
  # Update choices for selectizeInput
  updateConditionChoices <- function(session, inputId, conceptId = "") {
    print(paste("Updating choices for:", inputId, "with conceptId:", conceptId))
    if (conceptId == "" | is.na(conceptId)) {
      
      choiceList <- setNames(allConditionConcepts$concept_id, allConditionConcepts$concept_name_id)
      updateSelectizeInput(session, inputId, choices = choiceList, server = FALSE,
                           selected = "",
                           options = list(
                             placeholder = "Type to search conditions",
                             maxOptions = 10,
                             searchField = "label"
                           ))
      
    } else {
      # Fetch related concepts for dynamic update
      choices <- getRelatedConcepts(cdm, conceptId)
      choiceList <- setNames(choices$concept_id, choices$concept_name_id)
      updateSelectizeInput(session, inputId, choices = choiceList, server = FALSE,
                           selected = conceptId, # Keep the current selection
                           options = list(
                             placeholder = "Type to search related conditions",
                             maxOptions = 10,
                             searchField = "label"
                           ))
    }
  }
  
  
  ## Initialize start condition choices
  updateConditionChoices(session, "startConditionId", "")
  
  ## Initialize target condition choices
  updateConditionChoices(session, "targetConditionId", "")
  
  
  # Observer that updates the input’s choices when the user selects a start condition.
  observeEvent(input$startConditionId, {
    # Check if the new value differs from the last updated value.
    if (identical(input$startConditionId, lastStartCondition())) {
      return()
    }
    
    # Save the new value
    lastStartCondition(input$startConditionId)
    
    # Now update the input’s choices
    updateConditionChoices(session, "startConditionId", input$startConditionId)

  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  # Observer that updates the input’s choices when the user selects a target condition.
  observeEvent(input$targetConditionId, {
    # Check if the new value differs from the last updated value.
    if (identical(input$targetConditionId, lastTargetCondition())) {
      return()
    }
    
    # Save the new value
    lastTargetCondition(input$targetConditionId)
    
    # Now update the input’s choices
    updateConditionChoices(session, "targetConditionId", input$targetConditionId)
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  
  #output$startConditionText <- renderText(paste(input$startConditionId, getConditionName(cdm, input$startConditionId)))
  
  #output$targetConditionText <- renderText(paste(input$targetConditionId, getConditionName(cdm, input$targetConditionId)))
  
  
  ############### TRAJECTORY DATA
  
  
  # Reactive expression to fetch data when the button is clicked
  trajectoriesData <- eventReactive(input$getData, {
    req(input$startConditionId > 0)
    req(input$targetConditionId > 0)
    
    df <- getTrajectoriesForCondition(cdm, input$startConditionId)
    return(df)
  })
  
  
  timeframes = c("2 weeks" = 14,
                 "1 month" = 30,
                 "6 months" = 6 * 30,
                 "1 year" = 365,
                 "3 years" = 3 * 365,
                 "5 years" = 5 * 365)
  
  startToTargetConditionDF <- eventReactive(trajectoriesData(), {
    req(trajectoriesData())
    
    df <- createStartToTargetConditionDF(trajectoriesData(), input$startConditionId, input$targetConditionId, timeframes)
    return(df)
  })
  
  resultPercentages <- reactive({
    calculateStartToTargetPercentages(startToTargetConditionDF())
  })
  
  # Percentage data
  output$percentageOutputs <- renderUI({
    req(resultPercentages())
    percentages <- resultPercentages()
    
    # Dynamically create UI elements for each percentage
    percentage_cols <- names(percentages)[grepl("^target_condition_in_", names(percentages))]
    
    # Generate labels based on column names
    time_labels <- sub("^target_condition_in_", "in ", percentage_cols)
    time_labels <- paste0(time_labels, ":")
    
    # Create output elements for each percentage
    percentage_outputs <- lapply(seq_along(percentage_cols), function(i) {
      column(width = 2,
             h4(time_labels[i]),
             p(paste0(sprintf("%.1f", percentages[[percentage_cols[i]]]), "%")) # Format to one decimal place
      )
    })
    
    # Wrap in a fluidRow for horizontal layout
    fluidRow(percentage_outputs)
  })
  
  
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
    
    ggplot(patientData, aes(x = condition_start_date, y = concept_name)) +
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
