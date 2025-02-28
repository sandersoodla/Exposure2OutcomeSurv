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
library(networkD3)
library(survival)
library(survminer)
library(dplyr)

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
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Type to search conditions")
      ),
      textOutput("startConditionText"),
      hr(),
      selectizeInput(
        "targetConditionId",
        "Target Condition concept",
        choices = NULL,
        selected = character(0),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Type to search conditions")
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
        tabPanel("Start to Target KM", uiOutput("kmPlotsUI")),
        tabPanel("Conditions Sankey",
                 sankeyNetworkOutput("sankeyPlotCondition"),
                 div(style = "min-height: 300px",
                     selectizeInput(
                       "hiddenConditions",
                       "Conditions to hide in Sankey Plot:",
                       choices = NULL,
                       multiple = TRUE,
                       selected = NULL,
                       options = list(
                         plugins = list("remove_button"),
                         placeholder = "Select conditions to hide")
                     )
                 )
        ),
        tabPanel("Procedures Sankey",
                 sankeyNetworkOutput("sankeyPlotProcedure"),
                 div(style = "min-height: 300px",
                     selectizeInput(
                       "hiddenProcedures",
                       "Procedures to hide in Sankey Plot:",
                       choices = NULL,
                       multiple = TRUE,
                       selected = NULL,
                       options = list(
                         plugins = list("remove_button"),
                         placeholder = "Select procedures to hide")
                     )
                 )
        ),
        tabPanel("Demographic overview", 
                 fluidRow(
                   column(6, plotOutput("patientPyramid1")),
                   column(6, plotOutput("patientPyramid2"))
                 ),
        ),
        tabPanel("Patient Timeline", plotOutput("patientTimeline")),
        tabPanel("Trajectories containing start condition", DTOutput("trajectoryTable")),
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
source("scripts/getProcedures.R")



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
  
  
  
  ### OUTPUTS ##############
  
  
  # METADATA
  metadata <- getMetadata(cdm)
  
  output$dbName <- renderText(metadata$dbName)
  output$personCount <- renderText(paste("n = ", metadata$personCount))
  
  
  ######### CONDITION INPUT
  
  # Fetch all condition concepts for initial choices using getAllConditions function
  allConditionConcepts <- getAllConditions(cdm)
  
  # Update choices for selectizeInput
  updateConditionChoices <- function(session, inputId, conceptId = character(0)) {
    print(paste("Updating choices for:", inputId, "with conceptId:", conceptId))
    if (length(conceptId) == 0 | all(is.na(conceptId))) {
      
      choiceList <- setNames(allConditionConcepts$concept_id, allConditionConcepts$concept_name_id)
      updateSelectizeInput(session, inputId, choices = choiceList, server = TRUE,
                           options = list(
                             placeholder = "Type to search conditions",
                             maxOptions = 10,
                             searchField = "label"
                           ))
      
    } else {
      # Fetch related concepts for dynamic update, currently does the same as the other branch
      choiceList <- setNames(allConditionConcepts$concept_id, allConditionConcepts$concept_name_id)
      #choices <- getRelatedConcepts(cdm, conceptId)
      #choiceList <- setNames(choices$concept_id, choices$concept_name_id)
      updateSelectizeInput(session, inputId, choices = choiceList, server = TRUE,
                           options = list(
                             placeholder = "Type to search related conditions",
                             maxOptions = 10,
                             searchField = "label"
                           ))
    }
  }
  
  
  ## Initialize start condition choices
  updateConditionChoices(session, "startConditionId")
  
  ## Initialize target condition choices
  updateConditionChoices(session, "targetConditionId")
  
  
  
  #output$startConditionText <- renderText(paste(input$startConditionId, getConditionName(cdm, input$startConditionId)))
  
  #output$targetConditionText <- renderText(paste(input$targetConditionId, getConditionName(cdm, input$targetConditionId)))
  
  
  ############### TRAJECTORY DATA
  
  
  # Reactive expression to fetch data when the button is clicked
  trajectoriesData <- eventReactive(input$getData, {
    req(length(input$startConditionId) > 0)
    req(length(input$targetConditionId) > 0)
    
    df <- getTrajectoriesForCondition(cdm, input$startConditionId)
    return(df)
  })
  
  # For KM analysis events for both start and target conditions are needed
  kmTrajectoriesData <- eventReactive(input$getData, {
    req(length(input$startConditionId) > 0)
    req(length(input$targetConditionId) > 0)
    unionIds <- union(input$startConditionId, input$targetConditionId)
    df <- getTrajectoriesForCondition(cdm, unionIds)
    return(df)
  })
  
  # Trajectories filtered to observe from start condition onwards
  trajectoriesDataFromStartCondition <- eventReactive(trajectoriesData(), {
    req(trajectoriesData())
    
    trajectories <- trajectoriesData()
    
    # Compute the first date the start condition appears for each person
    firstOccurrence <- trajectories %>%
      filter(concept_id %in% input$startConditionId) %>%
      group_by(person_id) %>%
      summarise(first_date = min(condition_start_date), .groups = "drop")
    
    # Only keep trajectories from that first date onward
    filteredTrajectories <- trajectories %>%
      inner_join(firstOccurrence, by = "person_id") %>%
      filter(condition_start_date >= first_date)
    
    return(filteredTrajectories)
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
  
  resultAbsolute <- reactive({
    req(startToTargetConditionDF())
    
    df <- startToTargetConditionDF()
    # Calculate the counts of TRUE values for each target_condition_in_* column
    absoluteCounts <- sapply(df %>% select(starts_with("target_condition_in_")), function(x) sum(x))
    # Calculate the total number of rows (start condition occurrences)
    totalCounts <- sapply(df %>% select(starts_with("target_condition_in_")), function(x) length(x))
    
    #Combine the counts of TRUE and ALL
    resultDf <- data.frame(
      true_counts = absoluteCounts,
      total = totalCounts
    )
    
    return(resultDf)
  })
  
  # Percentage data
  output$percentageOutputs <- renderUI({
    req(resultPercentages())
    req(resultAbsolute())
    
    percentages <- resultPercentages()
    absoluteCountsDf <- resultAbsolute()
    
    # Dynamically create UI elements for each percentage
    percentageCols <- names(percentages)[grepl("^target_condition_in_", names(percentages))]
    
    # Generate labels based on column names
    timeLabels <- sub("^target_condition_in_", "in ", percentageCols)
    timeLabels <- paste0(timeLabels, ":")
    
    # Create output elements for each percentage and absolute count
    outputElements <- lapply(seq_along(percentageCols), function(i) {
      colName <- percentageCols[i]
      trueCount <- absoluteCountsDf[colName, "true_counts"]
      totalCount <- absoluteCountsDf[colName, "total"]
      
      column(width = 2,
             h4(timeLabels[i]),
             p(paste0(sprintf("%.1f", percentages[[colName]]), "% (", trueCount, "/", totalCount, ")"))
      )
    })
    
    
    # Wrap in a fluidRow for horizontal layout
    fluidRow(outputElements)
  })
  
  
  # Render the trajectories table
  output$trajectoryTable <- renderDT({
    req(trajectoriesData())
    datatable(
      trajectoriesData(),
      options = list(pageLength = 10),
      filter = 'top'
    )
  }, server = TRUE)
  
  # Render the start to target condition table
  output$startToTargetConditionTable <- renderDT({
    req(startToTargetConditionDF())
    datatable(
      startToTargetConditionDF(),
      options = list(pageLength = 10),
      filter = 'top'
    )
  }, server = TRUE)
  
  
  # Reactive value to store hidden conditions
  hiddenConditionList <- reactiveVal(NULL)
  
  # Update the patient selector and hidden condition options when trajectories Data changes
  observeEvent(trajectoriesData(), {
    currentHidden <- hiddenConditionList()
    availableConditions <- unique(trajectoriesData()$concept_name)
    validHidden <- intersect(currentHidden, availableConditions) # Keep only valid hidden conditions
    
    updateSelectizeInput(
      session,
      "selectedPatient",
      choices = unique(trajectoriesData()$person_id),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "hiddenConditions",
      choices = availableConditions,
      selected = validHidden,
      server = TRUE
    )
    # Update reactive value
    hiddenConditionList(validHidden)
  })
  
  
  # Observer to update hiddenConditionList when input$hiddenConditions changes
  observeEvent(input$hiddenConditions, {
    hiddenConditionList(input$hiddenConditions)
  }, ignoreNULL = FALSE)
  
  
  
  
  #################### KM PLOTS FOR DEFINED TARGET OUTCOMES ####################
  
  # Build survival data for each target condition.
  kmSurvivalData <- eventReactive(input$getData, {
    req(kmTrajectoriesData())
    req(length(input$startConditionId) > 0, length(input$targetConditionId) > 0)
    traj <- kmTrajectoriesData()
    
    survivalList <- lapply(input$targetConditionId, function(target) {
      # Start events: those with a start condition.
      trajStart <- traj %>% filter(concept_id %in% input$startConditionId)
      # Target events: events matching the target condition.
      trajTarget <- traj %>% filter(concept_id == target)
      survivalDf <- trajStart %>%
        rowwise() %>%
        mutate(
          targetDate = {
            possibleTargets <- trajTarget$condition_start_date[
              trajTarget$person_id == person_id &
                trajTarget$condition_start_date > condition_start_date
            ]
            if (length(possibleTargets) > 0) min(possibleTargets) else as.Date(NA)
          }
        ) %>%
        ungroup()
      censorDate <- max(traj$condition_start_date, na.rm = TRUE)
      survivalDf <- survivalDf %>%
        mutate(
          event = ifelse(!is.na(targetDate), 1, 0),
          finalDate = as.Date(ifelse(event == 1, as.character(targetDate), as.character(censorDate))),
          timeToEvent = as.numeric(finalDate - condition_start_date)
        )
      survivalDf
    })
    
    # Name the list elements using target condition names.
    targetNames <- sapply(input$targetConditionId, function(id) {
      nm <- allConditionConcepts$concept_name_id[allConditionConcepts$concept_id == id]
      if (length(nm) == 0) id else nm
    })
    names(survivalList) <- targetNames
    survivalList
  })
  
  # Create KM plots for each target outcome.
  kmPlots <- reactive({
    req(kmSurvivalData())
    survivalList <- kmSurvivalData()
    plots <- lapply(names(survivalList), function(targetName) {
      survData <- survivalList[[targetName]]
      kmFit <- survfit(Surv(timeToEvent, event) ~ concept_name, data = survData)
      p <- ggsurvplot(
        kmFit,
        data = survData,
        conf.int = TRUE,
        pval = TRUE,
        risk.table = TRUE,
        legend.title = "Start Condition",
        title = paste("KM Curves for Outcome:", targetName),
        xlab = "Days from Start Condition",
        ylab = "Outcome-free probability"
      )$plot
      p
    })
    plots
  })
  
  # Render the KM plots in a grid layout (two columns per row).
  output$kmPlotsUI <- renderUI({
    req(kmPlots())
    plots <- kmPlots()
    numPlots <- length(plots)
    colsPerRow <- 2
    numRows <- ceiling(numPlots / colsPerRow)
    plotTags <- list()
    index <- 1
    for (r in 1:numRows) {
      rowPlots <- list()
      for (c in 1:colsPerRow) {
        if (index <= numPlots) {
          rowPlots[[c]] <- column(width = 6, plotOutput(outputId = paste0("kmPlot_", index)))
          index <- index + 1
        }
      }
      plotTags[[r]] <- fluidRow(rowPlots)
    }
    do.call(tagList, plotTags)
  })
  
  # Render each KM plot output.
  observe({
    req(kmPlots())
    for (i in seq_along(kmPlots())) {
      local({
        ii <- i
        outputId <- paste0("kmPlot_", ii)
        output[[outputId]] <- renderPlot({
          kmPlots()[[ii]]
        })
      })
    }
  })
  
  
  
  
  ################# PROCEDURE DATA ###################
  
  # Reactive expression to fetch procedure data
  proceduresData <- eventReactive(input$getData, {
    req(length(input$startConditionId) > 0)
    
    df_procedure <- getProceduresAfterStartCondition(cdm, input$startConditionId)
    return(df_procedure)
  })
  
  # Reactive value to store hidden procedures
  hiddenProcedureList <- reactiveVal(NULL)
  
  # Update hidden procedure options when procedure data changes
  observeEvent(proceduresData(), {
    currentHiddenProcedures <- hiddenProcedureList()
    availableProcedures <- unique(proceduresData()$concept_name)
    validHiddenProcedures <- intersect(currentHiddenProcedures, availableProcedures) # Keep only valid hidden procedures
    
    updateSelectizeInput(
      session,
      "hiddenProcedures",
      choices = availableProcedures,
      selected = validHiddenProcedures,
      server = TRUE
    )
    # Update reactive value
    hiddenProcedureList(validHiddenProcedures)
  })
  
  
  # Observer to update hiddenProcedureList when input$hiddenProcedures changes
  observeEvent(input$hiddenProcedures, {
    hiddenProcedureList(input$hiddenProcedures)
  }, ignoreNULL = FALSE)
  
  
  
  ################# SANKEY PLOT CONDITION #
  
  createSankeyLinks <- function(trajectoryDf, hiddenItems = NULL) {
    linksList <- list()
    personIds <- unique(trajectoryDf$person_id)
    
    for (personId in personIds) {
      personTrajectory <- trajectoryDf %>%
        filter(person_id == !!personId)
      
      if (nrow(personTrajectory) > 1) {
        for (i in 1:(nrow(personTrajectory) - 1)) {
          sourceEvent <- personTrajectory$concept_name[i]
          targetEvent <- personTrajectory$concept_name[i + 1]
          
          # Skip if either source or target event is in hiddenItems
          if (!(sourceEvent %in% hiddenItems) && !(targetEvent %in% hiddenItems)) {
            linksList[[length(linksList) + 1]] <- data.frame(
              source = sourceEvent,
              target = targetEvent
            )
          }
        }
      }
    }
    linksDf <- bind_rows(linksList)
    
    # Aggregate links to count transitions
    aggregatedLinks <- linksDf %>%
      group_by(source, target) %>%
      summarise(value = n(), .groups = 'drop') %>%
      ungroup()
    
    return(aggregatedLinks)
  }
  
  
  output$sankeyPlotCondition <- renderSankeyNetwork({
    req(trajectoriesDataFromStartCondition())
    
    trajectories <- trajectoriesDataFromStartCondition()
    
    hiddenConditions <- hiddenConditionList() # Get selected hidden conditions
    
    # Create Sankey Links, passing hidden conditions
    sankeyLinksData <- createSankeyLinks(trajectories, hiddenConditions)
    
    # Create Nodes data frame
    sankeyNodesData <- data.frame(
      name = unique(c(sankeyLinksData$source, sankeyLinksData$target))
    )
    
    # Prepare Links for networkD3
    linksForNetworkD3 <- sankeyLinksData %>%
      left_join(sankeyNodesData %>% mutate(Id = 0:(n()-1)), by = c("source" = "name")) %>%
      rename(sourceId = Id) %>%
      left_join(sankeyNodesData %>% mutate(Id = 0:(n()-1)), by = c("target" = "name")) %>%
      rename(targetId = Id) %>%
      select(source = sourceId, target = targetId, value, sourceName = source, targetName = target)
    
    # Get both links and nodes
    data <- list(links = linksForNetworkD3, nodes = sankeyNodesData)
    
    # Generate sankey plot
    sankeyNetwork(
      Links = data$links,
      Nodes = data$nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      units = "occurrences",
      fontSize = 12,
      nodeWidth = 30,
      nodePadding = 15,
      sinksRight = FALSE,
      width = "100%",
      height = "1000px"
    )
  })
  
  
  ################ SANKEY PLOT PROCEDURE #
  
  
  output$sankeyPlotProcedure <- renderSankeyNetwork({
    req(proceduresData())
    
    procedures <- proceduresData()
    
    hiddenProcedures <- hiddenProcedureList() # Get selected hidden procedures
    
    # Create Sankey Links, passing hidden procedures
    sankeyLinksDataProcedure <- createSankeyLinks(procedures, hiddenProcedures)
    
    # Create Nodes data frame
    sankeyNodesDataProcedure <- data.frame(
      name = unique(c(sankeyLinksDataProcedure$source, sankeyLinksDataProcedure$target))
    )
    
    # Prepare Links for networkD3
    linksForNetworkD3Procedure <- sankeyLinksDataProcedure %>%
      left_join(sankeyNodesDataProcedure %>% mutate(Id = 0:(n()-1)), by = c("source" = "name")) %>%
      rename(sourceId = Id) %>%
      left_join(sankeyNodesDataProcedure %>% mutate(Id = 0:(n()-1)), by = c("target" = "name")) %>%
      rename(targetId = Id) %>%
      select(source = sourceId, target = targetId, value, sourceName = source, targetName = target)
    
    # Get both links and nodes
    dataProcedure <- list(links = linksForNetworkD3Procedure, nodes = sankeyNodesDataProcedure)
    
    # Generate sankey plot
    sankeyNetwork(
      Links = dataProcedure$links,
      Nodes = dataProcedure$nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      units = "occurrences",
      fontSize = 12,
      nodeWidth = 30,
      nodePadding = 15,
      sinksRight = FALSE,
      width = "100%",
      height = "1000px"
    )
  })
  
  
  
  ########### PATIENT TIMELINE AND PYRAMIDS
  
  
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
