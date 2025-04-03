source("scripts/getConditionInfo.R")
source("scripts/conditionToCondition.R")
source("scripts/demographicAnalysis.R")
source("scripts/getMetadata.R")
source("scripts/conceptRelations.R")
source("scripts/getProcedures.R")
source("scripts/getAllTrajectories.R")

library(shiny)
library(DT)
library(ggplot2)
library(networkD3)
library(survival)
library(survminer)
library(dplyr)
library(MatchIt)

# Define server logic 
server <- function(input, output, session) {
  
  #################### CONNECTION SETUP ###########
  
  DBMS <- Sys.getenv("DBMS")
  DATABASE <- paste(Sys.getenv("DB_HOST"),"/",Sys.getenv("DB_NAME"),sep='')
  DB_USER <- Sys.getenv('DB_USERNAME')
  DB_PASSWORD <- Sys.getenv('DB_PASSWORD')
  DB_PORT <- Sys.getenv('DB_PORT')
  
  CDM_SCHEMA <- Sys.getenv("CDM_SCHEMA")
  WRITE_SCHEMA <- Sys.getenv("WRITE_SCHEMA")
  
  
  if (DBMS == "postgresql") { # TODO: enable all DBMS that are supported by DBConnector
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = DBMS,
      server = DATABASE,
      user = DB_USER,
      password = DB_PASSWORD,
      port = DB_PORT
    )
    connection <- DatabaseConnector::connect(connectionDetails)
    cdm <- CDMConnector::cdmFromCon(connection, cdmSchema = CDM_SCHEMA, writeSchema = WRITE_SCHEMA, cdmName = "maitt_cdm")
    
  } else if (DBMS == "duckdb") {
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = "c:/temp/EunomiaData/GiBleed_5.3.duckdb")
    cdm <- CDMConnector::cdmFromCon(connection, cdmSchema = CDM_SCHEMA, writeSchema = WRITE_SCHEMA)
  }
  
  
  
  ### OUTPUTS ##############
  
  
  # METADATA
  metadata <- getMetadata(cdm)
  
  output$dbName <- renderText(metadata$dbName)
  output$personCount <- renderText(paste("n = ", metadata$personCount))
  
  
  ######### CONDITION INPUT
  
  # Fetch all condition concepts that have at least 1 occurrence for initial choices
  allOccurredConditions <- getAllConditionsWithOccurrences(cdm)
  
  # Update choices for selectizeInput
  updateConditionChoices <- function(session, inputId) {
    choiceList <- setNames(allOccurredConditions$concept_id, allOccurredConditions$concept_name_id)
    updateSelectizeInput(session, inputId, choices = choiceList, server = TRUE,
                         options = list(
                           placeholder = "Type to search conditions",
                           searchField = "label"
                         )
    )
  }
  
  
  ## Initialize start condition choices
  updateConditionChoices(session, "startConditionId")
  
  ## Initialize target condition choices
  updateConditionChoices(session, "targetConditionId")
  
  
  ### CONDITION INPUT FROM FILE
  
  # Helper function to extract condition IDs from an uploaded CSV file:
  extractConditionIds <- function(file) {
    req(file)
    
    possibleCols <- c("Id", "condition_source_concept_id", "concept_id", "condition_concept_id")
    
    # Try reading as tab-separated file first (ATLAS and ATHENA files)
    df <- read.delim(file$datapath, stringsAsFactors = FALSE)
    colFound <- intersect(possibleCols, colnames(df))
    
    # If no valid columns found, try reading as comma-separated file.
    if (length(colFound) == 0) {
      df <- read.csv(file$datapath, stringsAsFactors = FALSE)
      colFound <- intersect(possibleCols, colnames(df))
    }
    
    if (length(colFound) > 0) {
      return(df[[colFound[1]]])
    } else {
      showNotification("Uploaded file does not contain any valid column name for condition IDs.",
                       type = "error")
      return(NULL)
    }
  }
  
  # When a start condition file is uploaded, update the selected values:
  observeEvent(input$startConditionFile, {
    sourceIds <- extractConditionIds(input$startConditionFile)
    if (!is.null(sourceIds)) {
      # Map source concept ids to standard concept ids
      mapping <- mapInputToStandardIds(cdm, sourceIds)
      
      # Identify unmapped source concept IDs
      unmappedSourceIds <- setdiff(sourceIds, mapping$input_concept_id)
      
      # Check if there are any mappings
      if (nrow(mapping) == 0) {
        # If no mappings, then check which input IDs are already standard and exist in the dataset
        occurredStandardIds <- sourceIds[sourceIds %in% allOccurredConditions$concept_id]
      } else {
        # Identify mapped standard IDs that occur in the dataset
        occurredStandardIds <- mapping$standard_concept_id[mapping$standard_concept_id %in% allOccurredConditions$concept_id]
      }
      
      # Create a detailed mapping table
      mappingDetails <- mapping %>%
        mutate(
          status = as.character(ifelse(standard_concept_id %in% allOccurredConditions$concept_id,
                                       "Mapped", "Mapped, no occurrences"))
        )
      
      # If there are unmapped source IDs, add them as separate rows
      if (length(unmappedSourceIds) > 0) {
        unmappedDf <- data.frame(
          input_concept_id = unmappedSourceIds,
          standard_concept_id = NA,
          status = "Not mapped",
          stringsAsFactors = FALSE
        )
        
        # Update the status to "Standard" if the unmapped ID is in allOccurredConditions
        unmappedDf$status[unmappedDf$input_concept_id %in% allOccurredConditions$concept_id] <- "Standard"
        
        mappingDetails <- dplyr::bind_rows(mappingDetails, unmappedDf)
      }
      
      # Show a modal dialog with the mapping details
      showModal(modalDialog(
        title = "Input conditions mapped to standard concepts",
        "Standard - input condition ID is already a standard concept ID; 
        Not mapped - ID is non-standard and couldn't be mapped with concept_relationship;
        Mapped, no occurrences - ID mapped to standard, no occurrences in the dataset",
        DT::dataTableOutput("mappingTableDetails"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
      
      # Render the mapping details table in the modal
      output$mappingTableDetails <- DT::renderDataTable({
        DT::datatable(mappingDetails, options = list(pageLength = 10))
      })
      
      # Finally, update the selectizeInput with only occurring standard concept IDs
      updateSelectizeInput(
        session,
        "startConditionId",
        choices = setNames(allOccurredConditions$concept_id,
                           allOccurredConditions$concept_name_id),
        selected = occurredStandardIds,
        server = TRUE
      )
    }
  })
  
  # When a target condition file is uploaded, update the selected values:
  observeEvent(input$targetConditionFile, {
    sourceIds <- extractConditionIds(input$targetConditionFile)
    if (!is.null(sourceIds)) {
      # Map source concept ids to standard concept ids
      mapping <- mapInputToStandardIds(cdm, sourceIds)
      
      # Identify unmapped source concept IDs
      unmappedSourceIds <- setdiff(sourceIds, mapping$input_concept_id)
      
      # Check if there are any mappings
      if (nrow(mapping) == 0) {
        # If no mappings, then check which input IDs are already standard and exist in the dataset
        occurredStandardIds <- sourceIds[sourceIds %in% allOccurredConditions$concept_id]
      } else {
        # Identify mapped standard IDs that occur in the dataset
        occurredStandardIds <- mapping$standard_concept_id[mapping$standard_concept_id %in% allOccurredConditions$concept_id]
      }
      
      # Create a detailed mapping table
      mappingDetails <- mapping %>%
        mutate(
          status = as.character(ifelse(standard_concept_id %in% allOccurredConditions$concept_id,
                                       "Mapped", "Mapped, no occurrences"))
        )
      
      # If there are unmapped source IDs, add them as separate rows
      if (length(unmappedSourceIds) > 0) {
        unmappedDf <- data.frame(
          input_concept_id = unmappedSourceIds,
          standard_concept_id = NA,
          status = "Not mapped",
          stringsAsFactors = FALSE
        )
        
        # Update the status to "Standard" if the unmapped ID is in allOccurredConditions
        unmappedDf$status[unmappedDf$input_concept_id %in% allOccurredConditions$concept_id] <- "Standard"
        
        mappingDetails <- dplyr::bind_rows(mappingDetails, unmappedDf)
      }
      
      # Show a modal dialog with the mapping details
      showModal(modalDialog(
        title = "Input conditions mapped to standard concepts",
        "Standard - input condition ID is already a standard concept ID; 
        Not mapped - ID is non-standard and couldn't be mapped with concept_relationship;
        Mapped, no occurrences - ID mapped to standard, no occurrences in the dataset",
        DT::dataTableOutput("mappingTableDetails"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
      
      # Render the mapping details table in the modal
      output$mappingTableDetails <- DT::renderDataTable({
        DT::datatable(mappingDetails, options = list(pageLength = 10))
      })
      
      # Finally, update the selectizeInput with only occurring standard concept IDs
      updateSelectizeInput(
        session,
        "targetConditionId",
        choices = setNames(allOccurredConditions$concept_id,
                           allOccurredConditions$concept_name_id),
        selected = occurredStandardIds,
        server = TRUE
      )
    }
  })
  
  
  
  ############### TRAJECTORY DATA
  
  
  # Reactive expression to fetch data when the button is clicked
  trajectoriesData <- eventReactive(input$getData, {
    req(length(input$startConditionId) > 0)
    req(length(input$targetConditionId) > 0)
    
    df <- getTrajectoriesForCondition(cdm, input$startConditionId)
    return(df)
  })
  
  # All trajectories, for KM analysis
  allTrajectoriesData <- eventReactive(input$getData, {
    allTrajectories <- getAllTrajectories(cdm)
    return(allTrajectories)
  })
  
  
  # CAN BE REMOVED IF I REMOVE THE PROCEDURES SANKEY PLOT
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
  
  # Demographic data used for matching a control group
  demographics <- getAllGendersAndBirthyears(cdm)
  
  # For every start and target condition pair, build a matched survival dataset and KM plot
  kmSurvivalDataPairs <- eventReactive(input$getData, {
    req(allTrajectoriesData())
    
    traj <- allTrajectoriesData()
    censorDate <- max(traj$condition_start_date, na.rm = TRUE)
    
    # Get population with demographics, filter out those without necessary data upfront
    population <- traj %>%
      distinct(person_id, .keep_all = TRUE) %>%
      inner_join(demographics, by = "person_id") %>%
      filter(!is.na(gender_concept_id) & !is.na(year_of_birth))
    
    # Pre-calculate target dates for efficiency
    allTargetDates <- traj %>%
      filter(concept_id %in% input$targetConditionId) %>%
      group_by(person_id, concept_id) %>%
      summarise(target_date = min(condition_start_date), .groups = "drop")
    
    
    # Iterate through each Start Condition for Matching
    lapply(input$startConditionId, function(startId) {
      
      # 1. Identify first occurrence date for the current start condition
      startCondDates <- traj %>%
        filter(concept_id == startId) %>%
        group_by(person_id) %>%
        summarise(start_cond_date = min(condition_start_date), .groups = "drop")
      
      # 2. Prepare the base dataset for matching for *this* start condition
      matchDataBase <- population %>%
        left_join(startCondDates, by = "person_id") %>%
        mutate(
          # Indicator: 1 if they have the start condition (case), 0 otherwise (potential control)
          has_start_condition = if_else(!is.na(start_cond_date), 1, 0),
          # Store the potential index date (start date for cases, NA for controls)
          # This is only used temporarily before alignment
          index_date_potential = start_cond_date,
          # Convert gender concept to categorical factor for matchit
          gender_concept_id = factor(gender_concept_id)
        )
      
      # 3. Check if there are enough cases and potential controls
      n_cases <- sum(matchDataBase$has_start_condition == 1)
      n_controls_potential <- sum(matchDataBase$has_start_condition == 0)
      
      if (n_cases == 0) {
        showNotification(paste("No patients found with the start condition ID:", startId, " and required demographic data. Skipping."), type = "warning")
        return(NULL)
      }
      # Ensure the cases actually have a start date needed for index date alignment
      if (nrow(matchDataBase %>% filter(has_start_condition == 1, is.na(index_date_potential))) > 0) {
        showNotification(paste("Error: Some cases with start condition ID:", startId, " have a missing start_cond_date. Cannot proceed."), type = "error")
        return(NULL)
      }
      if (n_controls_potential == 0) {
        showNotification(paste("No potential controls found for start condition ID:", startId, " with required demographic data. Skipping."), type = "warning")
        return(NULL)
      }
      
      
      # 4. Perform Matching
      matchResult <- tryCatch({
        matchit(
          has_start_condition ~ year_of_birth + gender_concept_id, # Formula for matching
          data = matchDataBase,
          method = "nearest",
          ratio = 2 # 1:2 matching
        )
      }, error = function(e) {
        showNotification(paste("Matching failed for start condition ID:", startId, "-", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(matchResult)) return(NULL) # Exit if matching failed
      
      # 5. Extract Matched Data
      matchedData <- match.data(matchResult)
      
      if (nrow(matchedData) == 0) {
        showNotification(paste("Matching yielded no pairs for start condition ID:", startId, ". Skipping."), type = "warning")
        return(NULL)
      }
      
      # 6. Align Index Dates: Assign Case's index date to their matched Control(s)
      matchedDataWithAlignedIndex <- matchedData %>%
        group_by(subclass) %>% # Group by matched set ID
        mutate(
          # Get the case's potential index date (their start_cond_date)
          # Assign it to the 'index_date' column for everyone in this subclass
          index_date = index_date_potential[has_start_condition == 1][1]
        ) %>%
        ungroup() %>%
        filter(!is.na(index_date)) # Ensure alignment worked and produced a valid date
      
      if (nrow(matchedDataWithAlignedIndex) < nrow(matchedData)) {
        warning(paste("Some matched pairs removed for Start ID:", startId, "due to missing index date after alignment (problem with case start_cond_date?)."))
      }
      if (nrow(matchedDataWithAlignedIndex) == 0) {
        showNotification(paste("No matched pairs remained after index date alignment for Start ID:", startId, ". Skipping."), type = "warning")
        return(NULL)
      }
      
      
      # --- Iterate through each Target Condition for Survival Analysis on Matched Data ---
      lapply(input$targetConditionId, function(targetId) {
        
        # 7. Get target dates for the *current* target condition
        currentTargetDates <- allTargetDates %>%
          filter(concept_id == targetId) %>%
          select(person_id, target_date)
        
        # 8. Calculate Survival Variables using aligned index dates
        survData <- matchedDataWithAlignedIndex %>%
          left_join(currentTargetDates, by = "person_id") %>%
          mutate(
            # Event: Target date exists AND is after the ALIGNED index date
            event = if_else(!is.na(target_date) & target_date > index_date, 1, 0),
            # Final Date: Event date if event occurred, otherwise censor date
            final_date = if_else(event == 1, target_date, censorDate),
            # Time-to-event: Difference from ALIGNED index date to final date
            time_to_event = pmax(0, as.numeric(difftime(final_date, index_date, units = "days"))),
            # Group Factor for plotting, based on the start condition status
            group = factor(has_start_condition,
                           levels = c(0, 1),
                           labels = c("Control (Matched)", "Case (Start Cond.)"))
          ) %>%
          # Select columns needed for survfit and potentially diagnostics
          select(person_id, time_to_event, event, group, subclass, index_date)
        
        # 9. Final checks on calculated survival data
        if(any(is.na(survData$time_to_event)) | any(survData$time_to_event < 0)){
          warning(paste("Issue with time_to_event calculation for Start ID:", startId, "Target ID:", targetId))
          survData <- survData %>% filter(!is.na(time_to_event) & time_to_event >= 0)
        }
        
        if(nrow(survData) < 2 || n_distinct(survData$group) < 2 || sum(survData$event, na.rm = TRUE) == 0) {
          warning(paste("Insufficient data after matching/alignment for KM plot. Start:", startId, "Target:", targetId))
          return(NULL) # Not enough data/groups/events to plot
        }
        
        return(survData) # Return the final survival data for this pair
      })
    })
  })
  
  # Generate KM plots for each start-target pair:
  kmPlotsPairs <- eventReactive(kmSurvivalDataPairs(), {
    req(kmSurvivalDataPairs())
    # Create an empty list to store plots. Use names that indicate the pair.
    plotsList <- list()
    
    for (i in seq_along(input$startConditionId)) {
      for (j in seq_along(input$targetConditionId)) {
        # Retrieve the survival data for this pair.
        survData <- kmSurvivalDataPairs()[[i]][[j]]
        
        # Fit a KM model using the group variable (With vs. Without start condition)
        kmFit <- survfit(Surv(time_to_event, event) ~ group, data = survData)
        
        # Get labels for the plot (customize as needed)
        startLabel <- allOccurredConditions$concept_name_id[allOccurredConditions$concept_id == input$startConditionId[i]]
        targetLabel <- allOccurredConditions$concept_name_id[allOccurredConditions$concept_id == input$targetConditionId[j]]
        pairLabel <- paste0("Start: ", startLabel, " | Target: ", targetLabel)
        
        p <- ggsurvplot(
          kmFit,
          data = survData,
          conf.int = TRUE,
          pval = TRUE,
          risk.table = TRUE,
          legend.title = "Group",
          title = pairLabel,
          xlab = "Days from index date",
          ylab = "Target-free probability"
        )$plot
        
        plotsList[[pairLabel]] <- p
      }
    }

    plotsList
  })
  
  # Render the KM plots in a grid layout (adjust columns as needed)
  output$kmPlotsUI <- renderUI({
    req(kmPlotsPairs())
    plots <- kmPlotsPairs()
    numPlots <- length(plots)
    colsPerRow <- 2
    numRows <- ceiling(numPlots / colsPerRow)
    plotTags <- list()
    index <- 1
    for (r in 1:numRows) {
      rowPlots <- list()
      for (c in 1:colsPerRow) {
        if (index <= numPlots) {
          rowPlots[[c]] <- column(width = 6, plotOutput(outputId = paste0("kmPairPlot_", index)))
          index <- index + 1
        }
      }
      plotTags[[r]] <- fluidRow(rowPlots)
    }
    do.call(tagList, plotTags)
  })
  
  # Render each individual KM plot output.
  observe({
    req(kmPlotsPairs())
    numPlots <- length(kmPlotsPairs())
    for (i in seq_len(numPlots)) {
      local({
        ii <- i
        outputId <- paste0("kmPairPlot_", ii)
        output[[outputId]] <- renderPlot({
          kmPlotsPairs()[[ii]]
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
  
  
  # Module UI plotOutput
  populationPyramidUI <- function(id) {
    ns <- NS(id)
    plotOutput(ns("pyramidPlot"))
  }
  
  # Module Server: renders the population pyramid for the given condition
  populationPyramidServer <- function(id, conditionId) {
    moduleServer(id, function(input, output, session) {
      output$pyramidPlot <- renderPlot({
        # conditionId is expected to be a reactive value
        createPopulationPyramidForCondition(cdm, conditionId())
      })
    })
  }
  
  
  # Dynamic UI for start condition pyramids
  output$startPyramidsUI <- renderUI({
    req(input$startConditionId)
    tagList(
      lapply(seq_along(input$startConditionId), function(i) {
        # Each pyramid gets its own module instance
        populationPyramidUI(paste0("startPyramid_", i))
      })
    )
  })
  
  # Dynamic UI for target condition pyramids
  output$targetPyramidsUI <- renderUI({
    req(input$targetConditionId)
    tagList(
      lapply(seq_along(input$targetConditionId), function(i) {
        populationPyramidUI(paste0("targetPyramid_", i))
      })
    )
  })
  
  # Call the module for each start condition
  observe({
    req(input$startConditionId)
    for (i in seq_along(input$startConditionId)) {
      local({
        idx <- i  # Capture the loop variable
        # Wrap the condition in a reactive expression
        condition <- reactive({ input$startConditionId[idx] })
        populationPyramidServer(paste0("startPyramid_", idx), condition)
      })
    }
  })
  
  # Call the module for each target condition
  observe({
    req(input$targetConditionId)
    for (i in seq_along(input$targetConditionId)) {
      local({
        idx <- i
        condition <- reactive({ input$targetConditionId[idx] })
        populationPyramidServer(paste0("targetPyramid_", idx), condition)
      })
    }
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
  
  
  session$onSessionEnded(function() {
    # disconnect DB
    tryCatch(
      expr = {
        CDMConnector::cdmDisconnect(cdm)
      },
      error = function(e) {
        DatabaseConnector::disconnect(connection)
      }
    )
  })
}