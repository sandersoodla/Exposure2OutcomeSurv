source("scripts/getConditionInfo.R")
source("scripts/conditionToCondition.R")
source("scripts/demographicAnalysis.R")
source("scripts/getMetadata.R")
source("scripts/conceptRelations.R")
source("scripts/getProcedures.R")
source("scripts/getObservationPeriod.R")

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
  
  
  # Helper Function: Performs propensity score matching.
  # Attempts to match cases (start condition) to controls based on covariates.
  performMatching <- function(population, casePersonIds, startId) {
    # 1. Prepare data for matching: add a binary indicator for case status.
    matchDataBase <- population %>%
      mutate(
        is_case = if_else(person_id %in% casePersonIds, 1, 0)
      )
    
    # 2. Execute matching using matchit within a tryCatch for error handling.
    matchResult <- tryCatch({
      matchit(
        is_case ~ year_of_birth + gender_concept_id, # Matching formula
        data = matchDataBase,                               # Input data
        method = "nearest",                                  # Matching algorithm
        ratio = 2                                            # Target control:case ratio
      )
    }, error = function(e) {
      # Notify user and log warning if matching algorithm fails.
      showNotification(paste("Matching failed for Start ID:", startId), type="warning", duration=5)
      warning(paste("Matching failed for Start ID:", startId, "-", e$message))
      # Return NULL to indicate matching failure.
      return(NULL)
    })
    
    # 3. Extract the matched dataset. Assumes matchResult is valid.
    return(match.data(matchResult))
  }
  
  # Helper Function: Fetches dates and aligns index dates for the matched cohort.
  # Retrieves start dates for cases, filters pairs, aligns index dates, gets censor date.
  fetchAndAlignDates <- function(matchedData, cdm, startId) {
    # 1. Get IDs of all matched individuals and just the cases.
    # Assumes matchedData is a valid data frame from performMatching.
    matchedPersonIds <- unique(matchedData$person_id)
    casePersonIds <- matchedData %>% filter(is_case == 1) %>% pull(person_id) %>% unique()
    
    # 2. Fetch start condition dates for cases using tryCatch.
    startCondDates <- tryCatch({
      getFirstConditionDatesForPersons(cdm, personIds = casePersonIds, conceptIds = startId) %>%
        rename(start_cond_date = condition_start_date) %>%
        select(person_id, start_cond_date)
    }, error = function(e) {
      # Notify user if fetching start dates fails.
      showNotification(paste("Error fetching start dates for Start ID:", startId), type="error", duration=5)
      # Return NULL to indicate failure.
      return(NULL)
    })
    
    # --- Filtering and Alignment ---
    # Steps below assume startCondDates is a valid data frame.
    
    # 3. Identify cases for whom a start date was found.
    casesWithDates <- startCondDates %>% distinct(person_id)
    # 4. Find the matched sets (subclasses) where the case has a start date.
    subclassesToKeep <- matchedData %>%
      filter(is_case == 1) %>%
      semi_join(casesWithDates, by = "person_id") %>%
      distinct(subclass)
    
    # 5. Filter the matched data to only include these complete subclasses.
    matchedDataFiltered <- matchedData %>%
      semi_join(subclassesToKeep, by = "subclass")
    
    # 6. Update the list of person IDs after filtering.
    matchedPersonIds <- unique(matchedDataFiltered$person_id)
    
    # 7. Join the start dates back to the filtered matched data.
    matchedDataWithStart <- matchedDataFiltered %>%
      left_join(startCondDates, by = "person_id")
    
    # 8. Align index dates: Set index_date for controls to their matched case's start_cond_date.
    matchedDataAligned <- matchedDataWithStart %>%
      group_by(subclass) %>% # Process by matched set
      mutate(
        case_start_date = first(na.omit(start_cond_date[is_case == 1])), # Get case's start date
        index_date = case_start_date                                            # Apply to all in set
      ) %>%
      ungroup() %>%
      filter(!is.na(index_date)) # Remove rows where index date is NA (essential)
    
    # --- Determine Censor Date ---
    # 9. Fetch the latest observation end date for the aligned cohort using tryCatch.
    censorDate <- tryCatch({
      getMaxObservationEndDate(cdm, matchedPersonIds)
    }, error = function(e) {
      # Notify user and use fallback if fetching fails.
      showNotification(paste("Error fetching censor date for Start ID:", startId, ". Using fallback."), type="warning", duration=5)
      return(Sys.Date())
    })
    # Use fallback if the fetched date is invalid.
    if (is.na(censorDate) || !inherits(censorDate, "Date")) {
      censorDate <- Sys.Date()
    }
    
    # 10. Return the aligned data and the censor date.
    return(list(alignedData = matchedDataAligned, censorDate = censorDate))
  }
  
  
  # Helper Function: Calculate survival time and event status.
  # Computes time_to_event and event indicator for a specific target condition.
  calculateSurvival <- function(alignedData, targetDatesForPerson, targetId, censorDate, indexDateCol = "index_date", groupCol = "is_case") {
    
    # 1. Filter the fetched target dates for the specific targetId of interest.
    currentTargetDates <- targetDatesForPerson %>%
      filter(concept_id == targetId) %>%
      select(person_id, target_date = condition_start_date)
    
    # 2. Join target dates and calculate survival variables.
    # Assumes alignedData is a valid data frame.
    survData <- alignedData %>%
      left_join(currentTargetDates, by = "person_id") %>% # Add target dates
      mutate(
        # Event = 1 if target exists and is after index date
        event = if_else(!is.na(target_date) & target_date > .data[[indexDateCol]], 1, 0),
        # Follow-up ends at event or censoring
        final_date_raw = if_else(event == 1, target_date, censorDate),
        # Ensure follow-up doesn't end before index date
        final_date = pmax(final_date_raw, .data[[indexDateCol]], na.rm = TRUE),
        # Calculate time difference in days (non-negative)
        time_to_event = pmax(0, as.numeric(difftime(final_date, .data[[indexDateCol]], units = "days"))),
        # Create factor for case/control group
        group = factor(.data[[groupCol]],
                       levels = c(0, 1),
                       labels = c("Control (Matched)", "Case (Start Cond.)"))
      ) %>%
      # Select columns needed for survival analysis
      select(person_id, time_to_event, event, group, subclass, index_date, target_date, final_date)
    
    # 3. Return the resulting data frame with survival variables.
    return(survData)
  }
  
  # --- Main Reactive Function to Generate Survival Data ---
  # Orchestrates the entire process from matching to final survival data generation.
  kmSurvivalDataPairs <- eventReactive(input$getData, {
    # 1. Ensure required inputs and objects are available.
    req(input$startConditionId, input$targetConditionId, nrow(demographics) > 0, cdm)
    
    # 2. Prepare the base demographic data for matching.
    populationBase <- demographics %>%
      filter(!is.na(gender_concept_id) & !is.na(year_of_birth)) %>%
      mutate(gender_concept_id = factor(gender_concept_id)) %>%
      select(person_id, year_of_birth, gender_concept_id)
    
    # --- Outer Loop: Process each selected Start Condition ---
    resultsListStart <- lapply(input$startConditionId, function(startId) {
      
      # 3. Fetch persons having the current start condition (with error handling).
      startCasePersons <- tryCatch({
        getPeopleWithCondition(cdm, startId)
      }, error = function(e) {
        showNotification(paste("Error fetching persons for start condition", startId, ":", e$message), type = "error")
        return(NULL) # Skip this startId if fetch fails
      })
      # Code proceeds assuming startCasePersons is valid if tryCatch didn't return NULL.
      
      # 4. Perform matching for the current start condition.
      matchedData <- performMatching(populationBase, startCasePersons$person_id, startId)
      # Code proceeds assuming matchedData is valid if performMatching didn't return NULL.
      
      # 5. Fetch dates and align index dates for the matched cohort.
      dateInfo <- fetchAndAlignDates(matchedData, cdm, startId)
      # Code proceeds assuming dateInfo is valid if fetchAndAlignDates didn't return NULL.
      
      # 6. Extract results from date fetching/alignment (will error if dateInfo is NULL).
      alignedData <- dateInfo$alignedData
      censorDate <- dateInfo$censorDate
      alignedPersonIds <- unique(alignedData$person_id)
      
      # 7. Fetch all relevant target condition dates for the aligned cohort (with error handling).
      allTargetDatesForMatched <- tryCatch({
        getFirstConditionDatesForPersons(cdm, personIds = alignedPersonIds, conceptIds = input$targetConditionId)
      }, error = function(e) {
        showNotification(paste("Error fetching target dates for matched cohort (Start ID:", startId, "):", e$message), type = "error")
        return(NULL) # Skip this startId if fetch fails
      })
      # Code proceeds assuming allTargetDatesForMatched is valid if tryCatch didn't return NULL.
      
      # --- Inner Loop: Process each selected Target Condition for the current Start Condition ---
      resultsListTarget <- lapply(input$targetConditionId, function(targetId) {
        # 8. Calculate survival variables for the current start-target pair.
        survData <- calculateSurvival(
          alignedData = alignedData,
          targetDatesForPerson = allTargetDatesForMatched,
          targetId = targetId,
          censorDate = censorDate,
          indexDateCol = "index_date",
          groupCol = "is_case"
        )
        
        # 9. Add start/target IDs to the survival data if it was created successfully.
        if (!is.null(survData)) {
          survData$start_condition_id <- startId
          survData$target_condition_id <- targetId
        }
        # Return the result (data frame or NULL).
        return(survData)
      }) # End inner lapply
      
      # 10. Attempt to name the inner list elements by target ID.
      if (length(resultsListTarget) > 0) {
        tryCatch({ # Use tryCatch as naming might fail if list contains NULLs
          valid_results <- resultsListTarget[!sapply(resultsListTarget, is.null)]
          if(length(valid_results)>0){
            names(resultsListTarget) <- sapply(valid_results, function(df) unique(df$target_condition_id))
          }
        }, error = function(e) { }) # Ignore naming errors
      }
      
      # 11. Return the list of results for the current start ID.
      return(resultsListTarget)
      
    }) # End outer lapply
    
    # 12. Assign the results to the final list variable.
    # This list may contain NULLs where processing failed for a start ID.
    finalNestedList <- resultsListStart
    
    # 13. Attempt to name the outer list elements by start ID.
    if (length(finalNestedList) > 0) {
      tryCatch({ # Use tryCatch as naming might fail
        # Identify valid list elements for naming
        valid_start_results <- finalNestedList[!sapply(finalNestedList, function(l) !is.list(l) || length(l) == 0 || is.null(l[[1]])) ]
        if(length(valid_start_results)>0){
          # Extract start IDs from the first valid data frame within each valid inner list
          startIdsInList <- sapply(valid_start_results, function(innerList) {
            unique(innerList[[1]]$start_condition_id)
          })
          # Assign names
          names(finalNestedList) <- as.character(na.omit(startIdsInList))
        }
      }, error = function(e) { }) # Ignore naming errors
    }
    
    # 14. Return the final nested list structure.
    # Structure: List (named by startId) -> List (named by targetId) -> survData (data.frame)
    return(finalNestedList)
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