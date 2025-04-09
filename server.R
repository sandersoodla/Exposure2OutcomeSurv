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
  
  
  # --- Function: Calculate Matched Survival Data using Incidence Density Matching ---
  calculateMatchedSurvivalData <- function(selectedStartIds, selectedTargetIds, cdm) {
    # 1. Ensure required inputs and objects are available.
    req(selectedStartIds, selectedTargetIds, cdm)
    
    showNotification("Starting Incidence Density Matching for selected pairs...", duration = 10, type="message", id="proc_main")
    
    # --- Configuration ---
    matchRatio <- 2 # Number of controls per case
    
    # --- 2. Data Fetching ---
    # Fetch base demographic data for all potential persons
    demographicsBase <- tryCatch({
      getAllGendersAndBirthyears(cdm) %>%
        filter(!is.na(gender_concept_id) & !is.na(year_of_birth))
    }, error = function(e) {
      showNotification(paste("Error fetching demographics:", e$message), type = "error")
      return(NULL)
    })
    # Check if fetching demographics failed or returned empty
    if (is.null(demographicsBase) || nrow(demographicsBase) == 0) {
      removeNotification(id="proc_main")
      return(NULL)
    }
    # Get the list of persons to use for subsequent queries
    basePersonIds <- demographicsBase$person_id
    
    # Fetch observation periods for the base cohort
    obsPeriods <- tryCatch({
      getObservationPeriods(cdm, personIds = basePersonIds)
      # column names: person_id, obs_start_date, obs_end_date
    }, error = function(e) {
      showNotification(paste("Error fetching observation periods:", e$message), type = "error")
      removeNotification(id="proc_main")
      return(NULL)
    })
    # Check if fetching observation periods failed or returned empty
    if (is.null(obsPeriods) || nrow(obsPeriods) == 0) {
      removeNotification(id="proc_main")
      return(NULL)
    }
    
    # Fetch ALL relevant condition dates (start & target) for the base cohort
    allConceptIdsToFetch <- unique(c(selectedStartIds, selectedTargetIds))
    allConditionDates <- tryCatch({
      getFirstConditionDatesForPersons(cdm, personIds = basePersonIds, conceptIds = allConceptIdsToFetch) %>%
        # column names: person_id, concept_id, condition_start_date
        # Remove rows where min(NA) resulted in Inf (no date found)
        filter(!is.infinite(condition_start_date))
    }, error = function(e) {
      showNotification(paste("Error fetching condition dates:", e$message), type = "error")
      removeNotification(id="proc_main")
      return(NULL)
    })
    
    # Check if any condition dates were found
    if (is.null(allConditionDates) || nrow(allConditionDates) == 0) {
      showNotification("No condition dates found for the selected concepts.", type = "warning")
      removeNotification(id="proc_main")
      return(NULL)
    }
    
    # --- 3. Prepare Base Cohort Data (Joining everything) ---
    # Combine demographics, observation periods, and all fetched condition dates
    cohortBase <- demographicsBase %>%
      inner_join(obsPeriods, by = "person_id") %>%
      # Ensure observation period is valid
      filter(obs_end_date > obs_start_date) %>%
      # Join ALL condition dates (long format is useful for filtering later)
      left_join(allConditionDates, by = "person_id")
    
    # Check if the base cohort is valid after joins
    if (nrow(cohortBase) == 0) {
      showNotification("Cohort base is empty after joining observation periods.", type = "warning")
      removeNotification(id="proc_main")
      return(NULL)
    }
    # Note: cohortBase now potentially has multiple rows per person if they have dates for multiple concepts.
    
    # --- 4. Initialize Results List ---
    # This list will store the final survival data for each valid start-target pair
    resultsList <- list()
    
    # --- 5. Outer Loop: Iterate through each selected Start Condition ---
    for (startId in selectedStartIds) {
      # Use startId in notifications
      startIdStr <- as.character(startId)
      showNotification(paste("Processing Start ID:", startIdStr), duration = NA, id = paste0("proc_start_", startIdStr))
      
      # --- 5a. Identify Cases for the CURRENT startId ---
      # Cases are persons with the start condition occurring within their observation period
      cases <- cohortBase %>%
        # Filter rows corresponding to the current start condition ID and ensure the date is valid
        filter(concept_id == startId & !is.na(condition_start_date)) %>%
        # Ensure the condition occurs within the person's observation period
        filter(condition_start_date >= obs_start_date & condition_start_date <= obs_end_date) %>%
        # Select relevant information for the case and define the index date
        select(case_id = person_id,
               index_date = condition_start_date, # The date the start condition occurred is the index date for matching
               year_of_birth,
               gender_concept_id) %>%
        # Ensure unique case events (if a person could have multiple obs periods)
        distinct(case_id, index_date, .keep_all = TRUE)
      
      # Check if any cases were found for this start condition
      if (nrow(cases) == 0) {
        showNotification(paste("No valid cases found for Start ID:", startIdStr, "within observation periods."), type = "warning", duration = 5)
        removeNotification(id = paste0("proc_start_", startIdStr))
        next # Skip to the next startId in the outer loop
      }
      showNotification(paste("Identified", nrow(cases), "cases for Start ID:", startIdStr), type="message", duration = 5)
      
      # --- 5b. Prepare Potential Controls Pool Data (for efficient lookup) ---
      # This pool contains necessary info (demographics, obs period, date of CURRENT start condition if any)
      # for everyone, needed to define the risk set quickly inside the case loop.
      
      # Get dates ONLY for the current start condition
      startCondDatesPool <- allConditionDates %>%
        filter(concept_id == startId) %>%
        select(person_id, start_cond_date = condition_start_date)
      
      # Create the pool by joining demographics, observation periods, and the specific start condition date
      potentialControlsPool <- demographicsBase %>%
        inner_join(obsPeriods, by="person_id") %>%
        filter(obs_end_date > obs_start_date) %>%
        select(person_id, year_of_birth, gender_concept_id, obs_start_date, obs_end_date) %>%
        # Join the date of the current start condition (will be NA if person never had it)
        left_join(startCondDatesPool, by="person_id")
      
      
      # --- 5c. Loop Through Cases and Match (Incidence Density Matching) ---
      # This list will hold the matched sets (dataframes) for the current startId
      matchedSetsList <- list()
      nCasesProcessed <- 0 # Counter for cases looped through
      nCasesMatched <- 0 # Counter for cases for whom controls were found
      
      totalCases <- nrow(cases)
      # Iterate through each identified case for the current startId
      for (i in 1:totalCases) {
        # Get details for the current case
        currentCase <- cases[i, ]
        caseId <- currentCase$case_id
        caseIndexDate <- currentCase$index_date
        caseBirthYear <- currentCase$year_of_birth
        caseGender <- currentCase$gender_concept_id
        # Calculate the case's age in years at the index date for matching
        caseAttainedAgeYear <- year(caseIndexDate) - caseBirthYear
        nCasesProcessed <- nCasesProcessed + 1
        
        # Define the Risk Set: Find potential controls eligible at the case's index date
        riskSet <- potentialControlsPool %>%
          filter(
            person_id != caseId, # Exclude the case themselves
            # Ensure potential controls are observed at the case's index date
            obs_start_date <= caseIndexDate,
            obs_end_date >= caseIndexDate,
            # Crucial: Ensure controls do NOT have the start condition ON OR BEFORE the index date
            is.na(start_cond_date) | start_cond_date > caseIndexDate
          )
        
        matchedControlsDf <- NULL # Initialize dataframe for matched controls
        
        # Proceed only if there are potential controls in the risk set
        if (nrow(riskSet) > 0) {
          # Calculate attained age for potential controls in the risk set at the case's index date
          riskSetWithAge <- riskSet %>%
            mutate(
              attained_age_year = year(caseIndexDate) - year_of_birth
            )
          
          # Perform Matching: Find controls matching the case (within the risk set)
          # Example: Exact match on gender, nearest neighbor on attained age
          matchedControlsDf <- riskSetWithAge %>%
            filter(gender_concept_id == caseGender) %>% # Match on gender
            mutate(age_diff = abs(attained_age_year - caseAttainedAgeYear)) %>% # Calculate age difference
            arrange(age_diff) %>% # Find controls with the smallest age difference
            slice_head(n = matchRatio) # Select the desired number of controls
        }
        
        # Store the matched set (case + selected controls) if controls were successfully found
        if (!is.null(matchedControlsDf) && nrow(matchedControlsDf) > 0) {
          nCasesMatched <- nCasesMatched + 1
          # Create a dataframe for this matched set
          setData <- tibble(
            person_id = c(caseId, matchedControlsDf$person_id),
            is_case = c(1, rep(0, nrow(matchedControlsDf))), # 1 for case, 0 for controls
            index_date = caseIndexDate, # Assign the case's index date to everyone in the set
            set_id = i # Use the case loop index as a unique identifier for this set
          )
          # Store this set's dataframe in the list
          matchedSetsList[[i]] <- setData
        } else {
          # Store NULL if no controls were found for this case
          matchedSetsList[[i]] <- NULL
        }
        
        # Update progress notification periodically
        if (i %% 50 == 0 || i == totalCases) {
          currentProgress <- round((i / totalCases) * 100)
          showNotification(
            paste0("Matching for Start ID: ", startIdStr, " (", currentProgress,"%) - Matched ", nCasesMatched, "/", i),
            duration = NULL, # Keep showing until next update
            id = paste0("match_prog_", startIdStr),
            type = "message"
          )
        }
      } # End loop iterating through cases for the current startId
      
      # Remove the progress notification for this startId
      removeNotification(id = paste0("match_prog_", startIdStr))
      
      # Filter out the NULL entries where no match was found
      matchedSetsList <- matchedSetsList[!sapply(matchedSetsList, is.null)]
      
      # Check if any matched sets were created for this startId
      if (length(matchedSetsList) == 0) {
        showNotification(paste("Matching complete, but no matched sets could be created for Start ID:", startIdStr), type = "warning", duration = 5)
        removeNotification(id = paste0("proc_start_", startIdStr))
        next # Skip to the next startId
      }
      showNotification(paste("Matching complete for Start ID:", startIdStr, ". Created sets for", nCasesMatched, "out of", nCasesProcessed, "cases."), type="message", duration = 5)
      
      # Consolidate all matched sets for the CURRENT startId into a single dataframe
      matchedDataFinal <- bind_rows(matchedSetsList)
      
      # --- 6. Inner Loop: Calculate Survival for each Target Condition ---
      # Prepare base data for survival calculation by joining matched data with observation periods
      # This ensures we have the correct obs_start_date and obs_end_date for each person in the matched sets
      survivalInputBase <- matchedDataFinal %>%
        select(person_id, set_id, is_case, index_date) %>% # Keep matching info
        # Join back to get observation periods for the matched individuals
        inner_join(select(potentialControlsPool, person_id, obs_start_date, obs_end_date), by = "person_id") %>%
        distinct() # Ensure unique person-set combinations
      
      
      # Iterate through each selected target condition ID
      for (targetId in selectedTargetIds) {
        targetIdStr <- as.character(targetId)
        # Show notification for survival calculation step
        showNotification(paste("Calculating survival for Start:", startIdStr, "Target:", targetIdStr), duration = NA, id = paste0("proc_surv_", startIdStr, "_", targetIdStr))
        
        # Get the dates for the CURRENT target condition from the pre-fetched data
        targetDatesCurrent <- allConditionDates %>%
          filter(concept_id == targetId) %>%
          select(person_id, target_cond_date = condition_start_date)
        
        # Join the target dates and calculate survival outcomes (time-to-event, status)
        survivalDataCurrentPair <- survivalInputBase %>%
          # Add the date of the target condition (if any) for each person
          left_join(targetDatesCurrent, by = "person_id") %>%
          mutate(
            # Determine the date of the outcome event, must be AFTER index date and within observation
            event_date = if_else(!is.na(target_cond_date) & target_cond_date > index_date & target_cond_date <= obs_end_date,
                                 target_cond_date,
                                 NA_Date_),
            
            # Determine the end of follow-up: observation end or event date, whichever is earlier
            study_exit_date = pmin(obs_end_date, event_date, na.rm = TRUE),
            
            # Ensure follow-up does not end before it starts (handles edge cases)
            study_exit_date = pmax(study_exit_date, index_date),
            
            # Determine event status: 1 if follow-up ended due to the target condition, 0 otherwise (censored)
            event_status = if_else(!is.na(event_date) & event_date == study_exit_date, 1, 0),
            
            # Calculate follow-up time in days from index date to exit date
            time_to_event = as.numeric(difftime(study_exit_date, index_date, units = "days"))
          ) %>%
          # Filter out any rows with invalid follow-up time (should be >= 0)
          filter(time_to_event >= 0) %>%
          # Select the final columns needed for survival analysis functions
          select(person_id, set_id, is_case, index_date, study_exit_date, time_to_event, event_status)
        
        # Check if any valid survival data was generated for this pair
        if (nrow(survivalDataCurrentPair) > 0) {
          # Store the results for this specific start-target pair in the main results list
          uniquePairKey <- paste0("pair_", startIdStr, "_", targetIdStr)
          # Get concept names for labels
          startLabel <- allOccurredConditions$concept_name_id[allOccurredConditions$concept_id == startId]
          targetLabel <- allOccurredConditions$concept_name_id[allOccurredConditions$concept_id == targetId]
          
          # Store the data and metadata in the list
          resultsList[[uniquePairKey]] <- list(
            survivalData = survivalDataCurrentPair,
            startConditionId = startId,
            targetConditionId = targetId,
            startConditionLabel = startLabel,
            targetConditionLabel = targetLabel,
            nCasesInMatching = nrow(cases), # Total cases identified for this startId
            nCasesMatched = nCasesMatched, # Cases for whom at least one control was found
            nControlsMatched = sum(survivalDataCurrentPair$is_case == 0), # Total controls in final analysis data for this pair
            nTotalPersonsInAnalysis = nrow(survivalDataCurrentPair) # Total rows in analysis data for this pair
          )
          showNotification(paste("Survival calculated for Start:", startIdStr, "Target:", targetIdStr), duration = 3, type = "message")
        } else {
          # Notify if no valid survival data resulted for this pair
          showNotification(paste("No valid survival data for Start:", startIdStr, "Target:", targetIdStr), type = "warning", duration = 5)
        }
        # Remove the survival calculation notification for this pair
        removeNotification(id = paste0("proc_surv_", startIdStr, "_", targetIdStr))
        
      } # End inner loop iterating through targetIds
      # Remove the notification for the current startId processing
      removeNotification(id = paste0("proc_start_", startIdStr))
      
    } # End outer loop iterating through startIds
    
    # --- 7. Return Final Results List ---
    # Clean up the main processing notification
    removeNotification(id="proc_main")
    # Check if any results were generated at all
    if (length(resultsList) == 0) {
      showNotification("Processing complete, but no results were generated for any pair.", type="error", duration = 10)
      return(NULL) # Return NULL if the list is empty
    } else {
      showNotification("All processing complete.", type="message", duration = 5)
      
      # Return the list containing results for all processed pairs
      return(resultsList)
    }
  }
  
  
  # Function: Generate KM plots, risk tables and p-value info for each start-target pair:
  generateKmPlotObjects <- function(matchedSurvivalData) {
    req(matchedSurvivalData)
    
    numPairs <- length(matchedSurvivalData)
    
    # Show initial notification for plot generation
    notificationId <- "km_plot_progress" # Unique ID for this notification
    showNotification(
      paste("Starting KM plot generation for", numPairs, "pairs..."), 
      duration = NA, # Keep open until removed
      id = notificationId, 
      type = "message"
    )
    # Ensure notification is removed when function exits (even on error)
    on.exit(removeNotification(id = notificationId), add = TRUE) 
    
    # Create an empty list to store plots and tables. Use names that indicate the pair.
    plotsList <- list()
    plotCounter <- 0
    
    # Loop through each start-target pair result stored in the input list
    for (pairKey in names(matchedSurvivalData)) {
      plotCounter <- plotCounter + 1 # Increment counter
      
      # Update progress notification
      showNotification(
        paste0("Generating plot ", plotCounter, " of ", numPairs, ": ", pairKey), 
        duration = NA, 
        id = notificationId, # Use same ID to update
        type = "message"
      )
      
      # Extract the result list for the current pair
      pairResult <- matchedSurvivalData[[pairKey]]
      # Extract the survival dataframe for this pair
      survData <- pairResult$survivalData
      
      # Create a factor for the grouping variable (Case vs Control)
      # This provides clearer labels for the plot legend.
      survData <- survData %>%
        mutate(group = factor(is_case,
                              levels = c(0, 1),
                              labels = c("Control (Matched)", "Case (Start Cond.)")))
      
      # Fit a KM model using the group variable (With vs. Without start condition)
      kmFit <- survfit(Surv(time_to_event, event_status) ~ group, data = survData)
      
      # Calculate the p-value for storing it alongside the plots
      pValueInfo <- surv_pvalue(kmFit, data = survData)
      
      # Construct the plot title using labels from the results list
      pairLabel <- paste0("Start: ", pairResult$startConditionLabel,
                          " | Outcome: ", pairResult$targetConditionLabel)
      
      ggsurvObject <- ggsurvplot(
        kmFit,
        data = survData,
        conf.int = TRUE,
        pval = TRUE,
        risk.table = TRUE,
        risk.table.height = 0.25,
        legend.title = "Group",
        title = pairLabel,
        xlab = "Days from index date",
        ylab = "Target-free probability"
      )
      
      # Store the entire ggsurvplot list object (plot + table)
      # and the p-value
      if (!is.null(ggsurvObject)) {
        plotsList[[pairKey]] <- list(
          plotObject = ggsurvObject,
          pValueInfo = pValueInfo
        )
      }
    }
    plotsList
    
  }
  
  
  # --- KM Computation, Plot Generation, and saving (Triggered by Button) ---
  observeEvent(input$getData, {
    req(input$startConditionId, input$targetConditionId, cdm) 
    req(input$saveFileName, cancelOutput = TRUE) 
    
    # --- Filename Handling ---
    safeFileName <- gsub("[^a-zA-Z0-9_\\-]", "_", input$saveFileName) 
    safeFileName <- tools::file_path_sans_ext(safeFileName) 
    if (nchar(safeFileName) == 0) { showNotification("Please enter a valid file name.", type="error"); return() }
    savePath <- file.path(resultsDir, paste0(safeFileName, ".rds"))
    if (file.exists(savePath)) { showNotification(paste("File", basename(savePath), "already exists. Overwriting."), type="warning", duration = 5) }
    
    # --- Trigger Calculation ---
    resultsData <- calculateMatchedSurvivalData(input$startConditionId, input$targetConditionId, cdm) 
    if (is.null(resultsData)) { showNotification("KM analysis failed during data preparation. Nothing saved.", type="error"); return() }
    
    # This returns a list containing plotObject and pvalue for each pair
    plotsAndPvalsData <- generateKmPlotObjects(resultsData) 
    if (is.null(plotsAndPvalsData)) { showNotification("KM analysis completed data preparation, but failed to generate plots. Nothing saved.", type="warning"); return() }
    
    # --- Create Summary Data Frame ---
    summaryList <- list()
    for(pairKey in names(resultsData)) {
      pairResult <- resultsData[[pairKey]]

      # Get the p-value
      pValue <- plotsAndPvalsData[[pairKey]]$pValueInfo$pval
      
      summaryList[[pairKey]] <- data.frame(
        PairKey = pairKey, 
        StartCondition = pairResult$startConditionLabel, TargetCondition = pairResult$targetConditionLabel,
        StartID = pairResult$startConditionId, TargetID = pairResult$targetConditionId, 
        PValue = pValue,
        NCases = pairResult$nCasesMatched, 
        NControls = pairResult$nControlsMatched,
        NCaseEvents = sum(pairResult$survivalData$is_case == 1 & pairResult$survivalData$event_status == 1),
        NControlEvents = sum(pairResult$survivalData$is_case == 0 & pairResult$survivalData$event_status == 1),
        stringsAsFactors = FALSE
      )
    }
    finalSummaryDf <- if(length(summaryList) > 0) bind_rows(summaryList) else NULL
    
    # --- Combine and Save ---
    # Extract only the plot objects for saving in the 'plots' component
    finalPlotsData <- lapply(plotsAndPvalsData, function(item) item$plotObject)
    
    if (!is.null(finalSummaryDf) || length(finalPlotsData) > 0) { 
      # Save the summary and the list of actual plot objects
      resultsToSave <- list(summary = finalSummaryDf, plots = finalPlotsData) 
      
      tryCatch({
        saveRDS(resultsToSave, file = savePath) 
        
        # Update reactive values for current session
        loadedSummary(resultsToSave$summary) 
        loadedPlots(resultsToSave$plots) # Store the list of plot objects    
        currentLoadedFileName(basename(savePath)) 
        
        # Update file list dropdown
        savedFiles <- listSavedFiles()
        updateSelectInput(session, "selectLoadFile", choices = savedFiles, selected = tools::file_path_sans_ext(basename(savePath))) 
        
        numPairsSaved <- if(!is.null(resultsToSave$summary)) nrow(resultsToSave$summary) else 0
        showNotification(paste("KM analysis complete. Results saved as", basename(savePath), "for", numPairsSaved, "pairs."), type = "message", duration=10)
        
      }, error = function(e) { showNotification(paste("Error saving results:", e$message), type = "error") })
    } else {
      showNotification("No valid summary or plot results generated to save.", type="warning")
    }
    
  })
  
  
  #### LOADING KM ANALYSIS RESULTS FROM FILES
  
  # Define directory for storing computed KM results
  resultsDir <- "km_results" 
  dir.create(resultsDir, showWarnings = FALSE) # Create directory if it doesn't exist
  
  # --- Reactive Values to Store Loaded KM Results ---
  # Stores the summary data frame (p-values, counts, etc.) from the currently loaded file
  loadedSummary <- reactiveVal(NULL)
  # Stores the list of ggsurvplot objects (plot + table) from the currently loaded file
  loadedPlots <- reactiveVal(NULL) 
  # Stores the name of the currently loaded result set
  currentLoadedFileName <- reactiveVal(NULL)
  
  # --- Function to List Available Result Files ---
  listSavedFiles <- function() {
    files <- list.files(path = resultsDir, pattern = "\\.rds$", full.names = FALSE)
    # Remove the .rds extension for display
    tools::file_path_sans_ext(files) 
  }
  
  # --- Update Load File Choices ---
  # Use an observer to update the choices when the app starts and after saving
  observe({
    savedFiles <- listSavedFiles()
    updateSelectInput(session, "selectLoadFile", choices = savedFiles)
  })
  
  
  # --- Output: Display Currently Loaded File Name ---
  output$currentResultSet <- renderText({
    fname <- currentLoadedFileName()
    if (is.null(fname) || nchar(fname) == 0) { "No result set loaded." } 
    else { paste("Currently loaded:", fname) }
  })
  
  
  # --- Load Selected Results File (Triggered by Button) ---
  observeEvent(input$loadButton, {
    selectedFileBaseName <- input$selectLoadFile 
    req(selectedFileBaseName, cancelOutput = TRUE) 
    loadPath <- file.path(resultsDir, paste0(selectedFileBaseName, ".rds"))
    if (file.exists(loadPath)) {
      tryCatch({
        shiny::withProgress(message = 'Loading selected results...', value = 0.5, {
          loadedData <- readRDS(loadPath)
          loadedSummary(loadedData$summary)
          loadedPlots(loadedData$plots) 
          currentLoadedFileName(basename(loadPath)) 
          selectedPlotsDataList(list()) # Clear selection when loading new file
          incProgress(0.5) 
        }) 
        showNotification(paste("Loaded results from", basename(loadPath)), type="message")
      }, error = function(e) { 
        showNotification(paste("Error loading file:", e$message), type="error")
        loadedSummary(NULL); loadedPlots(NULL); currentLoadedFileName(NULL); selectedPlotsDataList(list()) 
      })
    } else { 
      showNotification(paste("Selected file not found:", basename(loadPath)), type="error")
    }
  })
  
  
  # --- Output: KM Summary Table ---
  output$kmSummaryTable <- DT::renderDataTable({
    req(loadedSummary())
    summaryDf <- loadedSummary() 
    if("PValue" %in% names(summaryDf)) {
      summaryDf$PValueFormatted <- scales::pvalue(summaryDf$PValue, accuracy = 0.001, add_p = TRUE) 
    } else { summaryDf$PValueFormatted <- "N/A" }
    datatable(
      summaryDf %>% select(StartCondition, TargetCondition, PValueFormatted, NCases, NControls, NCaseEvents, NControlEvents, PairKey), 
      colnames = c("Start Condition", "Outcome Condition", "P-Value", "N Cases", "N Controls", "N Case Events", "N Control Events", "PairKey"), 
      rownames = FALSE, 
      selection = 'multiple', # Allow multiple rows to be selected
      options = list(pageLength = 10, columnDefs = list(list(visible=FALSE, targets=7)))
    ) %>% DT::formatStyle(columns = "PValueFormatted", fontWeight = styleInterval(0.05, c('bold', 'normal'))) 
  }) 
  
  
  # --- Reactive Value for Selected Plot Data ---
  selectedPlotsDataList <- reactiveVal(list()) # Initialize as an empty list
  
  
  # --- Observer for Summary Table Row Selection ---
  observeEvent(input$kmSummaryTable_rows_selected, {
    # Ensure summary and plots are loaded before processing selection
    req(loadedSummary()) 
    selectedIndices <- input$kmSummaryTable_rows_selected
    
    # If rows are selected
    if (!is.null(selectedIndices) && length(selectedIndices) > 0) {
      summaryDf <- loadedSummary()
      # Get the PairKeys for all selected rows
      selectedPairKeys <- summaryDf$PairKey[selectedIndices]
      
      # Check if plots are loaded (load if necessary)
      if (is.null(loadedPlots())) {
        currentFileBaseName <- tools::file_path_sans_ext(currentLoadedFileName())
        if (!is.null(currentFileBaseName) && nchar(currentFileBaseName) > 0) {
          loadPath <- file.path(resultsDir, paste0(currentFileBaseName, ".rds"))
          if(file.exists(loadPath)) { 
            tryCatch({
              loadedData <- readRDS(loadPath) 
              if (!is.null(loadedData$plots)) { loadedPlots(loadedData$plots) } 
              else { showNotification("Plots data not found within the current results file.", type="warning"); return() }
            }, error = function(e) { showNotification(paste("Error loading results file for plots:", e$message), type = "error"); return() })
          } else { showNotification("Current results file not found.", type="error"); return() }
        } else { showNotification("No result set is currently loaded.", type="warning"); return() }
      } 
      
      # Retrieve plot objects for selected keys
      plotList <- loadedPlots() 
      tempSelectedList <- list() # Temporary list to build the selection
      
      if (!is.null(plotList)) {
        for (key in selectedPairKeys) {
          if (key %in% names(plotList)) {
            # Add the plot object to the temp list, using the key as the name
            tempSelectedList[[key]] <- plotList[[key]] 
          } else {
            warning("Plot data for key ", key, " not found in the loaded set.")
          }
        }
      }
      # Update the reactive value with the list of selected plot objects
      selectedPlotsDataList(tempSelectedList) 
      
    } else { 
      # If no rows are selected, clear the list
      selectedPlotsDataList(list()) 
    }
  }, ignoreNULL = FALSE) # ignoreNULL=FALSE ensures clearing works when selection becomes NULL
  
  
  # --- Output: Dynamic UI for KM Plot Grid ---
  output$kmPlotsUI <- renderUI({
    # Require the list of selected plots
    selectedPlots <- selectedPlotsDataList()
    req(length(selectedPlots) > 0) # Only render if plots are selected
    
    numPlots <- length(selectedPlots)
    plotKeys <- names(selectedPlots) # Get the unique keys for selected plots
    
    colsPerRow <- 2
    numRows <- ceiling(numPlots / colsPerRow)
    plotTags <- list()
    plotIndex <- 1
    
    for (r in 1:numRows) {
      rowContent <- list()
      for (c in 1:colsPerRow) {
        if (plotIndex <= numPlots) {
          # Use the unique pairKey for the output IDs
          currentKey <- plotKeys[[plotIndex]]
          
          rowContent[[c]] <- column(width = 12 / colsPerRow,
                                    # Placeholder for the KM plot
                                    plotOutput(outputId = paste0("kmPlot_", currentKey)), 
                                    # Placeholder for the risk table
                                    plotOutput(outputId = paste0("kmTable_", currentKey), height = "200px") 
          )
          plotIndex <- plotIndex + 1
        }
      }
      plotTags[[r]] <- fluidRow(rowContent)
    }
    do.call(tagList, plotTags) # Combine all rows into a single UI output
  })
  
  # --- Observer for Rendering Plots into the Grid ---
  observe({
    # React to changes in the list of selected plots
    selectedPlots <- selectedPlotsDataList()
    req(length(selectedPlots) > 0) # Only run if plots are selected
    
    plotKeys <- names(selectedPlots) # Get the keys of the selected plots
    
    # Loop through the selected plots and render them
    for (key in plotKeys) {
      # Use local() to ensure correct scoping of variables within the loop for renderPlot
      local({
        currentKey <- key # Capture key for this iteration
        ggsurvObj <- selectedPlots[[currentKey]] # Get the ggsurvplot object
        
        # Define output IDs based on the key (must match renderUI)
        plotOutputId <- paste0("kmPlot_", currentKey)
        tableOutputId <- paste0("kmTable_", currentKey)
        
        # Render the KM plot
        output[[plotOutputId]] <- renderPlot({
          if (!is.null(ggsurvObj) && !is.null(ggsurvObj$plot)) {
            print(ggsurvObj$plot) 
          } else {
            plot.new(); title(main = "KM Plot Unavailable") 
          }
        })
        
        # Render the risk table
        output[[tableOutputId]] <- renderPlot({
          if (!is.null(ggsurvObj) && !is.null(ggsurvObj$table)) {
            print(ggsurvObj$table) 
          } else {
            plot.new(); title(main = "Risk Table Unavailable") 
          }
        })
      })
    }
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