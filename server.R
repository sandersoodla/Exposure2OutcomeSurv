source("scripts/getConditionInfo.R")
source("scripts/conditions.R")
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
  
  
  ## Initialize exposure condition choices
  updateConditionChoices(session, "exposureConditionIds")
  
  ## Initialize outcome condition choices
  updateConditionChoices(session, "outcomeConditionIds")
  
  
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
      idColumn <- df[[colFound[1]]]
      return(unique(as.numeric(idColumn)))
    } else {
      showNotification("Uploaded file does not contain any valid column name for condition IDs.",
                       type = "error")
      return(NULL)
    }
  }
  
  
  # When an exposure condition file is uploaded, update the selected values:
  observeEvent(input$exposureConditionFile, {
    req(input$exposureConditionFile)
    
    # Extract condition concept IDs from file
    sourceIds <- extractConditionIds(input$exposureConditionFile)
    
    if (is.null(sourceIds) || length(sourceIds) == 0) {
      showNotification("No valid concept IDs found or could be extracted.", type = "warning")
      return()
    }

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
      mutate(status = as.character(
        ifelse(
          standard_concept_id %in% allOccurredConditions$concept_id,
          "Mapped",
          "Mapped, no occurrences"
        )
      ))
    
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
    showModal(
      modalDialog(
        title = "Input conditions mapped to standard concepts",
        "Standard - input condition ID is already a standard concept ID;
        Not mapped - ID is non-standard and couldn't be mapped with concept_relationship;
        Mapped, no occurrences - ID mapped to standard, no occurrences in the dataset",
        DT::dataTableOutput("mappingTableDetails"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      )
    )
    
    # Render the mapping details table in the modal
    output$mappingTableDetails <- DT::renderDataTable({
      DT::datatable(mappingDetails, options = list(pageLength = 10))
    })
    
    # Finally, update the selectizeInput with only occurring standard concept IDs
    updateSelectizeInput(
      session,
      "exposureConditionIds",
      choices = setNames(
        allOccurredConditions$concept_id,
        allOccurredConditions$concept_name_id
      ),
      selected = occurredStandardIds,
      server = TRUE
    )
    
  })
  
  
  # When a target condition file is uploaded, update the selected values:
  observeEvent(input$outcomeConditionFile, {
    req(input$outcomeConditionFile)
    
    # Extract condition concept IDs from file
    sourceIds <- extractConditionIds(input$outcomeConditionFile)
    
    if (is.null(sourceIds) || length(sourceIds) == 0) {
      showNotification("No valid concept IDs found or could be extracted.", type = "warning")
      return()
    }
    
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
      mutate(status = as.character(
        ifelse(
          standard_concept_id %in% allOccurredConditions$concept_id,
          "Mapped",
          "Mapped, no occurrences"
        )
      ))
    
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
    showModal(
      modalDialog(
        title = "Input conditions mapped to standard concepts",
        "Standard - input condition ID is already a standard concept ID;
        Not mapped - ID is non-standard and couldn't be mapped with concept_relationship;
        Mapped, no occurrences - ID mapped to standard, no occurrences in the dataset",
        DT::dataTableOutput("mappingTableDetails"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      )
    )
    
    # Render the mapping details table in the modal
    output$mappingTableDetails <- DT::renderDataTable({
      DT::datatable(mappingDetails, options = list(pageLength = 10))
    })
    
    # Finally, update the selectizeInput with only occurring standard concept IDs
    updateSelectizeInput(
      session,
      "outcomeConditionIds",
      choices = setNames(
        allOccurredConditions$concept_id,
        allOccurredConditions$concept_name_id
      ),
      selected = occurredStandardIds,
      server = TRUE
    )
  
  })
  
  
  ############### TRAJECTORY DATA
  
  
  # Reactive expression to fetch data when the button is clicked
  trajectoriesData <- eventReactive(input$runAnalysis, {
    req(length(input$exposureConditionIds) > 0)
    req(length(input$outcomeConditionIds) > 0)
    
    df <- getTrajectoriesForCondition(cdm, input$exposureConditionIds)
    return(df)
  })
  
  # Update the patient timeline selector options when trajectories Data changes
  observeEvent(trajectoriesData(), {
    updateSelectizeInput(
      session,
      "selectedPatient",
      choices = unique(trajectoriesData()$person_id),
      server = TRUE
    )
  })
  
  
  
  #################### KM PLOTS FOR DEFINED TARGET OUTCOMES ####################
  
  
  # --- Function: Calculate Matched Survival Data using Incidence Density Matching ---
  calculateMatchedSurvivalData <- function(selectedExposureIds,
                                           selectedOutcomeIds,
                                           cdm,
                                           matchRatio = 4,
                                           washoutYears = 2) {
    
    # 1. Ensure required inputs and objects are available.
    if (missing(selectedExposureIds) || missing(selectedOutcomeIds) || missing(cdm)) {
      stop("Missing required arguments: selectedExposureIds, selectedOutcomeIds, cdm")
    }
    if (!is.numeric(selectedExposureIds) || !is.numeric(selectedOutcomeIds)) {
      stop("selectedExposureIds and selectedOutcomeIds must be numeric vectors.")
    }
    if (!is.numeric(matchRatio) || matchRatio < 1) {
      stop("matchRatio must be a positive integer.")
    }
    if (!is.numeric(washoutYears) || washoutYears < 0) {
      stop("washoutYears must be a non-negative number.")
    }
    
    showNotification("Starting Incidence Density Matching for selected pairs...", duration = 10, type="message", id="proc_main")
    
    
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
    
    # Fetch ALL relevant condition dates (exposure & outcome) for the base cohort
    allConceptIdsToFetch <- unique(c(selectedExposureIds, selectedOutcomeIds))
    
    allConditionFirstDates <- tryCatch({
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
    if (is.null(allConditionFirstDates) || nrow(allConditionFirstDates) == 0) {
      showNotification("No condition dates found for the selected concepts.", type = "warning")
      removeNotification(id="proc_main")
      return(NULL)
    }
    
    
    # --- 3. Prepare Base Cohort Data (Demographics + Obs periods) ---
    
    cohortBase <- demographicsBase %>%
      inner_join(obsPeriods, by = "person_id") %>%
      filter(obs_end_date > obs_start_date) # Ensure observation period is valid
    
    # Check if the base cohort is valid after joins
    if (nrow(cohortBase) == 0) {
      showNotification("Cohort base is empty after joining observation periods.", type = "warning")
      removeNotification(id="proc_main")
      return(NULL)
    }
    
    
    # --- 4. Initialize Results List ---
    # This list will store the final survival data for each valid exposure-outcome pair
    resultsList <- list()
    
    # --- 5. Outer Loop: Iterate through each selected Outcome condition ---
    for (outcomeId in selectedOutcomeIds) {
      
      # Use id in notifications
      outcomeIdStr <- as.character(outcomeId)
      showNotification(paste("Preparing analysis for Outcome ID:", outcomeIdStr), duration = NA, id = paste0("prep_outcome_", outcomeIdStr))
      
      
      # --- 5a. Apply Washout to this Outcome ---
      
      # Get dates for the current outcome condition
      outcomeDatesCurrentOutcome <- allConditionFirstDates %>%
        filter(concept_id == outcomeId) %>%
        select(person_id, outcome_date = condition_start_date)
      
      # Identify persons eligible for this outcome based on washout
      eligiblePersonsForThisOutcome <- cohortBase %>%
        select(person_id, obs_start_date) %>%
        left_join(outcomeDatesCurrentOutcome, by = "person_id") %>%
        filter(is.na(outcome_date) | outcome_date >= (obs_start_date %m+% years(washoutYears))) %>%
        pull(person_id) %>%
        unique()
      
      if (length(eligiblePersonsForThisOutcome) == 0) {
        showNotification(paste("No persons eligible for Outcome ID:", outcomeIdStr, "after applying", washoutYears, "-year washout."), type = "warning", duration = 5)
        removeNotification(id = paste0("prep_outcome_", outcomeIdStr))
        next # Skip to the next outcomeId
      }
      showNotification(paste("Eligible persons for Outcome ID", outcomeIdStr, "after washout:", length(eligiblePersonsForThisOutcome)), type="message", duration=3)
      
      # Create the base cohort for this outcome analysis
      cohortBaseForOutcome <- cohortBase %>%
        filter(person_id %in% eligiblePersonsForThisOutcome)
      
      
      # --- 5b. Prepare Potential Controls Pool for this Outcome ID ---
      # Contains eligible persons + their date for this outcome (if any)
      controlsPoolBaseOutcome <- cohortBaseForOutcome %>%
        select(person_id, year_of_birth, gender_concept_id, obs_start_date, obs_end_date) %>%
        left_join(outcomeDatesCurrentOutcome, by = "person_id") # Join the dates of the current outcome
      
      removeNotification(id = paste0("prep_outcome_", outcomeIdStr)) # Done preparing for this outcome
      
      
      # --- 6. Inner Loop: Iterate through each selected Exposure condition ---
      for (exposureId in selectedExposureIds) {
        
        exposureIdStr <- as.character(exposureId)
        uniquePairKey <- paste0("pair_", exposureIdStr, "_", outcomeIdStr)
        showNotification(paste("Processing Exposure ID:", exposureIdStr, "for Outcome ID:", outcomeIdStr), duration = NA, id = paste0("proc_pair_", uniquePairKey))
        
        
        # --- 6a. Identify Exposed Individuals for the current exposureId ---
        
        # Find first exposure date among persons eligible for this outcome
        exposureDatesCurrentExposure <- allConditionFirstDates %>%
          filter(person_id %in% eligiblePersonsForThisOutcome, concept_id == exposureId) %>%
          select(person_id, exposure_date = condition_start_date)
        
        # Identify exposed individuals from the outcome-specific eligible cohort
        exposedCohortDefinition <- cohortBaseForOutcome %>%
          inner_join(exposureDatesCurrentExposure, by = "person_id") %>%
          left_join(outcomeDatesCurrentOutcome, by = "person_id") %>%
          filter(!is.na(exposure_date)) %>%
          # Keep only the observation period where the exposure happened, if a person has multiple observation periods
          filter(exposure_date >= obs_start_date & exposure_date <= obs_end_date) %>%
          # Keep person only if outcome date is missing (no outcome) OR occurs strictly AFTER exposure date
          filter(is.na(outcome_date) | outcome_date > exposure_date) %>%
          # Select necessary info, define index date as exposure date
          select(exposed_person_id = person_id,
                 index_date = exposure_date,
                 year_of_birth,
                 gender_concept_id) %>%
          distinct(exposed_person_id, index_date, .keep_all = TRUE) # Ensure unique exposure events per person
        
        if (nrow(exposedCohortDefinition) == 0) {
          showNotification(paste("No valid exposed individuals found for Exposure ID:", exposureIdStr, "(eligible for Outcome", outcomeIdStr, "and outcome-free prior to exposure)"), type = "warning", duration = 6)
          removeNotification(id = paste0("proc_pair_", uniquePairKey))
          next # Skip to next exposureId
        }
        
        
        # --- 6b. Prepare Potential Controls Pool for this Exposure/Outcome pair ---
        
        # Add the exposure condition date info
        potentialControlsPool <- controlsPoolBaseOutcome %>%
          left_join(exposureDatesCurrentExposure, by="person_id")
        
        
        # --- 6c. Loop Through EXPOSED Individuals and Match UNEXPOSED "controls" ---
        matchedSetsList <- list()
        nExposedProcessed <- 0
        nExposedMatched <- 0
        totalExposed <- nrow(exposedCohortDefinition)
        
        for (i in 1:totalExposed) {
          currentExposed <- exposedCohortDefinition[i, ]
          exposedPersonId <- currentExposed$exposed_person_id
          exposureIndexDate <- currentExposed$index_date
          exposedBirthYear <- currentExposed$year_of_birth
          exposedGender <- currentExposed$gender_concept_id
          nExposedProcessed <- nExposedProcessed + 1
          
          # Define Risk Set for this Exposed Person and this Outcome
          # Controls must be free of exposure AND outcome before index date
          riskSet <- potentialControlsPool %>%
            filter(
              person_id != exposedPersonId,
              obs_start_date <= exposureIndexDate,
              obs_end_date >= exposureIndexDate,
              is.na(exposure_date) | exposure_date > exposureIndexDate, # Free of exposure
              is.na(outcome_date) | outcome_date > exposureIndexDate    # Free of outcome
            )
          
          matchedControlsDf <- NULL
          if (nrow(riskSet) > 0) {
            # Perform matching (Nearest Neighbor on age, exact on gender)
            matchedControlsDf <- riskSet %>%
              filter(gender_concept_id == exposedGender) %>%
              mutate(age_diff = abs(year_of_birth - exposedBirthYear)) %>%
              arrange(age_diff) %>%
              slice_head(n = matchRatio)
          }
          
          # Store matched set if controls were found
          if (!is.null(matchedControlsDf) && nrow(matchedControlsDf) > 0) {
            nExposedMatched <- nExposedMatched + 1
            setData <- tibble(
              person_id = c(exposedPersonId, matchedControlsDf$person_id),
              exposure_status = c(1, rep(0, nrow(matchedControlsDf))), # 1=Exposed, 0=Unexposed
              index_date = exposureIndexDate,
              set_id = i # Unique set identifier within this matching run
            )
            matchedSetsList[[i]] <- setData
          } else {
            matchedSetsList[[i]] <- NULL
          }
          
          # Progress update
          if (i %% 50 == 0 || i == totalExposed) {
            currentProgress <- round((i / totalExposed) * 100)
            showNotification(
              paste0("Matching for Exp:", exposureIdStr, ", Out:", outcomeIdStr, " (", currentProgress,"%)"),
              duration = NULL, id = paste0("match_prog_", uniquePairKey), type = "message"
            )
          }
          
        } # End exposed person matching loop
        
        # Clean up progress notification
        removeNotification(id = paste0("match_prog_", uniquePairKey))
        # Remove NULLs where no match was found
        matchedSetsList <- matchedSetsList[!sapply(matchedSetsList, is.null)]
        
        # Check if any sets were actually created
        if (length(matchedSetsList) == 0) {
          showNotification(paste("No matched sets created for Exposure:", exposureIdStr, "Outcome:", outcomeIdStr), type = "warning", duration=5)
          removeNotification(id = paste0("proc_pair_", uniquePairKey))
          next # Skip survival calculation if no sets
        }
        showNotification(paste("Matching complete for Exp:", exposureIdStr, "Out:", outcomeIdStr, "-", nExposedMatched, "sets."), type="message", duration = 3)
        
        # Combine matched data for the current exposure-outcome pair
        matchedDataFinal <- bind_rows(matchedSetsList)
        
        
        # --- 6d. Calculate Survival for this Pair ---
        
        # Prepare base for survival calculation (matched individuals + obs periods)
        survivalInputBase <- matchedDataFinal %>%
          select(person_id, set_id, exposure_status, index_date) %>%
          inner_join(select(cohortBaseForOutcome, person_id, obs_start_date, obs_end_date), by = "person_id") %>%
          distinct()
        
        # Calculate time-to-outcome and status
        survivalDataCurrentPair <- survivalInputBase %>%
          left_join(outcomeDatesCurrentOutcome, by = "person_id") %>%
          mutate(
            outcome_event_date = if_else(!is.na(outcome_date) & outcome_date > index_date & outcome_date <= obs_end_date,
                                         outcome_date, NA_Date_),
            study_exit_date = pmin(obs_end_date, outcome_event_date, na.rm = TRUE),
            outcome_status = if_else(!is.na(outcome_event_date) & outcome_event_date == study_exit_date, 1, 0),
            time_to_outcome = as.numeric(difftime(study_exit_date, index_date, units = "days"))
          ) %>%
          filter(time_to_outcome >= 0) %>%
          select(person_id, set_id, exposure_status, index_date, study_exit_date, time_to_outcome, outcome_status)
        
        # --- Store Results for this Pair ---
        if (nrow(survivalDataCurrentPair) > 0) {
          exposureLabel <- allOccurredConditions$concept_name_id[allOccurredConditions$concept_id == exposureId]
          outcomeLabel <- allOccurredConditions$concept_name_id[allOccurredConditions$concept_id == outcomeId]
          
          resultsList[[uniquePairKey]] <- list(
            survivalData = survivalDataCurrentPair,
            exposureId = exposureId, outcomeId = outcomeId,
            exposureLabel = exposureLabel, outcomeLabel = outcomeLabel,
            nExposedIdentifiedInitial = totalExposed, # Total exposed initially identified for matching
            nExposedIncludedInMatching = nrow(exposedCohortDefinition), # Exposed after prior outcome check
            nExposedMatched = nExposedMatched, # Exposed for whom controls were found
            nUnexposedMatched = sum(survivalDataCurrentPair$exposure_status == 0), # Total matched controls
            nTotalPersonsInAnalysis = nrow(survivalDataCurrentPair) # Total rows in final analysis dataset
          )
          showNotification(paste("Survival calculated for Exposure:", exposureIdStr, "Outcome:", outcomeIdStr), duration = 3, type = "message")
        } else {
          showNotification(paste("No valid survival data for Exposure:", exposureIdStr, "Outcome:", outcomeIdStr), type = "warning", duration = 5)
        }
        removeNotification(id = paste0("proc_pair_", uniquePairKey))
        
      } # End inner loop iterating through exposures

    } # End outer loop iterating through outcomes
    
    
    # --- 7. Return Final Results List ---
    
    # Clean up the main processing notification
    removeNotification(id="proc_main")
    
    # Check if any results were generated
    if (length(resultsList) == 0) {
      showNotification("Processing complete, but no valid results could be generated for any pair.", type="error", duration = 10)
      return(NULL) # Return NULL if the list is empty
    } else {
      showNotification("All processing complete.", type="message", duration = 5)
      
      # Return the list containing results for all processed pairs
      return(resultsList)
    }
  }
  
  
  # Function: Generate KM plots, risk tables and p-value info for each exposure-outcome pair:
  generateKmPlotObjects <- function(matchedSurvivalData, maxPlotTime = 1825) {
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
    
    # Loop through each exposure-outcome pair result stored in the input list
    for (pairKey in names(matchedSurvivalData)) {
      plotCounter <- plotCounter + 1 # Increment counter
      
      # Update progress notification
      showNotification(
        paste0("Generating plot ", plotCounter, " of ", numPairs, ": ", pairKey), 
        duration = NA, 
        id = notificationId,
        type = "message"
      )
      
      # Extract the result list for the current pair
      pairResult <- matchedSurvivalData[[pairKey]]
      
      # Extract the survival dataframe for this pair
      survData <- pairResult$survivalData
      
      # Create a factor for the grouping variable (Exposed vs Unexposed)
      # This provides clearer labels for the plot legend.
      survData <- survData %>%
        mutate(group = factor(exposure_status,
                              levels = c(0, 1),
                              labels = c("Unexposed (Matched)", "Exposed")))
      
      # Fit a KM model using the group variable
      kmFit <- survfit(Surv(time_to_outcome, outcome_status) ~ group, data = survData)
      
      # Find minimum survival for the graph y-axis limit
      # Get summary at the max time
      endSummary <- summary(kmFit, times = maxPlotTime)
      
      # Find smallest survival probability among groups at the end time
      minEndProb <- min(endSummary$surv, na.rm = TRUE)
      
      # Set y-axis lower limit slightly below minimum
      lowerYLim <- max(0, minEndProb - 0.02)
      
      dynamicYLim <- c(lowerYLim, 1.0)
      
      # Calculate the p-value for storing it in the summary table
      pValueInfo <- surv_pvalue(kmFit, data = survData)
      
      # Construct the plot title using labels from the results list
      pairPlotTitle <- paste0("Exposure: ", pairResult$exposureLabel,
                          " | Outcome: ", pairResult$outcomeLabel)
      
      ggsurvObject <- ggsurvplot(
        kmFit,
        data = survData,
        conf.int = TRUE,
        risk.table = TRUE,
        risk.table.height = 0.25,
        legend.title = "Exposure",
        legend.labs = c("Unexposed (Matched)", "Exposed"),
        title = pairPlotTitle,
        xlab = "Days from index date",
        ylab = "Outcome-free probability",
        xlim = c(0, maxPlotTime), # Limit x to max plot time
        ylim = dynamicYLim, # Use dynamic y-limit
        break.time.by = 400
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
  observeEvent(input$runAnalysis, {
    req(input$exposureConditionIds, input$outcomeConditionIds, cdm) 
    req(input$saveFileName, cancelOutput = TRUE)
    
    selectedExposureIds <- as.numeric(input$exposureConditionIds)
    selectedOutcomeIds <- as.numeric(input$outcomeConditionIds)
    
    
    # --- Filename Handling ---
    safeFileName <- gsub("[^a-zA-Z0-9_\\-]", "_", input$saveFileName) 
    safeFileName <- tools::file_path_sans_ext(safeFileName) 
    if (nchar(safeFileName) == 0) {
      showNotification("Please enter a valid file name.", type="error")
      return()
    }
    savePath <- file.path(resultsDir, paste0(safeFileName, ".rds"))
    if (file.exists(savePath)) {
      showNotification(paste("File", basename(savePath), "already exists. Overwriting."), type="warning", duration = 5)
    }
    
    
    # --- Trigger Survival Data Calculation ---
    matchedSurvivalData <- calculateMatchedSurvivalData(selectedExposureIds, selectedOutcomeIds, cdm) 
    
    if (is.null(matchedSurvivalData)) { showNotification("KM analysis failed during data preparation. Nothing saved.", type="error"); return() }
    
    maxPlotTime <- 1825 # End plot at 5 years
    
    # Get plotObject and pvalue for each pair
    plotsAndPvalsData <- generateKmPlotObjects(matchedSurvivalData, maxPlotTime) 
    
    if (is.null(plotsAndPvalsData)) { showNotification("KM analysis completed data preparation, but failed to generate plots. Nothing saved.", type="warning"); return() }
    
    
    # --- Create Summary Data Frame ---
    summaryList <- list()
    
    for(pairKey in names(plotsAndPvalsData)) {
      
      pairResult <- matchedSurvivalData[[pairKey]]
      pairPlotPval <- plotsAndPvalsData[[pairKey]]

      # Get the p-value
      pValue <- pairPlotPval$pValueInfo$pval
      
      nExposedEvents <- sum(pairResult$survivalData$exposure_status == 1 & pairResult$survivalData$outcome_status == 1)
      nUnexposedEvents <- sum(pairResult$survivalData$exposure_status == 0 & pairResult$survivalData$outcome_status == 1)
      
      summaryList[[pairKey]] <- data.frame(
        pairKey = pairKey, 
        exposureCondition = pairResult$exposureLabel, 
        outcomeCondition = pairResult$outcomeLabel,
        exposureID = pairResult$exposureId,
        outcomeID = pairResult$outcomeId, 
        pValue = pValue,
        nExposed = pairResult$nExposedMatched, 
        nUnexposed = pairResult$nUnexposedMatched,
        nExposedEvents = nExposedEvents,
        nUnexposedEvents = nUnexposedEvents,
        totalInAnalysis = pairResult$nTotalPersonsInAnalysis,
        stringsAsFactors = FALSE
      )
    }
    finalSummaryDf <- if(length(summaryList) > 0) bind_rows(summaryList) else NULL
    
    
    # --- Adjust p-values ---
    
    if (!is.null(finalSummaryDf)) {
      pValues <- finalSummaryDf$pValue
      
      # Number of p-values
      numTests <- nrow(finalSummaryDf)
      
      # Adjust p-values with Bonferroni and Holm methods
      finalSummaryDf$pAdjBon <- p.adjust(pValues, method = "bonferroni", n = numTests)
      finalSummaryDf$pAdjHolm <- p.adjust(pValues, method = "holm", n = numTests)
    }
    
    
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
    
    dt <- datatable(
      summaryDf %>% select(exposureCondition, outcomeCondition,
                           pValue, pAdjBon, pAdjHolm,
                           nExposed, nUnexposed, nExposedEvents, nUnexposedEvents, pairKey), 
      colnames = c("Exposure Condition", "Outcome Condition",
                   "p-value (raw)", "p-value (Bonferroni)", "p-value (Holm)",
                   "N Exposed", "N Unexposed", "N Outcomes for Exposed", "N Outcomes for Unexposed", "PairKey"), 
      rownames = FALSE, 
      selection = 'multiple', # Allow multiple rows to be selected
      options = list(pageLength = 10, columnDefs = list(list(visible=FALSE, targets=9)))
    )
    
    # Format p-value columns 
    dt <- dt %>% DT::formatSignif(columns = c("pValue", "pAdjBon", "pAdjHolm"), digits = 3) 
    
    # Style based on significance
    dt <- dt %>% DT::formatStyle(columns = "pAdjHolm", 
                                 fontWeight = styleInterval(0.05, c('bold', 'normal')))
    
    dt
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
      selectedPairKeys <- summaryDf$pairKey[selectedIndices]
      
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
    
    colsPerRow <- 1
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
            suppressWarnings(print(ggsurvObj$plot)) # Suppress warnings when x-axis values are outside the max plot time
          } else {
            plot.new(); title(main = "KM Plot Unavailable") 
          }
        })
        
        # Render the risk table
        output[[tableOutputId]] <- renderPlot({
          if (!is.null(ggsurvObj) && !is.null(ggsurvObj$table)) {
            suppressWarnings(print(ggsurvObj$table))
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
  
  
  # Dynamic UI for exposure condition pyramids
  output$startPyramidsUI <- renderUI({
    req(input$exposureConditionIds)
    tagList(
      lapply(seq_along(input$exposureConditionIds), function(i) {
        # Each pyramid gets its own module instance
        populationPyramidUI(paste0("startPyramid_", i))
      })
    )
  })
  
  # Dynamic UI for outcome condition pyramids
  output$targetPyramidsUI <- renderUI({
    req(input$outcomeConditionIds)
    tagList(
      lapply(seq_along(input$outcomeConditionIds), function(i) {
        populationPyramidUI(paste0("targetPyramid_", i))
      })
    )
  })
  
  # Call the module for each exposure condition
  observe({
    req(input$exposureConditionIds)
    for (i in seq_along(input$exposureConditionIds)) {
      local({
        idx <- i  # Capture the loop variable
        # Wrap the condition in a reactive expression
        condition <- reactive({ input$exposureConditionIds[idx] })
        populationPyramidServer(paste0("startPyramid_", idx), condition)
      })
    }
  })
  
  # Call the module for each outcome condition
  observe({
    req(input$outcomeConditionIds)
    for (i in seq_along(input$outcomeConditionIds)) {
      local({
        idx <- i
        condition <- reactive({ input$outcomeConditionIds[idx] })
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