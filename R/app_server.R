
# Define server logic 
app_server <- function(input, output, session) {
  
  #################### CONNECTION SETUP ###########
  
  DBMS <- Sys.getenv("DBMS")
  DATABASE <- paste(Sys.getenv("DB_HOST"),"/",Sys.getenv("DB_NAME"),sep='')
  DB_USER <- Sys.getenv('DB_USERNAME')
  DB_PASSWORD <- Sys.getenv('DB_PASSWORD')
  DB_PORT <- Sys.getenv('DB_PORT')
  
  CDM_SCHEMA <- Sys.getenv("CDM_SCHEMA")
  WRITE_SCHEMA <- Sys.getenv("WRITE_SCHEMA")
  
  
  if (DBMS == "duckdb") {
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = DATABASE)
    cdm <- CDMConnector::cdmFromCon(connection, cdmSchema = CDM_SCHEMA, writeSchema = WRITE_SCHEMA)
    
  } else {
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = DBMS,
      server = DATABASE,
      user = DB_USER,
      password = DB_PASSWORD,
      port = DB_PORT
    )
    connection <- DatabaseConnector::connect(connectionDetails)
    cdm <- CDMConnector::cdmFromCon(connection, cdmSchema = CDM_SCHEMA, writeSchema = WRITE_SCHEMA, cdmName = "maitt_cdm")
    
  }
  
  
  
  ### OUTPUTS ##############
  
  
  # METADATA
  metadata <- getMetadata(cdm)
  
  req(metadata, cancelOutput = TRUE)
  
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
      dplyr::mutate(status = as.character(
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
      dplyr::mutate(status = as.character(
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
  
  
  #### SURVIVAL ANALYSIS
  
  # --- Survival Analysis Computation, Plot Generation, and saving (Triggered by Button) ---
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
    
    
    # --- Survival Data Preparation and Matching ---
    matchedSurvivalData <- calculateMatchedSurvivalData(selectedExposureIds, selectedOutcomeIds, cdm, allOccurredConditions, session = session) 
    
    if (is.null(matchedSurvivalData)) { showNotification("Analysis failed during data preparation. Nothing saved.", type="error"); return() }
    
    
    # --- Generate KM plots and p-values ---
    maxPlotTime <- 2190 # End plot at 6 years
    
    # Get plotObject and pvalue for each pair
    kmResultsList <- generateKmPlotObjects(matchedSurvivalData, maxPlotTime) 
    
    if (is.null(kmResultsList)) { showNotification("Failed to generate KM plots.", type="error"); return() }
    
    
    # --- Calculate Cox Model Results ---
    coxResultsList <- calculateCoxResults(matchedSurvivalData)
    
    if (is.null(coxResultsList)) { showNotification("Failed to calculate Cox model results.", type="error"); return() }
    
    
    # --- Create Summary Data Frame ---
    summaryList <- list()
    
    for(pairKey in names(matchedSurvivalData)) {
      
      pairData <- matchedSurvivalData[[pairKey]]
      pairKM <- kmResultsList[[pairKey]]
      pairCox <- coxResultsList[[pairKey]]

      # Get the p-value
      kmPvalue <- pairKM$kmPValueInfo$pval
      
      nExposedEvents <- sum(pairData$survivalData$exposure_status == 1 & pairData$survivalData$outcome_status == 1)
      nUnexposedEvents <- sum(pairData$survivalData$exposure_status == 0 & pairData$survivalData$outcome_status == 1)
      
      summaryList[[pairKey]] <- data.frame(
        pairKey = pairKey, 
        exposureCondition = pairData$exposureLabel, 
        outcomeCondition = pairData$outcomeLabel,
        exposureID = pairData$exposureId,
        outcomeID = pairData$outcomeId, 
        kmPvalue = kmPvalue,
        nExposed = pairData$nExposedMatched, 
        nUnexposed = pairData$nUnexposedMatched,
        nExposedEvents = nExposedEvents,
        nUnexposedEvents = nUnexposedEvents,
        totalInAnalysis = pairData$nTotalPersonsInAnalysis,
        hazardRatio = pairCox$hazardRatio,
        hrCiLower = pairCox$hrCiLower,     
        hrCiUpper = pairCox$hrCiUpper,     
        hrPvalue = pairCox$hrPvalue,
        stringsAsFactors = FALSE
      )
    }
    finalSummaryDf <- if(length(summaryList) > 0) dplyr::bind_rows(summaryList) else NULL
    
    
    # --- Adjust p-values ---
    
    if (!is.null(finalSummaryDf)) {
      kmPvalues <- finalSummaryDf$kmPvalue
      hrPvalues <- finalSummaryDf$hrPvalue
      
      # Number of p-values
      numTests <- nrow(finalSummaryDf)
      
      #finalSummaryDf$pAdjBon <- p.adjust(pValues, method = "bonferroni", n = numTests)
      
      # Adjust p-values with the Holm method
      finalSummaryDf$kmPvalueAdjHolm <- p.adjust(kmPvalues, method = "holm", n = numTests)
      finalSummaryDf$hrPvalueAdjHolm <- p.adjust(hrPvalues, method = "holm", n = numTests)
    }
    
    
    # --- Combine and Save ---
    
    # Extract only the plot objects for saving in the 'plots' component
    finalPlotsData <- lapply(kmResultsList, function(item) item$plotObject)
    
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
  
  
  #### LOADING SURVIVAL ANALYSIS RESULTS FROM FILES
  
  # --- Determine Results Directory ---
  # Function to get the results directory path
  getResultsDir <- function() {
    # Check Environment Variable
    envDir <- Sys.getenv("SURV_RESULTS_DIR")
    if (envDir == "") {
      stop("SURV_RESULTS_DIR environment variable empty or not set")
    } else {
      message("Using results directory from environment variable SURV_RESULTS_DIR: ", envDir)
      return(fs::path_norm(envDir)) # Normalize path
    }
  }
  
  # Define directory for storing computed survival analysis results
  resultsDir <- getResultsDir()
  
  # Create directory if it doesn't exist (use recursive = TRUE)
  if (!dir.exists(resultsDir)) {
    dir.create(resultsDir, showWarnings = TRUE, recursive = TRUE)
    message("Created results directory: ", resultsDir)
  }
  
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
    
    # Create formatted CI column
    summaryDf <- summaryDf %>%
      dplyr::mutate(
        hrCiLowerFormatted = format(round(hrCiLower, 3), nsmall = 3),
        hrCiUpperFormatted = format(round(hrCiUpper, 3), nsmall = 3),
        # Combine into a single string column
        hrCI_combined = paste0(hrCiLowerFormatted, " - ", hrCiUpperFormatted)
      )
    
    colsToSelect <- c(
      "exposureCondition", "outcomeCondition", 
      "hazardRatio", "hrCI_combined",          # HR and CI
      "hrPvalueAdjHolm",                       # Adjusted HR P-value
      "kmPvalueAdjHolm",                       # Adjusted KM P-value 
      "nExposed", "nUnexposed", 
      "nExposedEvents", "nUnexposedEvents",
      "pairKey"                                # Hidden key for plot selection
    )
    
    colNamesDisplay <- c(
      "Exposure", "Outcome", 
      "HR", "HR CI (95%)", 
      "HR p-val Adjusted (Holm)", 
      "KM p-val Adjusted (Holm)", 
      "N Exposed", "N Unexposed", 
      "N Outcomes for Exposed", "N Outcomes for Unexposed",
      "PairKey" 
    )
    
    dt <- DT::datatable(
      summaryDf %>% dplyr::select(dplyr::all_of(colsToSelect)), 
      colnames = colNamesDisplay, 
      rownames = FALSE, 
      selection = "multiple", # Allow multiple rows to be selected
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        #layout = list(
        #  topStart = "buttons",
        #  topEnd = "search",
        #  bottomStart = "info",
        #  bottomEnd = "paging"
        #),
        dom = "lBfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          # Hide pairKey column
          list(visible = FALSE, targets = length(colsToSelect) - 1)))
    )
    
    # Format number columns 
    dt <- dt %>% DT::formatSignif(columns = c("hazardRatio", "hrPvalueAdjHolm", "kmPvalueAdjHolm"), digits = 3) 
    
    # Style based on significance
    dt <- dt %>% DT::formatStyle(columns = c("hrPvalueAdjHolm", "kmPvalueAdjHolm"), 
                                 fontWeight = DT::styleInterval(0.05, c("bold", "normal")))
    
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
                                    # Placeholder for the KM plot (and risk table)
                                    plotOutput(outputId = paste0("kmPlot_", currentKey), height = "600px"), 
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
        
        # Define output ID based on the key
        plotOutputId <- paste0("kmPlot_", currentKey)
        
        # Render the KM plot and table
        output[[plotOutputId]] <- renderPlot({
          if (!is.null(ggsurvObj)) {
            suppressWarnings(print(ggsurvObj)) # Suppress warnings when x-axis values are outside the max plot time
          } else {
            plot.new(); title(main = "KM Plot Unavailable") 
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
      dplyr::filter(person_id == input$selectedPatient)
    
    ggplot2::ggplot(patientData, ggplot2::aes(x = condition_start_date, y = concept_name)) +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = paste("Condition timeline for Patient", input$selectedPatient),
        x = "Date",
        y = "Condition"
      ) +
      ggplot2::theme_minimal()
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