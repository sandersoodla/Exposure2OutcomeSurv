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
      dplyr::mutate(group = factor(exposure_status,
                                   levels = c(0, 1),
                                   labels = c("Unexposed (Matched)", "Exposed")))
    
    # Fit a KM model using the group variable
    kmFit <- survival::survfit(survival::Surv(time_to_outcome, outcome_status) ~ group, data = survData)
    
    # Find minimum survival for the graph y-axis limit
    # Get summary at the max time
    endSummary <- summary(kmFit, times = maxPlotTime)
    
    # Find smallest survival probability among groups at the end time
    minEndProb <- min(endSummary$surv, na.rm = TRUE)
    
    # Set y-axis lower limit slightly below minimum
    lowerYLim <- max(0, minEndProb - 0.02)
    
    dynamicYLim <- c(lowerYLim, 1.0)
    
    # Calculate the p-value for storing it in the summary table
    pValueInfo <- survminer::surv_pvalue(kmFit, data = survData)
    
    # Construct the plot title using labels from the results list
    pairPlotTitle <- paste0("Exposure: ", pairResult$exposureLabel,
                            " | Outcome: ", pairResult$outcomeLabel)
    
    ggsurvObject <- survminer::ggsurvplot(
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
      break.time.by = 400,
      tables.y.text = FALSE
    ) 
    
    ggsurvObject$plot <- ggsurvObject$plot + ggplot2::theme(legend.text = ggplot2::element_text(size = 14),
                                                            legend.title = ggplot2::element_text(size = 14))
    
    # Store the entire ggsurvplot list object (plot + table)
    # and the p-value
    if (!is.null(ggsurvObject)) {
      plotsList[[pairKey]] <- list(
        plotObject = ggsurvObject,
        kmPValueInfo = pValueInfo
      )
    }
  }
  plotsList
  
}



calculateCoxResults <- function(matchedSurvivalData) {
  req(matchedSurvivalData)
  
  coxResultsList <- list()
  
  for (pairKey in names(matchedSurvivalData)) {
    
    pairData <- matchedSurvivalData[[pairKey]]
    currentSurvData <- pairData$survivalData
    
    # Initialize results for this pair
    coxRes <- list(hazardRatio = NA_real_, hrCiLower = NA_real_, hrCiUpper = NA_real_, hrPvalue = NA_real_, coxError = NA_character_)
    
    # Fit Cox model with clustering on sets
    coxFit <- survival::coxph(survival::Surv(time_to_outcome, outcome_status) ~ exposure_status + cluster(set_id),
                              data = currentSurvData, robust = TRUE)
    
    summaryCox <- summary(coxFit)
    
    coxRes$hazardRatio <- summaryCox$conf.int[,"exp(coef)"]
    coxRes$hrCiLower <- summaryCox$conf.int[,"lower .95"]
    coxRes$hrCiUpper <- summaryCox$conf.int[,"upper .95"]
    coxRes$hrPvalue <- summaryCox$waldtest["pvalue"]
    
    coxResultsList[[pairKey]] <- coxRes
  }
  return(coxResultsList)
}
