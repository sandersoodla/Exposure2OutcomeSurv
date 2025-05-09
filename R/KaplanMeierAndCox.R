#' Generate Kaplan-Meier Plot Objects for Matched Pairs
#'
#' Creates Kaplan-Meier plots (with risk tables) using `survminer` and calculates
#' log-rank p-values for each exposure-outcome pair present in the matched
#' survival data. Adjusts plot limits dynamically.
#'
#' @param matchedSurvivalData A list structured generally like the output of
#'   `calculateMatchedSurvivalData`. Each named element corresponds to an
#'   exposure-outcome pair and is itself a list that must contain at least:
#'   `$survivalData` (a tibble/data frame with columns `time_to_outcome`,
#'   `outcome_status`, `exposure_status`) and descriptive character labels
#'   `$exposureLabel`, `$outcomeLabel`.
#' @param maxPlotTime Numeric. The maximum time (in days, starting from index date)
#'   to display on the x-axis of the Kaplan-Meier plots. Defaults to 1825 (~5 years).
#' @param session Optional. The Shiny session object. If provided, notifications
#'   will be shown in the Shiny UI. Otherwise, messages are printed to the console.
#'
#' @return A list named by the unique exposure-outcome pair keys present in
#'   `matchedSurvivalData`. Each element is a list containing:
#'   \describe{
#'     \item{`plotObject`}{The list object returned by `survminer::ggsurvplot`. This itself contains elements like `$plot` (the ggplot KM curve) and `$table` (the risk table plot).}
#'     \item{`kmPValueInfo`}{The object returned by `survminer::surv_pvalue`, containing details about the log-rank test p-value.}
#'   }
#'   Returns an empty list if `matchedSurvivalData` is NULL or empty.
#' 
#' @keywords internal
generateKmPlotObjects <- function(matchedSurvivalData, maxPlotTime = 1825, session = NULL) {
  if (is.null(matchedSurvivalData)) return(list())
  
  numPairs <- length(matchedSurvivalData)
  
  # Show initial notification for plot generation
  notificationId <- "km_plot_progress" # Unique ID for this notification
  .notifyUser(
    paste("Starting KM plot generation for", numPairs, "pairs..."), 
    duration = NA, # Keep open until removed
    id = notificationId, 
    type = "message",
    session = session
  )
  # Ensure notification is removed when function exits (even on error)
  on.exit(.removeUserNotify(id = notificationId, session), add = TRUE) 
  
  # Create an empty list to store plots and tables. Use names that indicate the pair.
  plotsList <- list()
  plotCounter <- 0
  
  # Loop through each exposure-outcome pair result stored in the input list
  for (pairKey in names(matchedSurvivalData)) {
    plotCounter <- plotCounter + 1 # Increment counter
    
    # Update progress notification
    .notifyUser(
      paste0("Generating plot ", plotCounter, " of ", numPairs, ": ", pairKey), 
      duration = NA, 
      id = notificationId,
      type = "message",
      session = session
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
    endSummary <- summary(kmFit, times = maxPlotTime, extend = TRUE)
    
    # Find smallest survival probability among groups at the end time
    minEndProb <- min(endSummary$surv, na.rm = TRUE)
    
    # Set y-axis lower limit slightly below minimum
    lowerYLim <- max(0, minEndProb - 0.02)
    
    dynamicYLim <- c(lowerYLim, 1.0)
    
    # --- Check for zero outcome events before calculating p-value ---
    eventCounts <- survData %>%
      dplyr::filter(outcome_status == 1) %>%
      dplyr::group_by(exposure_status) %>%
      dplyr::summarise(events = dplyr::n(), .groups = "drop")
    
    exposedEvents <- eventCounts$events[eventCounts$exposure_status == 1]
    unexposedEvents <- eventCounts$events[eventCounts$exposure_status == 0]
    
    if (length(exposedEvents) == 0) exposedEvents <- 0
    if (length(unexposedEvents) == 0) unexposedEvents <- 0
    
    if (exposedEvents == 0 && unexposedEvents == 0) {
      pValueInfo <- list(pval = NA_real_, pval.txt = "NE (zero events)")
    } else {
      # Calculate the p-value for storing it in the summary table
      pValueInfo <- survminer::surv_pvalue(kmFit, data = survData)
    }
    
    
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
      break.time.by = 365,
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


#' Calculate Cox Model Results for Matched Pairs
#'
#' Fits a Cox Proportional Hazards model for each exposure-outcome pair,
#' comparing exposed (status=1) vs. unexposed (status=0) groups. Accounts for the
#' matched set structure using `cluster(set_id)` to compute robust variance
#' estimates for the hazard ratio.
#'
#' @param matchedSurvivalData A list structured generally like the output of
#'   `calculateMatchedSurvivalData`. Each named element corresponds to an
#'   exposure-outcome pair and must contain `$survivalData` (a tibble/data frame
#'   with columns `time_to_outcome`, `outcome_status`, `exposure_status`, `set_id`).
#'
#' @return A list named by the unique exposure-outcome pair keys present in
#'   `matchedSurvivalData`. Each element is a list containing the Cox model results:
#'   \describe{
#'     \item{`hazardRatio`}{Numeric estimate of the Hazard Ratio (Exposed vs. Unexposed).}
#'     \item{`hrCiLower`}{Numeric lower bound of the 95% Confidence Interval for the HR.}
#'     \item{`hrCiUpper`}{Numeric upper bound of the 95% Confidence Interval for the HR.}
#'     \item{`hrPvalue`}{Numeric p-value associated with the Hazard Ratio (from Wald test).}
#'   }
#'   Results will be `NA` if the model fails to converge or data is insufficient.
#'
#' @keywords internal
calculateCoxResults <- function(matchedSurvivalData) {
  if (is.null(matchedSurvivalData)) return(list())
  
  coxResultsList <- list()
  
  for (pairKey in names(matchedSurvivalData)) {
    
    pairData <- matchedSurvivalData[[pairKey]]
    currentSurvData <- pairData$survivalData
    
    # Initialize results for this pair
    coxRes <- list(hazardRatio = NA_real_, hrCiLower = NA_real_, hrCiUpper = NA_real_, hrPvalue = NA_real_)
    
    # Check for zero outcome events in one or both groups
    eventCounts <- currentSurvData %>%
      dplyr::filter(outcome_status == 1) %>%
      dplyr::group_by(exposure_status) %>%
      dplyr::summarise(events = dplyr::n(), .groups = "drop")
    
    exposedEvents <- eventCounts$events[eventCounts$exposure_status == 1]
    unexposedEvents <- eventCounts$events[eventCounts$exposure_status == 0]
    
    if (length(exposedEvents) == 0) exposedEvents <- 0
    if (length(unexposedEvents) == 0) unexposedEvents <- 0
    
    if (exposedEvents == 0 && unexposedEvents == 0) {
      coxResultsList[[pairKey]] <- coxRes
      next
    } else if (exposedEvents == 0) {
      # HR would be 0, p-value often 1 or NA. Not very informative.
      coxResultsList[[pairKey]] <- coxRes
      next
    } else if (unexposedEvents == 0) {
      # HR would be Inf, p-value often 1 or NA. Not very informative.
      coxResultsList[[pairKey]] <- coxRes
      next
    }
    
    # Fit Cox model with clustering on sets
    coxFit <- survival::coxph(survival::Surv(time_to_outcome, outcome_status) ~ exposure_status + cluster(set_id),
                              data = currentSurvData)
    
    summaryCox <- summary(coxFit)
    
    coxRes$hazardRatio <- summaryCox$conf.int[,"exp(coef)"]
    coxRes$hrCiLower <- summaryCox$conf.int[,"lower .95"]
    coxRes$hrCiUpper <- summaryCox$conf.int[,"upper .95"]
    coxRes$hrPvalue <- summaryCox$waldtest["pvalue"]
    
    coxResultsList[[pairKey]] <- coxRes
  }
  return(coxResultsList)
}
