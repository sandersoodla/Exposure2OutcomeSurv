#' Fetch Base Data for Survival Analysis
#'
#' Fetches initial demographic, observation period, and condition occurrence data
#' required for the survival analysis workflow.
#'
#' @param cdm A CDM reference object created by `CDMConnector::cdmFromCon`.
#' @param allConceptIdsToFetch A numeric vector of all exposure and outcome
#'   concept IDs for which to fetch first occurrence dates.
#' @param session Optional. The Shiny session object. If provided, notifications
#'   will be shown in the Shiny UI. Otherwise, messages are printed to the console.
#'
#' @return A list containing:
#'   \describe{
#'     \item{demographics}{A tibble with `person_id`, `gender_concept_id`, `year_of_birth` for persons with non-missing gender and birth year.}
#'     \item{obsPeriods}{A tibble with `person_id`, `observation_period_start_date`, `observation_period_end_date`.}
#'     \item{conditionDates}{A tibble with `person_id`, `concept_id`, `condition_start_date` (first occurrence) for the requested concept IDs.}
#'   }
#' Returns `NULL` for list elements if data fetching fails.
#' 
#' @keywords internal
fetchDataForSurvAnalysis <- function(cdm, allConceptIdsToFetch, session = NULL) {
  
  .notifyUser("Fetching base data (demographics, observation periods, conditions)...", duration = 5, type = "message", id = "fetch_base", session = session)
  on.exit(.removeUserNotify(id = "fetch_base", session = session), add = TRUE)
  
  results <- list(demographics = NULL, obsPeriods = NULL, conditionDates = NULL)
  
  # Fetch demographic data
  results$demographics <- tryCatch({
    getAllGendersAndBirthyears(cdm) %>%
      dplyr::filter(!is.na(gender_concept_id) & !is.na(year_of_birth))
  }, error = function(e) {
    .notifyUser(paste("Error fetching demographics:", e$message), type = "error", session = session)
    return(NULL)
  })
  if (is.null(results$demographics) || nrow(results$demographics) == 0) {
    .notifyUser("No valid demographic data found.",
                     type="warning", duration=10, session=session)
    return(results)
  }
  
  
  basePersonIds <- results$demographics$person_id
  
  # Fetch observation periods for the base cohort
  results$obsPeriods <- tryCatch({
    getObservationPeriods(cdm, personIds = basePersonIds)
    # column names: person_id, obs_start_date, obs_end_date
  }, error = function(e) {
    .notifyUser(paste("Error fetching observation periods:", e$message), type = "error", session = session)
    return(NULL)
  })
  if (is.null(results$obsPeriods) || nrow(results$obsPeriods) == 0) {
    .notifyUser("No observation periods found for initial cohort.",
                     type="warning", duration=10, session=session)
    return(results)
  }
  
  
  # Fetch all relevant condition dates (exposure & outcome) for the base cohort
  results$conditionDates <- tryCatch({
    getFirstConditionDatesForPersons(cdm, personIds = basePersonIds, conceptIds = allConceptIdsToFetch) %>%
      # column names: person_id, concept_id, condition_start_date
      # Remove rows where min(NA) resulted in Inf (no date found)
      dplyr::filter(!is.infinite(condition_start_date))
  }, error = function(e) {
    .notifyUser(paste("Error fetching condition dates:", e$message), type = "error", session = session)
    return(NULL)
  })
  # Check if any condition dates were found
  if (is.null(results$conditionDates) || nrow(results$conditionDates) == 0) {
    .notifyUser("No condition dates found for the selected concepts.", type = "warning", session = session)
    return(results)
  }
  
  
  .notifyUser("Base data fetched.", duration = 2, type="message", session = session)
  return(results)
}




#' Filter Cohort by Outcome Washout and Get Corresponding Outcome Dates
#'
#' Filters a base cohort based on a washout period for a specific outcome ID.
#' It also extracts and returns a table containing the first occurrence dates
#' of that specific outcome.
#'
#' Individuals are kept in the filtered cohort if they either do not have the
#' outcome or if their first occurrence is on or after the required washout
#' period end date (relative to their observation start date).
#'
#' @param cohortBase A tibble representing the base cohort, containing demographics and
#' observation period data from `fetchDataForSurvAnalysis`, joined together by person_id.
#' @param allConditionDates A tibble containing first occurrence dates for
#'   all relevant concept IDs (`person_id`, `concept_id`, `condition_start_date`).
#'   Typically obtained from `fetchDataForSurvAnalysis`.
#' @param outcomeId The numeric concept ID of the outcome condition for which
#'   to apply washout and extract dates.
#' @param washoutYears The minimum number of years for the washout period.
#' @param session Optional. The Shiny session object.
#'
#' @return A list containing:
#'   \describe{
#'     \item{cohortEligible}{A tibble containing the subset of `cohortBase` that meets the washout criteria for the specified `outcomeId`.}
#'     \item{outcomeDates}{A tibble with `person_id` and `outcome_date` (first occurrence) specifically for the provided `outcomeId`.}
#'   }
#' Returns an empty tibble for `cohortEligible` if no persons remain after washout.
#'
#' @keywords internal
filterByWashoutAndGetOutcomeDates <- function(cohortBase, allConditionDates, outcomeId, washoutYears, session = NULL) {
  
  if (is.null(cohortBase) || nrow(cohortBase) == 0) {
    return(list(cohortEligible = tibble::tibble(), outcomeDates = tibble::tibble()))
  }
  if (is.null(allConditionDates)) {
    # Handle case where condition dates might not have been fetched successfully
    allConditionDates <- tibble::tibble(person_id=integer(), concept_id=integer(), condition_start_date=as.Date(character()))
  }
  
  
  # Get dates for the current outcome from the condition first dates
  peopleInBase <- cohortBase$person_id
  outcomeDatesCurrentOutcome <- allConditionDates %>%
    dplyr::filter(person_id %in% peopleInBase) %>%
    dplyr::filter(concept_id == outcomeId) %>%
    dplyr::select(person_id, outcome_date = condition_start_date)
  
  # Identify persons eligible for this outcome based on washout logic
  # Join outcome dates to the base cohort's obs start dates
  eligiblePersonsIds <- cohortBase %>%
    dplyr::select(person_id, obs_start_date) %>%
    dplyr::left_join(outcomeDatesCurrentOutcome, by = "person_id") %>%
    # Keep if outcome is NA OR occurs ON or AFTER washout period end date
    dplyr::filter(is.na(outcome_date) | outcome_date >= (obs_start_date %m+% lubridate::years(washoutYears))) %>%
    dplyr::pull(person_id) %>%
    unique()
  
  if (length(eligiblePersonsIds) == 0) {
    outcomeIdStr = as.character(outcomeId)
    .notifyUser(paste("No persons eligible for Outcome ID:", outcomeIdStr, "after applying", washoutYears, "-year washout."),
                     type = "warning", duration = 5)
    return(list(cohortEligible = tibble::tibble(), outcomeDates = outcomeDatesCurrentOutcome))
  }
  
  # Filter the base cohort to only eligible persons for this outcome
  cohortBaseForOutcome <- cohortBase %>%
    dplyr::filter(person_id %in% eligiblePersonsIds)
  
  # Return the filtered cohort and the outcome dates table (needed later)
  return(list(cohortEligible = cohortBaseForOutcome, outcomeDates = outcomeDatesCurrentOutcome))
}




#' Define Exposed Cohort for a Specific Exposure-Outcome Pair
#'
#' Identifies individuals exposed to a specific condition (`exposureId`) from a
#' base cohort (already filtered by outcome washout), ensuring their exposure
#' occurs AFTER the specified washout period relative to observation start,
#' and BEFORE any occurrence of the specified outcome condition (`outcomeId`).
#' Returns the filtered exposed cohort table and their exposure dates.
#'
#' @param cohortBaseForOutcome A tibble representing the cohort that has already
#'   passed the outcome-specific washout, as returned by `filterByWashoutAndGetOutcomeDates`.
#' @param allConditionFirstDates A tibble containing first occurrence dates for
#'   all relevant concept IDs, as returned by `fetchDataForSurvAnalysis`.
#' @param outcomeDatesCurrentOutcome A tibble with `person_id` and `outcome_date`
#'   for the specific outcome being analyzed in the current loop iteration.
#' @param exposureId The numeric concept ID of the exposure condition.
#' @param outcomeId The numeric concept ID of the outcome condition.
#' @param washoutYears The washout period in years. Exposure must occur on or after
#'   `obs_start_date + washoutYears`.
#' @param session Optional. The Shiny session object.
#'
#' @return A list containing:
#'   \describe{
#'     \item{exposed}{A tibble defining the exposed cohort for this pair. Contains unique exposure events (`exposed_person_id`, `index_date`, `year_of_birth`, `gender_concept_id`) meeting the criteria.}
#'     \item{exposureDates}{A tibble with `person_id` and `exposure_date` (first occurrence) for the specific `exposureId` among the `cohortBaseForOutcome`.}
#'   }
#' Returns `NULL` for `exposed` if no valid exposed individuals are found or
#' `NULL`for both `exposed` and `exposureDates` if `cohortBaseForOutcome` is null or empty
#'
#' @keywords internal
defineExposedCohortForPair <- function(cohortBaseForOutcome,
                                       allConditionFirstDates,
                                       outcomeDatesCurrentOutcome,
                                       exposureId,
                                       outcomeId,
                                       washoutYears,
                                       session = NULL) {
  
  if (is.null(cohortBaseForOutcome) || nrow(cohortBaseForOutcome) == 0) {
    return(list(exposed = NULL, exposureDates = NULL))
    
  }
  exposureIdStr <- as.character(exposureId) # For messages
  outcomeIdStr <- as.character(outcomeId)
  
  # Find first exposure date among persons eligible for this outcome
  exposureDatesCurrentExposure <- allConditionFirstDates %>%
    dplyr::filter(person_id %in% cohortBaseForOutcome$person_id, # Filter by eligible persons
                  concept_id == exposureId) %>%
    dplyr::select(person_id, exposure_date = condition_start_date)
  
  # Identify exposed individuals from the outcome-specific eligible cohort
  exposedCohortDefinition <- cohortBaseForOutcome %>%
    dplyr::inner_join(exposureDatesCurrentExposure, by = "person_id") %>%
    # Apply exposure washout
    dplyr::mutate(washout_end_date = obs_start_date %m+% lubridate::years(washoutYears)) %>%
    dplyr::filter(exposure_date >= washout_end_date) %>%
  
    dplyr::left_join(outcomeDatesCurrentOutcome, by = "person_id") %>% # Join specific outcome dates
    dplyr::filter(!is.na(exposure_date)) %>%
    # Keep only the observation period where the exposure happened
    dplyr::filter(exposure_date >= obs_start_date & exposure_date <= obs_end_date) %>%
    # Keep person only if outcome date is missing OR occurs strictly AFTER exposure date
    dplyr::filter(is.na(outcome_date) | outcome_date > exposure_date) %>%
    # Select necessary info, define index date as exposure date
    dplyr::select(exposed_person_id = person_id,
                  index_date = exposure_date,
                  year_of_birth,
                  gender_concept_id) %>%
    dplyr::distinct(exposed_person_id, index_date, .keep_all = TRUE) # Ensure unique exposure events
  
  if (nrow(exposedCohortDefinition) == 0) {
    .notifyUser(paste("No valid exposed individuals found for Exposure ID:", exposureIdStr, "(eligible for Outcome", outcomeIdStr, "and outcome-free prior to exposure)"),
                     type = "warning", duration = 6, session = session)
    return(list(exposed = NULL, exposureDates = exposureDatesCurrentExposure))
  }
  
  return(list(exposed = exposedCohortDefinition, exposureDates = exposureDatesCurrentExposure))
}




#' Perform Incidence Density Matching for an Exposure-Outcome Pair
#'
#' Matches unexposed controls to exposed individuals based on incidence density
#' sampling principles. Matching is performed on gender (exact) and age
#' (nearest neighbor based on year of birth). Controls are selected from the
#' risk set at the exposed individual's index date.
#' 
#' Control Eligibility Criteria applied within this function:
#' 1. Must be under observation at the case's `index_date`.
#' 2. Must have at least `washoutYears` of observation time *prior* to the case's `index_date`.
#' 3. Must be free of the specific exposure event *prior to or on* the case's `index_date`.
#' 4. Must be free of the specific outcome event *prior to or on* the case's `index_date`.
#'
#' @param exposedCohort A tibble of exposed individuals eligible for
#'   matching, as returned by `defineExposedCohortForPair`. Must contain
#'   `exposed_person_id`, `index_date`, `year_of_birth`, `gender_concept_id`.
#' @param controlsPool A tibble representing the potential control
#'   pool (passed outcome washout). Required columns: `person_id`,
#'   `year_of_birth`, `gender_concept_id`, `obs_start_date`, `obs_end_date`,
#'   `outcome_date` (can be NA), `exposure_date` (can be NA).
#' @param matchRatio The target number of controls to match to each exposed person.
#' @param exposureId The numeric concept ID of the exposure (for notifications).
#' @param outcomeId The numeric concept ID of the outcome (for notifications).
#' @param washoutYears The washout period in years, used to ensure unexposed controls have
#'   a sufficient observation window prior to the exposed's `index_date`.
#' @param session Optional. The Shiny session object.
#'
#' @return A list containing:
#'   \describe{
#'     \item{matchedData}{A tibble containing the matched sets. Includes `person_id`, `exposure_status` (1 for exposed, 0 for control), `index_date` (from the matched exposed person), and `set_id` (linking members of a matched set).}
#'     \item{nMatchedExposed}{The number of exposed individuals for whom at least one control was successfully matched.}
#'   }
#' Returns `NULL` for `matchedData` if no matched sets could be created.
#'
#' @keywords internal
performPairMatching <- function(exposedCohort,
                                controlsPool,
                                matchRatio,
                                exposureId,
                                outcomeId,
                                washoutYears,
                                session = NULL) {
  
  if (is.null(exposedCohort) || nrow(exposedCohort) == 0) {
    return(list(matchedData = NULL, nMatchedExposed = 0))
  }
  
  uniquePairKey <- paste0("pair_", exposureId, "_", outcomeId)
  matchProgId <- paste0("match_prog_", uniquePairKey)
  
  matchedSetsList <- list()
  nExposedMatched <- 0
  totalExposedToMatch <- nrow(exposedCohort)
  
  .notifyUser(
    paste0("Starting matching for Exp:", exposureId, ", Out:", outcomeId, " (", totalExposedToMatch, " exposed)"),
    duration = NA, id = matchProgId, type = "message", session = session
  )
  on.exit(.removeUserNotify(id = matchProgId, session = session), add = TRUE)
  
  for (i in 1:totalExposedToMatch) {
    currentExposed <- exposedCohort[i, ]
    exposedPersonId <- currentExposed$exposed_person_id
    exposureIndexDate <- currentExposed$index_date
    exposedBirthYear <- currentExposed$year_of_birth
    exposedGender <- currentExposed$gender_concept_id
    
    # Define Risk Set for this Exposed Person
    riskSet <- controlsPool %>%
      dplyr::filter(
        person_id != exposedPersonId,
        # CRITERION 1: Control must be under observation on exposed's exposure date
        obs_start_date <= exposureIndexDate,
        obs_end_date >= exposureIndexDate,
        # CRITERION 2: Control must have `washoutYears` of observation before exposed case's index_date
        (obs_start_date %m+% lubridate::years(washoutYears)) <= exposureIndexDate,
        
        # CRITERION 3: Control is UNEXPOSED to this specific exposure AT case's index_date
        is.na(exposure_date) | exposure_date > exposureIndexDate,
        
        # CRITERION 4: Control is FREE of the current main outcome AT case's index_date
        is.na(outcome_date) | outcome_date > exposureIndexDate
      )
    
    matchedControlsDf <- NULL
    if (nrow(riskSet) > 0) {
      # Perform matching (Nearest Neighbor on age, exact on gender)
      matchedControlsDf <- riskSet %>%
        dplyr::filter(gender_concept_id == exposedGender) %>%
        dplyr::mutate(age_diff = abs(year_of_birth - exposedBirthYear)) %>%
        dplyr::arrange(age_diff) %>%
        dplyr::slice_head(n = matchRatio)
    }
    
    # Store matched set if controls were found
    if (!is.null(matchedControlsDf) && nrow(matchedControlsDf) > 0) {
      nExposedMatched <- nExposedMatched + 1
      setData <- tibble::tibble(
        person_id = c(exposedPersonId, matchedControlsDf$person_id),
        exposure_status = c(1, rep(0, nrow(matchedControlsDf))),
        index_date = exposureIndexDate,
        set_id = i # Unique set identifier linking exposed to their controls
      )
      matchedSetsList[[length(matchedSetsList) + 1]] <- setData # Append
    }
    
    # Progress update
    if (i %% 50 == 0 || i == totalExposedToMatch) {
      currentProgress <- round((i / totalExposedToMatch) * 100)
      .notifyUser(
        paste0("Matching for Exp:", exposureId, ", Out:", outcomeId, " (", currentProgress,"%)"),
        duration = NA, id = matchProgId, type = "message", session = session # Update existing notification
      )
    }
  } # End exposed person matching loop
  
  if (length(matchedSetsList) == 0) {
    errMsg <- paste("No matched sets created for Exposure:", exposureId, "Outcome:", outcomeId)
    .notifyUser(errMsg, type = "warning", duration = 5, session = session)
    return(list(matchedData = NULL, nMatchedExposed = 0))
  }
  
  # Combine list of tibbles into one
  matchedDataFinal <- dplyr::bind_rows(matchedSetsList)
  
  return(list(matchedData = matchedDataFinal, nMatchedExposed = nExposedMatched))
}




#' Calculate Survival Time and Status for a Matched Pair
#'
#' Calculates the time-to-event (outcome) and event status for each person
#' in the matched cohort data. Time is calculated from the index date. Censoring
#' occurs at the outcome date or the end of the observation period, whichever
#' comes first after the index date.
#'
#' @param matchedDataFinal A tibble containing the matched sets data, as
#'   returned by `performPairMatching`. Must include `person_id`, `set_id`,
#'   `exposure_status`, `index_date`.
#' @param cohortBaseForOutcome A tibble representing the cohort that passed
#'   outcome washout, used to retrieve observation periods (`obs_start_date`,
#'   `obs_end_date`) for the matched individuals.
#' @param outcomeDatesCurrentOutcome A tibble containing `person_id` and
#'   `outcome_date` for the specific outcome being analyzed.
#' @param session Optional. The Shiny session object.
#'
#' @return A tibble ready for survival analysis. Includes `person_id`, `set_id`,
#'   `exposure_status`, `index_date`, `study_exit_date`, `time_to_outcome`
#'   (in days), and `outcome_status` (1 if event occurred, 0 if censored). 
#'   Returns `NULL` if no valid survival data rows are generated.
#'
#' @keywords internal
calculatePairSurvival <- function(matchedDataFinal,
                                  cohortBaseForOutcome,
                                  outcomeDatesCurrentOutcome,
                                  session = NULL) {
  
  if (is.null(matchedDataFinal) || nrow(matchedDataFinal) == 0) {
    return(NULL)
  }
  
  # Prepare base for survival calculation (matched individuals + obs periods)
  survivalInputBase <- matchedDataFinal %>%
    dplyr::select(person_id, set_id, exposure_status, index_date) %>%
    dplyr::inner_join(dplyr::select(cohortBaseForOutcome, person_id, obs_start_date, obs_end_date), by = "person_id") %>%
    dplyr::distinct() # Ensure distinct person-set combinations
  
  # Calculate time-to-outcome and status
  survivalDataCurrentPair <- survivalInputBase %>%
    dplyr::left_join(outcomeDatesCurrentOutcome, by = "person_id") %>% # Join the specific outcome dates
    dplyr::mutate(
      # Event happens if outcome date exists, is AFTER index, and ON/BEFORE obs_end
      outcome_event_date = dplyr::if_else(!is.na(outcome_date) & outcome_date > index_date & outcome_date <= obs_end_date,
                                          outcome_date, lubridate::NA_Date_),
      # Exit is the earliest of obs_end or the outcome event date (if it occurred)
      study_exit_date = pmin(obs_end_date, outcome_event_date, na.rm = TRUE),
      # Status is 1 if the exit date is the outcome date (i.e., event wasn't censored by obs_end)
      outcome_status = dplyr::if_else(!is.na(outcome_event_date) & outcome_event_date == study_exit_date, 1, 0),
      # Time is difference between exit and index
      time_to_outcome = as.numeric(difftime(study_exit_date, index_date, units = "days"))
    ) %>%
    # Keep only valid time entries (time must be non-negative)
    dplyr::filter(time_to_outcome >= 0) %>%
    dplyr::select(person_id, set_id, exposure_status, index_date, study_exit_date, time_to_outcome, outcome_status)
  
  if (nrow(survivalDataCurrentPair) == 0) {
    errMsg <- "No valid survival data after calculating time and status."
    .notifyUser(errMsg, type="warning", duration=5, session=session)
    return(NULL)
  }
  
  return(survivalDataCurrentPair)
}