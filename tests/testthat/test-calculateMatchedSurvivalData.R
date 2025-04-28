# --- Mock Setup ---

mock_notifyUser <- function(...) { invisible(NULL) }
mock_removeUserNotify <- function(...) { invisible(NULL) }

# Define mocks for the core helper functions
# These mocks will be overridden within specific tests as needed
mock_fetchDataForSurvAnalysis <- function(cdm, conceptIds, session = NULL) {
  # Default happy path: returns some basic data structure
  list(
    demographics = tibble::tibble(person_id = 1:10, gender_concept_id = 8507, year_of_birth = 1980),
    obsPeriods = tibble::tibble(person_id = 1:10, obs_start_date = as.Date("2000-01-01"), obs_end_date = as.Date("2010-12-31")),
    conditionDates = tibble::tribble(
      ~person_id, ~concept_id, ~condition_start_date,
      # Exposures (E1=101, E2=102)
      1, 101, as.Date("2005-01-01"), # E1 P1
      2, 101, as.Date("2006-01-01"), # E1 P2
      6, 101, as.Date("2003-01-01"), # E1 P6
      3, 102, as.Date("2007-01-01"), # E2 P3
      7, 102, as.Date("2008-01-01"), # E2 P7
      # Outcomes (O1=201, O2=202)
      1, 201, as.Date("2008-01-01"), # O1 P1 (after washout if washout=2y)
      2, 201, as.Date("2001-01-01"), # O1 P2 (before washout if washout=2y) -> P2 ineligible for O1
      3, 201, as.Date("2009-01-01"), # O1 P3 (after washout)
      4, 201, as.Date("2004-01-01"), # O1 P4 (after washout)
      5, 201, NA,                  # O1 P5 (no outcome) -> Eligible
      6, 201, as.Date("2007-05-01"), # O1 P6 (after washout)
      # --- For O2 ---
      7, 202, as.Date("2009-05-01"), # O2 P7 (after washout)
      8, 202, as.Date("2001-05-01"), # O2 P8 (before washout) -> P8 ineligible for O2
      9, 202, NA                   # O2 P9 (no outcome) -> Eligible
    )
  )
}

mock_filterByWashoutAndGetOutcomeDates <- function(cohortBase, allConditionDates, outcomeId, washoutYears, session = NULL) {
  # Default: Assume outcome 201, washout 2 years
  # P2 had outcome before washout, P1,3,4,5,6,7,8,9,10 eligible based on washout
  eligible_ids_o1 <- c(1, 3:10)
  outcome_dates_o1 <- allConditionDates %>% dplyr::filter(concept_id == 201) %>% dplyr::select(person_id, outcome_date = condition_start_date)
  
  # Assume outcome 202, washout 2 years
  eligible_ids_o2 <- c(1:7, 9, 10) # P8 ineligible
  outcome_dates_o2 <- allConditionDates %>% dplyr::filter(concept_id == 202) %>% dplyr::select(person_id, outcome_date = condition_start_date)
  
  if(outcomeId == 201) {
    eligible_cohort <- cohortBase %>% dplyr::filter(person_id %in% eligible_ids_o1)
    outcome_dates <- outcome_dates_o1
  } else if (outcomeId == 202) {
    eligible_cohort <- cohortBase %>% dplyr::filter(person_id %in% eligible_ids_o2)
    outcome_dates <- outcome_dates_o2
  } else { # Default fallback or error simulation
    eligible_cohort <- cohortBase # Assume no washout if not 201/202
    outcome_dates <- tibble::tibble(person_id = integer(), outcome_date=as.Date(character()))
  }
  
  list(
    cohortEligible = eligible_cohort,
    outcomeDates = outcome_dates
  )
}

mock_defineExposedCohortForPair <- function(cohortBaseForOutcome, allConditionFirstDates, outcomeDatesCurrentOutcome, exposureId, outcomeId, session = NULL) {
  # Default: Define based on E1=101, O1=201
  # Assuming cohortBaseForOutcome has IDs 1, 3:10
  # Exposed to E1: P1(2005), P2(2006 - not in base), P6(2003)
  # P1 outcome O1 at 2008, P6 outcome O1 at 2007. Exposure before outcome for both.
  exposed_ids_e1 <- c(1, 6)
  exposed_dates_e1 <- allConditionFirstDates %>% dplyr::filter(concept_id == 101) %>% dplyr::select(person_id, exposure_date = condition_start_date)
  
  # Exposed to E2=102: P3(2007), P7(2008)
  # P3 outcome O1 at 2009, P7 outcome O2 at 2009. Exposure before outcome for both.
  exposed_ids_e2 <- c(3, 7)
  exposed_dates_e2 <- allConditionFirstDates %>% dplyr::filter(concept_id == 102) %>% dplyr::select(person_id, exposure_date = condition_start_date)
  
  
  if(exposureId == 101) {
    exposed_ids <- exposed_ids_e1
    exposure_dates <- exposed_dates_e1
  } else if(exposureId == 102) {
    exposed_ids <- exposed_ids_e2
    exposure_dates <- exposed_dates_e2
  } else {
    exposed_ids <- integer()
    exposure_dates <- tibble::tibble(person_id = integer(), exposure_date = as.Date(character()))
  }
  
  # Filter exposed based on who is actually IN the current cohortBaseForOutcome
  valid_exposed_ids <- intersect(exposed_ids, cohortBaseForOutcome$person_id)
  exposed_def <- cohortBaseForOutcome %>%
    dplyr::filter(person_id %in% valid_exposed_ids) %>%
    dplyr::left_join(exposure_dates, by = "person_id") %>%
    dplyr::select(exposed_person_id = person_id, index_date = exposure_date, year_of_birth, gender_concept_id)
  
  list(
    exposed = exposed_def,
    exposureDates = exposure_dates %>% dplyr::filter(person_id %in% cohortBaseForOutcome$person_id) # Only return dates for people eligible for outcome
  )
}

mock_performPairMatching <- function(exposedCohortDefinition, controlsPoolBaseOutcome, exposureDatesCurrentExposure, matchRatio, exposureId, outcomeId, session = NULL) {
  # Default: Assume we can match P1 to P3, P4; and P6 to P5 for E1/O1
  # Assume we can match P3 to P1, P4; and P7 to P9, P10 for E2/O1 (if P1, P4 in controlsPool)
  # Assume we can match P3 to P1; and P7 to P9 for E2/O2 (if P1, P9 in controlsPool)
  
  n_exposed <- nrow(exposedCohortDefinition)
  if (n_exposed == 0) {
    return(list(matchedData = NULL, nMatchedExposed = 0))
  }
  
  # Simplified matching logic for mocking: create some matched sets
  exposed_ids <- exposedCohortDefinition$exposed_person_id
  matched_sets <- list()
  n_matched_exposed_count <- 0
  
  for(i in 1:n_exposed) {
    exposed_person <- exposedCohortDefinition[i,]
    # Find some arbitrary controls from the pool (ensure they are not the exposed person)
    potential_controls <- controlsPoolBaseOutcome %>% dplyr::filter(person_id != exposed_person$exposed_person_id)
    # Simple mock: grab first few available based on matchRatio, ignore dates/age/gender for mock simplicity
    n_controls_to_get <- min(matchRatio, nrow(potential_controls))
    if (n_controls_to_get > 0) {
      controls <- potential_controls %>% dplyr::slice(1:n_controls_to_get)
      set_data <- dplyr::bind_rows(
        tibble::tibble(person_id = exposed_person$exposed_person_id, exposure_status = 1, index_date = exposed_person$index_date, set_id = i),
        tibble::tibble(person_id = controls$person_id, exposure_status = 0, index_date = exposed_person$index_date, set_id = i)
      )
      matched_sets[[i]] <- set_data
      n_matched_exposed_count <- n_matched_exposed_count + 1
    } # else: exposed person not matched
  }
  
  if (length(matched_sets) > 0) {
    final_matched_data <- dplyr::bind_rows(matched_sets)
    list(matchedData = final_matched_data, nMatchedExposed = n_matched_exposed_count)
  } else {
    list(matchedData = NULL, nMatchedExposed = 0)
  }
}

mock_calculatePairSurvival <- function(matchedDataFinal, cohortBaseForOutcome, outcomeDatesCurrentOutcome, session = NULL) {
  # Default: Add dummy survival columns based on matched data
  if (is.null(matchedDataFinal) || nrow(matchedDataFinal) == 0) return(NULL)
  
  # Join necessary info
  base_info <- cohortBaseForOutcome %>% dplyr::select(person_id, obs_start_date, obs_end_date)
  survival_data <- matchedDataFinal %>%
    dplyr::left_join(base_info, by = "person_id") %>%
    dplyr::left_join(outcomeDatesCurrentOutcome, by = "person_id")
  
  # Simplified survival calculation for mock
  survival_data <- survival_data %>%
    dplyr::mutate(
      outcome_event_date = dplyr::if_else(!is.na(outcome_date) & outcome_date >= index_date & outcome_date <= obs_end_date, outcome_date, as.Date(NA)),
      study_exit_date = pmin(dplyr::coalesce(outcome_event_date, obs_end_date), obs_end_date, na.rm = TRUE),
      outcome_status = dplyr::if_else(!is.na(outcome_event_date) & outcome_event_date == study_exit_date, 1, 0),
      time_to_outcome = as.numeric(study_exit_date - index_date)
    ) %>%
    dplyr::select(person_id, set_id, exposure_status, index_date, study_exit_date, time_to_outcome, outcome_status)
  
  return(survival_data)
}


# --- Test Suite ---

test_that("calculateMatchedSurvivalData input validation works", {
  # Mock necessary objects
  mock_cdm <- list() # Simple mock
  mock_lookup <- tibble::tibble(concept_id = c(101, 201), concept_name_id = c("Exposure A", "Outcome X"))
  
  # Test missing arguments (session is allowed to be missing by default in function signature, test others)
  # expect_error(calculateMatchedSurvivalData(), "Missing required arguments") # Need to adjust function to handle missing better if needed
  expect_error(calculateMatchedSurvivalData(selectedOutcomeIds = 201, cdm = mock_cdm, conceptLabelLookup = mock_lookup), "Missing required arguments")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, cdm = mock_cdm, conceptLabelLookup = mock_lookup), "Missing required arguments")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = 201, conceptLabelLookup = mock_lookup), "Missing required arguments")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = 201, cdm = mock_cdm), "Missing required arguments")
  
  # Test invalid types/values
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = "a", selectedOutcomeIds = 201, cdm = mock_cdm, conceptLabelLookup = mock_lookup), "must be numeric vectors")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = "b", cdm = mock_cdm, conceptLabelLookup = mock_lookup), "must be numeric vectors")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = 201, cdm = mock_cdm, conceptLabelLookup = mock_lookup, matchRatio = 0), "must be a positive integer")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = 201, cdm = mock_cdm, conceptLabelLookup = mock_lookup, washoutYears = -1), "must be a non-negative number")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = 201, cdm = mock_cdm, conceptLabelLookup = tibble::tibble(id = 1, name = "X")), "must be a data frame/tibble")
  expect_error(calculateMatchedSurvivalData(selectedExposureIds = 101, selectedOutcomeIds = 201, cdm = mock_cdm, conceptLabelLookup = tibble::tibble(concept_id = 1, name = "X")), "must be a data frame/tibble")
})


test_that("calculateMatchedSurvivalData happy path (1 Exposure, 1 Outcome)", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = c(101, 201), concept_name_id = c("Exposure A", "Outcome X"))
  exp_id <- 101
  out_id <- 201
  
  # Use default mocks which simulate a successful run
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis,
    filterByWashoutAndGetOutcomeDates = mock_filterByWashoutAndGetOutcomeDates,
    defineExposedCohortForPair = mock_defineExposedCohortForPair,
    performPairMatching = mock_performPairMatching,
    calculatePairSurvival = mock_calculatePairSurvival,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv" # Replace with your actual package name if applicable
  )
  
  results <- calculateMatchedSurvivalData(
    selectedExposureIds = exp_id,
    selectedOutcomeIds = out_id,
    cdm = mock_cdm,
    conceptLabelLookup = mock_lookup,
    matchRatio = 2, # Match ratio used in mock setup
    washoutYears = 2 # Washout used in mock setup
    # session = NULL # Default is NULL
  )
  
  expect_type(results, "list")
  expect_equal(length(results), 1)
  expect_named(results, "pair_101_201")
  
  pair_result <- results$pair_101_201
  expect_type(pair_result, "list")
  expect_named(pair_result, c("survivalData", "exposureId", "outcomeId", "exposureLabel", "outcomeLabel",
                              "nExposedIncludedInMatching", "nExposedMatched", "nUnexposedMatched", "nTotalPersonsInAnalysis"))
  
  expect_s3_class(pair_result$survivalData, "tbl_df")
  # Check counts based on mock logic (E1/O1: Exposed P1, P6 initially)
  expect_gt(nrow(pair_result$survivalData), 0)
  expect_equal(pair_result$exposureId, 101)
  expect_equal(pair_result$outcomeId, 201)
  expect_equal(pair_result$exposureLabel, "Exposure A")
  expect_equal(pair_result$outcomeLabel, "Outcome X")
  expect_equal(pair_result$nExposedIncludedInMatching, 2) # Mock defines P1, P6 as exposed for E1 after washout for O1
  expect_equal(pair_result$nExposedMatched, 2) # Mock matches both P1 and P6
  expect_gt(pair_result$nUnexposedMatched, 0) # Mock adds controls
  expect_equal(pair_result$nTotalPersonsInAnalysis, nrow(pair_result$survivalData))
  expect_gt(pair_result$nTotalPersonsInAnalysis, pair_result$nExposedMatched) # Total includes controls
})

test_that("calculateMatchedSurvivalData handles multiple pairs", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = c(101, 102, 201, 202), concept_name_id = c("Exposure A", "Exposure B", "Outcome X", "Outcome Y"))
  exp_ids <- c(101, 102)
  out_ids <- c(201, 202)
  
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis,
    filterByWashoutAndGetOutcomeDates = mock_filterByWashoutAndGetOutcomeDates,
    defineExposedCohortForPair = mock_defineExposedCohortForPair,
    performPairMatching = mock_performPairMatching,
    calculatePairSurvival = mock_calculatePairSurvival,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(
    selectedExposureIds = exp_ids,
    selectedOutcomeIds = out_ids,
    cdm = mock_cdm,
    conceptLabelLookup = mock_lookup,
    matchRatio = 1,
    washoutYears = 2
  )
  
  expect_type(results, "list")
  expect_equal(length(results), 4) # 2 exposures * 2 outcomes = 4 pairs processed
  expect_named(results, c("pair_101_201", "pair_102_201", "pair_101_202", "pair_102_202"))
  
  # Check one pair as example
  pair_result_102_202 <- results$pair_102_202
  expect_equal(pair_result_102_202$exposureId, 102)
  expect_equal(pair_result_102_202$outcomeId, 202)
  expect_equal(pair_result_102_202$exposureLabel, "Exposure B")
  expect_equal(pair_result_102_202$outcomeLabel, "Outcome Y")
  # Based on mock: E2=102, O2=202. Base cohort after O2 washout excludes P8. Exposed E2 are P3, P7. Both are in base.
  expect_equal(pair_result_102_202$nExposedIncludedInMatching, 2)
  expect_equal(pair_result_102_202$nExposedMatched, 2) # Mock matches all defined exposed
  expect_gt(pair_result_102_202$nUnexposedMatched, 0)
})

test_that("calculateMatchedSurvivalData returns NULL if data fetch fails", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = 1, concept_name_id = "X")
  
  # Mock fetchData to return NULL
  mock_fetch_fail <- function(...) NULL
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetch_fail,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results)
  
  # Mock fetchData to return empty data
  mock_fetch_empty <- function(...) list(demographics=tibble::tibble(), obsPeriods=tibble::tibble(), conditionDates=tibble::tibble())
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetch_empty,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  results_empty <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results_empty)
  
})

test_that("calculateMatchedSurvivalData returns NULL if initial cohort empty", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = 1, concept_name_id = "X")
  
  # Mock fetchData to return data that leads to empty join
  mock_fetch_disjoint <- function(...) {
    list(
      demographics = tibble::tibble(person_id = 1:5, gender_concept_id = 8507, year_of_birth = 1980),
      obsPeriods = tibble::tibble(person_id = 6:10, obs_start_date = as.Date("2000-01-01"), obs_end_date = as.Date("2010-12-31")), # Disjoint IDs
      conditionDates = tibble::tibble(person_id=integer(), concept_id=integer(), condition_start_date=as.Date(character()))
    )
  }
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetch_disjoint,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  results <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results)
})

test_that("calculateMatchedSurvivalData skips outcome if washout removes all", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = c(101, 201), concept_name_id = c("E1", "O1"))
  
  # Mock filterByWashout to return empty cohort
  mock_washout_empty <- function(...) list(cohortEligible = tibble::tibble(), outcomeDates = tibble::tibble())
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis, # Use default fetch
    filterByWashoutAndGetOutcomeDates = mock_washout_empty,
    # Other mocks shouldn't be called if washout fails
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results) # Since only one outcome, and it fails, overall result is NULL
})

test_that("calculateMatchedSurvivalData skips pair if no exposed found", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = c(101, 201), concept_name_id = c("E1", "O1"))
  
  # Mock defineExposed to return empty
  mock_define_empty <- function(...) list(exposed = NULL, exposureDates = tibble::tibble())
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis,
    filterByWashoutAndGetOutcomeDates = mock_filterByWashoutAndGetOutcomeDates, # Default washout ok
    defineExposedCohortForPair = mock_define_empty,
    # Matching etc shouldn't be called
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results) # Only one pair, fails to find exposed -> NULL
})

test_that("calculateMatchedSurvivalData skips pair if matching fails", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = c(101, 201), concept_name_id = c("E1", "O1"))
  
  # Mock matching to return empty
  mock_match_fail <- function(...) list(matchedData = NULL, nMatchedExposed = 0)
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis,
    filterByWashoutAndGetOutcomeDates = mock_filterByWashoutAndGetOutcomeDates,
    defineExposedCohortForPair = mock_defineExposedCohortForPair, # Default define ok
    performPairMatching = mock_match_fail,
    # Survival calc shouldn't be called
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results) # Only one pair, fails matching -> NULL
})


test_that("calculateMatchedSurvivalData skips pair if survival calc fails", {
  mock_cdm <- list()
  mock_lookup <- tibble::tibble(concept_id = c(101, 201), concept_name_id = c("E1", "O1"))
  
  # Mock survival calc to return empty
  mock_survival_fail <- function(...) NULL
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis,
    filterByWashoutAndGetOutcomeDates = mock_filterByWashoutAndGetOutcomeDates,
    defineExposedCohortForPair = mock_defineExposedCohortForPair,
    performPairMatching = mock_performPairMatching, # Default matching ok
    calculatePairSurvival = mock_survival_fail,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(101, 201, mock_cdm, mock_lookup)
  expect_null(results) # Only one pair, fails survival calc -> NULL
})


test_that("calculateMatchedSurvivalData handles missing labels gracefully", {
  mock_cdm <- list()
  # Lookup missing E1 label
  mock_lookup_missing <- tibble::tibble(concept_id = c(201), concept_name_id = c("Outcome X"))
  exp_id <- 101
  out_id <- 201
  
  testthat::local_mocked_bindings(
    fetchDataForSurvAnalysis = mock_fetchDataForSurvAnalysis,
    filterByWashoutAndGetOutcomeDates = mock_filterByWashoutAndGetOutcomeDates,
    defineExposedCohortForPair = mock_defineExposedCohortForPair,
    performPairMatching = mock_performPairMatching,
    calculatePairSurvival = mock_calculatePairSurvival,
    .notifyUser = mock_notifyUser,
    .removeUserNotify = mock_removeUserNotify,
    .package = "Exposure2OutcomeSurv"
  )
  
  results <- calculateMatchedSurvivalData(
    selectedExposureIds = exp_id,
    selectedOutcomeIds = out_id,
    cdm = mock_cdm,
    conceptLabelLookup = mock_lookup_missing, # Use lookup with missing label
    matchRatio = 2,
    washoutYears = 2
  )
  
  expect_type(results, "list")
  expect_equal(length(results), 1)
  pair_result <- results$pair_101_201
  expect_equal(pair_result$exposureLabel, "Exposure ID: 101") # Fallback label
  expect_equal(pair_result$outcomeLabel, "Outcome X") # Found label
})
