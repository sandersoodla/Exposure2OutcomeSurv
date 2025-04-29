# --- Mock Helper Functions ---

# Define inert functions to replace the real helpers
mock_notifyUser <- function(...) { invisible(NULL) }
mock_removeUserNotify <- function(...) { invisible(NULL) }

# Use testthat's local mocking to replace the package's internal helpers
testthat::local_mocked_bindings(
  .notifyUser = mock_notifyUser,
  .removeUserNotify = mock_removeUserNotify,
  .package = "Exposure2OutcomeSurv"
)

# --- Test Suite ---

test_that("filterByWashoutAndGetOutcomeDates works correctly", {
  # --- Setup mock data ---
  cohortBase <- tibble::tibble(
    person_id = 1:5,
    gender_concept_id = c(8507, 8532, 8507, 8532, 8507),
    year_of_birth = c(1980, 1985, 1990, 1995, 1980),
    obs_start_date = as.Date(c("2000-01-01", "2005-01-01", "2008-01-01", "2010-01-01", "2002-06-01")),
    obs_end_date = as.Date(c("2015-12-31", "2018-12-31", "2019-12-31", "2020-12-31", "2016-05-31"))
  )
  allConditionDates <- tibble::tribble(
    ~person_id, ~concept_id, ~condition_start_date,
    1, 201, as.Date("2010-08-20"), # Outcome 1, after 5yr washout (2000+5=2005) -> Keep in cohortEligible
    2, 201, as.Date("2006-05-01"), # Outcome 1, before 5yr washout (2005+5=2010) -> Remove from cohortEligible
    3, 201, as.Date("2015-02-25"), # Outcome 1, after 5yr washout (2008+5=2013) -> Keep in cohortEligible
    4, 201, as.Date("2011-01-01"), # Outcome 1, before 5yr washout (2010+5=2015) -> Remove from cohortEligible
    5, 201, NA,                  # No Outcome 1 date -> Keep in cohortEligible
    1, 202, as.Date("2003-01-01"), # Outcome 2, before 5yr washout (2000+5=2005) -> Remove if testing O2
    3, 202, as.Date("2016-01-01"), # Outcome 2, after 5yr washout (2008+5=2013) -> Keep if testing O2
  )
  outcomeId_1 <- 201
  outcomeId_2 <- 202
  washoutYears <- 5
  
  # --- Test Case 1: Washout for Outcome 201 ---
  result1 <- filterByWashoutAndGetOutcomeDates(cohortBase, allConditionDates, outcomeId_1, washoutYears)
  
  expect_type(result1, "list")
  expect_named(result1, c("cohortEligible", "outcomeDates"))
  expect_s3_class(result1$cohortEligible, "tbl_df")
  expect_s3_class(result1$outcomeDates, "tbl_df")
  
  # Check eligible cohort (Persons 1, 3, 5 should remain)
  expect_equal(nrow(result1$cohortEligible), 3)
  expect_equal(sort(result1$cohortEligible$person_id), c(1, 3, 5))
  expect_equal(colnames(result1$cohortEligible), colnames(cohortBase)) # Structure preserved
  
  # Check outcome dates extracted (should include all with outcome 201 for persons 1-5)
  # Expect 5 rows, persons 1-5
  expect_equal(nrow(result1$outcomeDates), 5)
  expect_named(result1$outcomeDates, c("person_id", "outcome_date"))
  expect_equal(sort(result1$outcomeDates$person_id), c(1, 2, 3, 4, 5))
  expect_equal(result1$outcomeDates$outcome_date[result1$outcomeDates$person_id == 1], as.Date("2010-08-20"))
  expect_true(is.na(result1$outcomeDates$outcome_date[result1$outcomeDates$person_id == 5])) # Verify NA is present
  
  # --- Test Case 2: Washout for Outcome 202 ---
  result2 <- filterByWashoutAndGetOutcomeDates(cohortBase, allConditionDates, outcomeId_2, washoutYears)
  
  # Check eligible cohort (Persons 2, 3, 4, 5 should remain - P1 removed)
  expect_equal(nrow(result2$cohortEligible), 4)
  expect_equal(sort(result2$cohortEligible$person_id), c(2, 3, 4, 5))
  
  # Check outcome dates extracted for outcome 202 (Only P1 and P3 had outcome 202)
  expect_equal(nrow(result2$outcomeDates), 2)
  expect_named(result2$outcomeDates, c("person_id", "outcome_date"))
  expect_equal(sort(result2$outcomeDates$person_id), c(1, 3))
  
  # --- Test Case 3: No one eligible ---
  cohortBase_short_obs <- cohortBase %>% dplyr::filter(person_id %in% c(2, 4)) # Only those failing washout for O1
  result_none <- filterByWashoutAndGetOutcomeDates(cohortBase_short_obs, allConditionDates, outcomeId_1, washoutYears)
  expect_equal(nrow(result_none$cohortEligible), 0)
  expect_s3_class(result_none$cohortEligible, "tbl_df") # Still a tibble
  # Outcome dates should still be returned for persons 2 and 4 (who were in the input cohortBase_short_obs)
  expect_equal(nrow(result_none$outcomeDates), 2)
  expect_equal(sort(result_none$outcomeDates$person_id), c(2, 4))
  
  
  # --- Test Case 4: Empty input cohort ---
  result_empty_cohort <- filterByWashoutAndGetOutcomeDates(tibble::tibble(), allConditionDates, outcomeId_1, washoutYears)
  expect_equal(nrow(result_empty_cohort$cohortEligible), 0)
  expect_equal(nrow(result_empty_cohort$outcomeDates), 0)
  
  # --- Test Case 5: Null or empty condition dates ---
  # Scenario A: NULL input
  result_null_cond <- filterByWashoutAndGetOutcomeDates(cohortBase, NULL, outcomeId_1, washoutYears)
  expect_equal(nrow(result_null_cond$cohortEligible), 5) # Everyone kept as no outcome info
  expect_equal(nrow(result_null_cond$outcomeDates), 0)
  # Scenario B: Empty tibble input
  result_empty_cond <- filterByWashoutAndGetOutcomeDates(cohortBase,
                                                         tibble::tibble(person_id=integer(), concept_id=integer(), condition_start_date=as.Date(character())),
                                                         outcomeId_1, washoutYears)
  expect_equal(nrow(result_empty_cond$cohortEligible), 5) # Everyone kept as no outcome info
  expect_equal(nrow(result_empty_cond$outcomeDates), 0)
})


test_that("defineExposedCohortForPair works correctly", {
  # --- Setup mock data ---
  cohortBaseForOutcome <- tibble::tibble( # Assume these passed washout for Outcome 201
    person_id = c(1, 3, 5),
    gender_concept_id = c(8507, 8507, 8507),
    year_of_birth = c(1980, 1990, 1980),
    obs_start_date = as.Date(c("2000-01-01", "2008-01-01", "2002-06-01")),
    obs_end_date = as.Date(c("2015-12-31", "2019-12-31", "2016-05-31"))
  )
  allConditionFirstDates <- tibble::tribble(
    ~person_id, ~concept_id, ~condition_start_date,
    # Exposures (E1=101, E2=102)
    1, 101, as.Date("2005-03-15"), # E1 for P1: Passes washout(>=2002-01-01), obs, E<O -> Keep
    3, 102, as.Date("2009-11-01"), # E2 for P3: Fails washout(<2010-01-01) -> Remove
    5, 101, as.Date("2007-05-20"), # E1 for P5: Passes washout(>=2004-06-01), obs, E<O -> Keep
    1, 102, as.Date("2016-01-01"), # E2 for P1: Passes washout, but Fails obs end(>2015-12-31) -> Remove
    # Outcomes (O1=201)
    1, 201, as.Date("2010-08-20"), # O1 for P1
    3, 201, as.Date("2015-02-25"), # O1 for P3
    5, 201, as.Date("2014-09-10"), # O1 for P5
    # Other persons/conditions not relevant here
    2, 101, as.Date("2007-01-10"),
    4, 101, as.Date("2012-07-01"),
  )
  currentOutcomeId <- 201
  outcomeDatesCurrentOutcome <- tibble::tibble( # Outcome 201 dates for P1, P3, P5
    person_id = c(1, 3, 5),
    outcome_date = as.Date(c("2010-08-20", "2015-02-25", "2014-09-10"))
  )
  exposureId_1 <- 101
  exposureId_2 <- 102
  washoutYears <- 2
  
  # --- Test Case 1: Define exposed for Exposure 101 ---
  result1 <- defineExposedCohortForPair(cohortBaseForOutcome, allConditionFirstDates,
                                        outcomeDatesCurrentOutcome, exposureId_1,
                                        currentOutcomeId, washoutYears)
  
  expect_type(result1, "list")
  expect_named(result1, c("exposed", "exposureDates"))
  expect_s3_class(result1$exposed, "tbl_df")
  expect_s3_class(result1$exposureDates, "tbl_df")
  
  # Check exposed cohort (P1 and P5 pass all checks)
  expect_equal(nrow(result1$exposed), 2)
  expect_equal(sort(result1$exposed$exposed_person_id), c(1, 5))
  expect_named(result1$exposed, c("exposed_person_id", "index_date", "year_of_birth", "gender_concept_id"))
  expect_equal(result1$exposed$index_date[result1$exposed$exposed_person_id == 1], as.Date("2005-03-15"))
  expect_equal(result1$exposed$index_date[result1$exposed$exposed_person_id == 5], as.Date("2007-05-20"))
  
  # Check exposure dates extracted (P1, P5 had exposure 101)
  expect_equal(nrow(result1$exposureDates), 2)
  expect_named(result1$exposureDates, c("person_id", "exposure_date"))
  expect_equal(sort(result1$exposureDates$person_id), c(1, 5))
  
  # --- Test Case 2: Define exposed for Exposure 102 ---
  result2 <- defineExposedCohortForPair(cohortBaseForOutcome, allConditionFirstDates,
                                        outcomeDatesCurrentOutcome, exposureId_2,
                                        currentOutcomeId, washoutYears)
  
  # Check exposed cohort (P3 fails washout, P1 fails obs period) -> Should be NULL
  expect_null(result2$exposed)
  
  # Exposure dates should still be returned (P1, P3)
  expect_s3_class(result2$exposureDates, "tbl_df")
  expect_equal(nrow(result2$exposureDates), 2)
  expect_equal(sort(result2$exposureDates$person_id), c(1, 3))
  
  # --- Test Case 3: Exposure occurs after outcome ---
  allConditionFirstDates_exp_after_out <- allConditionFirstDates %>%
    dplyr::mutate(condition_start_date = dplyr::if_else(person_id == 1 & concept_id == 101,
                                                        as.Date("2011-01-01"), # Move P1's E1 after P1's O1
                                                        condition_start_date))
  
  result_exp_after <- defineExposedCohortForPair(cohortBaseForOutcome, allConditionFirstDates_exp_after_out,
                                                 outcomeDatesCurrentOutcome, exposureId_1,
                                                 currentOutcomeId, washoutYears)
  
  # P1: Passes washout, obs period. Fails E<O. -> Remove P1.
  # P5: Passes. -> Keep P5.
  expect_s3_class(result_exp_after$exposed, "tbl_df")
  expect_equal(nrow(result_exp_after$exposed), 1)
  expect_equal(result_exp_after$exposed$exposed_person_id, 5)
  
  # Expect exposure dates for P1 and P5 to still be returned
  expect_s3_class(result_exp_after$exposureDates, "tbl_df")
  expect_equal(nrow(result_exp_after$exposureDates), 2)
  expect_equal(sort(result_exp_after$exposureDates$person_id), c(1, 5))
  
  # --- Test Case 4: Exposure occurs outside observation period ---
  allConditionFirstDates_exp_outside_obs <- allConditionFirstDates %>%
    dplyr::mutate(condition_start_date = dplyr::if_else(person_id == 5 & concept_id == 101,
                                                        as.Date("2001-01-01"), # Move P5's E1 before P5's obs_start
                                                        condition_start_date))
  
  result_exp_outside <- defineExposedCohortForPair(cohortBaseForOutcome, allConditionFirstDates_exp_outside_obs,
                                                   outcomeDatesCurrentOutcome, exposureId_1,
                                                   currentOutcomeId, washoutYears)
  # Now only P1 should be eligible for E1
  expect_equal(nrow(result_exp_outside$exposed), 1)
  expect_equal(result_exp_outside$exposed$exposed_person_id, 1)
  # exposureDates still includes P1 and P5
  expect_equal(nrow(result_exp_outside$exposureDates), 2)
  expect_equal(sort(result_exp_outside$exposureDates$person_id), c(1, 5))
  
  # --- Test Case 5: No valid exposed individuals ---
  result_none <- defineExposedCohortForPair(cohortBaseForOutcome, allConditionFirstDates,
                                            outcomeDatesCurrentOutcome, 999, # Non-existent exposure ID
                                            currentOutcomeId, washoutYears)
  expect_null(result_none$exposed)
  expect_equal(nrow(result_none$exposureDates), 0) # No dates found for this exposure
  
  # --- Test Case 6: Empty base cohort ---
  result_empty_base <- defineExposedCohortForPair(tibble::tibble(), allConditionFirstDates,
                                                  outcomeDatesCurrentOutcome, exposureId_1,
                                                  currentOutcomeId, washoutYears)
  expect_null(result_empty_base$exposed) # Should be NULL
  expect_null(result_empty_base$exposureDates) # No dates as no one in base
})


test_that("performPairMatching works correctly", {
  # --- Setup mock data ---
  exposedCohortDefinition <- tibble::tibble( # P1 and P3 exposed
    exposed_person_id = c(1, 3),
    index_date = as.Date(c("2005-03-15", "2009-11-01")),
    year_of_birth = c(1980, 1990),
    gender_concept_id = c(8507, 8507) # Both Male
  )
  # Potential controls pool (passed outcome washout)
  controlsPoolBaseOutcome <- tibble::tibble(
    person_id = c(2, 4, 5, 6, 7, 8),
    year_of_birth =     c(1985, 1995, 1980, 1985, 1992, 1998),
    gender_concept_id = c(8532, 8532, 8507, 8532, 8507, 8507), # F, F, M, F, M, M
    obs_start_date = as.Date(c("2005-01-01", "2010-01-01", "2002-06-01", "2006-06-01", "2009-06-01", "2011-06-01")),
    obs_end_date = as.Date(c("2018-12-31", "2020-12-31", "2016-05-31", "2017-05-31", "2021-05-31", "2022-05-31")),
    outcome_date = as.Date(c("2006-05-01", NA, "2014-09-10", "2010-10-10", "2012-04-01", NA)) # Outcome 201 dates
  )
  # Exposure dates for the specific exposure (101) being matched
  exposureDatesCurrentExposure <- tibble::tibble(
    person_id = c(1, 4, 8), # P1 (exposed), P4 (exposed after index), P8 (exposed after index)
    exposure_date = as.Date(c("2005-03-15", "2012-07-01", "2013-05-05"))
  )
  matchRatio <- 2
  exposureId <- 101
  outcomeId <- 201 # Assume outcome 201 was used for washout/control eligibility
  
  # --- Test Case 1: Basic matching ---
  result1 <- performPairMatching(exposedCohortDefinition, controlsPoolBaseOutcome,
                                 exposureDatesCurrentExposure, matchRatio,
                                 exposureId, outcomeId)
  
  expect_type(result1, "list")
  expect_named(result1, c("matchedData", "nMatchedExposed"))
  expect_s3_class(result1$matchedData, "tbl_df")
  expect_type(result1$nMatchedExposed, "double")
  
  # Check number of matched exposed (should be 2)
  expect_equal(result1$nMatchedExposed, 2)
  
  # Check matched data structure
  expect_named(result1$matchedData, c("person_id", "exposure_status", "index_date", "set_id"))
  
  # --- Detailed checks for Exposed Person 1 (Index: 2005-03-15, YoB: 1980, Gender: M) ---
  # Potential Controls: P2(F), P4(F), P5(M, YoB:1980), P6(F), P7(M, YoB:1992), P8(M, YoB:1998)
  # Risk Set for P1 (Observed at 2005-03-15, No Exp 101 before, No Out 201 before):
  # P2: Obs OK (2005-01-01 - 2018-12-31). No Exp 101. Out 201 @ 2006-05-01 (> index). -> Eligible
  # P4: Obs NOT OK (starts 2010-01-01). -> Ineligible
  # P5: Obs OK (2002-06-01 - 2016-05-31). No Exp 101. Out 201 @ 2014-09-10 (> index). -> Eligible
  # P6: Obs NOT OK (starts 2006-06-01). -> Ineligible
  # P7: Obs NOT OK (starts 2009-06-01). -> Ineligible
  # P8: Obs NOT OK (starts 2011-06-01). -> Ineligible
  # Risk Set for P1: {P2, P5}
  # Matching for P1 (Gender M, YoB 1980, Ratio 2):
  # Filter by Gender M: {P5}
  # Calculate Age Diff: P5 (abs(1980-1980)=0)
  # Sort & Slice (n=2): {P5} -> Only 1 control found
  p1_set <- result1$matchedData %>% dplyr::filter(set_id == 1)
  expect_equal(nrow(p1_set), 2) # Exposed + 1 Control
  expect_equal(p1_set$person_id[p1_set$exposure_status == 1], 1)
  expect_equal(p1_set$person_id[p1_set$exposure_status == 0], 5)
  expect_true(all(p1_set$index_date == as.Date("2005-03-15")))
  
  # --- Detailed checks for Exposed Person 3 (Index: 2009-11-01, YoB: 1990, Gender: M) ---
  # Potential Controls: P2(F), P4(F), P5(M, YoB:1980), P6(F), P7(M, YoB:1992), P8(M, YoB:1998)
  # Risk Set for P3 (Observed at 2009-11-01, No Exp 101 before, No Out 201 before):
  # P2: Obs OK. No Exp 101. Out 201 @ 2006-05-01 (< index). -> Ineligible (Outcome before index)
  # P4: Obs NOT OK (starts 2010-01-01). -> Ineligible
  # P5: Obs OK. No Exp 101. Out 201 @ 2014-09-10 (> index). -> Eligible
  # P6: Obs OK (2006-06-01 - 2017-05-31). No Exp 101. Out 201 @ 2010-10-10 (> index). -> Eligible
  # P7: Obs OK (2009-06-01 - 2021-05-31). No Exp 101. Out 201 @ 2012-04-01 (> index). -> Eligible
  # P8: Obs NOT OK (starts 2011-06-01). -> Ineligible
  # Risk Set for P3: {P5, P6, P7}
  # Matching for P3 (Gender M, YoB 1990, Ratio 2):
  # Filter by Gender M: {P5, P7}
  # Calculate Age Diff: P5 (abs(1980-1990)=10), P7 (abs(1992-1990)=2)
  # Sort & Slice (n=2): {P7, P5}
  p3_set <- result1$matchedData %>% dplyr::filter(set_id == 2)
  expect_equal(nrow(p3_set), 3) # Exposed + 2 Controls
  expect_equal(p3_set$person_id[p3_set$exposure_status == 1], 3)
  expect_equal(sort(p3_set$person_id[p3_set$exposure_status == 0]), c(5, 7))
  expect_true(all(p3_set$index_date == as.Date("2009-11-01")))
  
  # --- Test Case 2: Match Ratio 1 ---
  result2 <- performPairMatching(exposedCohortDefinition, controlsPoolBaseOutcome,
                                 exposureDatesCurrentExposure, 1, # matchRatio = 1
                                 exposureId, outcomeId)
  expect_equal(result2$nMatchedExposed, 2)
  p1_set_ratio1 <- result2$matchedData %>% dplyr::filter(set_id == 1)
  p3_set_ratio1 <- result2$matchedData %>% dplyr::filter(set_id == 2)
  expect_equal(nrow(p1_set_ratio1), 2) # P1 + P5
  expect_equal(nrow(p3_set_ratio1), 2) # P3 + P7 (closest age match)
  expect_equal(p3_set_ratio1$person_id[p3_set_ratio1$exposure_status == 0], 7)
  
  # --- Test Case 3: No controls found for one exposed ---
  exposedCohortDefinition_P1_only <- exposedCohortDefinition %>% dplyr::filter(exposed_person_id == 1)
  controlsPoolBaseOutcome_no_males <- controlsPoolBaseOutcome %>% dplyr::filter(gender_concept_id != 8507)
  result3 <- performPairMatching(exposedCohortDefinition_P1_only, controlsPoolBaseOutcome_no_males,
                                 exposureDatesCurrentExposure, matchRatio,
                                 exposureId, outcomeId)
  expect_equal(result3$nMatchedExposed, 0)
  expect_null(result3$matchedData)
  
  # --- Test Case 4: Empty exposed cohort ---
  result4 <- performPairMatching(tibble::tibble(), controlsPoolBaseOutcome,
                                 exposureDatesCurrentExposure, matchRatio,
                                 exposureId, outcomeId)
  expect_equal(result4$nMatchedExposed, 0)
  expect_null(result4$matchedData)
})


test_that("calculatePairSurvival works correctly", {
  # --- Setup mock data ---
  matchedDataFinal <- tibble::tibble( # Output from performPairMatching (P1+P5, P3+P7+P5)
    person_id =       c(1, 5,    3, 7, 5),
    exposure_status = c(1, 0,    1, 0, 0),
    index_date = as.Date(c("2005-03-15", "2005-03-15", "2009-11-01", "2009-11-01", "2009-11-01")),
    set_id =          c(1, 1,    2, 2, 2)
  )
  # Base cohort info (needs obs periods for matched individuals)
  cohortBaseForOutcome <- tibble::tibble(
    person_id = c(1, 3, 5, 7), # Include all unique persons from matchedDataFinal
    obs_start_date = as.Date(c("2000-01-01", "2008-01-01", "2002-06-01", "2009-06-01")),
    obs_end_date = as.Date(c("2015-12-31", "2019-12-31", "2016-05-31", "2021-05-31"))
    # Other columns like gender, yob not strictly needed here but present in real data
  )
  # Outcome dates for the specific outcome (201)
  outcomeDatesCurrentOutcome <- tibble::tibble(
    person_id = c(1, 3, 5, 7),
    outcome_date = as.Date(c("2010-08-20", "2015-02-25", "2014-09-10", "2012-04-01"))
  )
  
  # --- Test Case 1: Calculate survival ---
  result <- calculatePairSurvival(matchedDataFinal, cohortBaseForOutcome, outcomeDatesCurrentOutcome)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5) # Same number of rows as input matched data
  expect_named(result, c("person_id", "set_id", "exposure_status", "index_date",
                         "study_exit_date", "time_to_outcome", "outcome_status"))
  
  # --- Detailed checks ---
  # Person 1 (Exposed, Set 1): Index=2005-03-15, ObsEnd=2015-12-31, Outcome=2010-08-20
  # Outcome occurs after index and before ObsEnd.
  # Exit = Outcome Date = 2010-08-20
  # Status = 1
  # Time = 2010-08-20 - 2005-03-15 = 1984 days
  p1_res <- result %>% dplyr::filter(person_id == 1)
  expect_equal(p1_res$study_exit_date, as.Date("2010-08-20"))
  expect_equal(p1_res$outcome_status, 1)
  expect_equal(p1_res$time_to_outcome, 1984)
  
  # Person 5 (Control, Set 1): Index=2005-03-15, ObsEnd=2016-05-31, Outcome=2014-09-10
  # Outcome occurs after index and before ObsEnd.
  # Exit = Outcome Date = 2014-09-10
  # Status = 1
  # Time = 2014-09-10 - 2005-03-15 = 3466 days
  p5_s1_res <- result %>% dplyr::filter(person_id == 5, set_id == 1)
  expect_equal(p5_s1_res$study_exit_date, as.Date("2014-09-10"))
  expect_equal(p5_s1_res$outcome_status, 1)
  expect_equal(p5_s1_res$time_to_outcome, 3466)
  
  # Person 3 (Exposed, Set 2): Index=2009-11-01, ObsEnd=2019-12-31, Outcome=2015-02-25
  # Outcome occurs after index and before ObsEnd.
  # Exit = Outcome Date = 2015-02-25
  # Status = 1
  # Time = 2015-02-25 - 2009-11-01 = 1942 days
  p3_res <- result %>% dplyr::filter(person_id == 3)
  expect_equal(p3_res$study_exit_date, as.Date("2015-02-25"))
  expect_equal(p3_res$outcome_status, 1)
  expect_equal(p3_res$time_to_outcome, 1942)
  
  # Person 7 (Control, Set 2): Index=2009-11-01, ObsEnd=2021-05-31, Outcome=2012-04-01
  # Outcome occurs after index and before ObsEnd.
  # Exit = Outcome Date = 2012-04-01
  # Status = 1
  # Time = 2012-04-01 - 2009-11-01 = 882 days
  p7_res <- result %>% dplyr::filter(person_id == 7)
  expect_equal(p7_res$study_exit_date, as.Date("2012-04-01"))
  expect_equal(p7_res$outcome_status, 1)
  expect_equal(p7_res$time_to_outcome, 882)
  
  # Person 5 (Control, Set 2): Index=2009-11-01, ObsEnd=2016-05-31, Outcome=2014-09-10
  # Outcome occurs after index and before ObsEnd.
  # Exit = Outcome Date = 2014-09-10
  # Status = 1
  # Time = 2014-09-10 - 2009-11-01 = 1774 days
  p5_s2_res <- result %>% dplyr::filter(person_id == 5, set_id == 2)
  expect_equal(p5_s2_res$study_exit_date, as.Date("2014-09-10"))
  expect_equal(p5_s2_res$outcome_status, 1)
  expect_equal(p5_s2_res$time_to_outcome, 1774)
  
  # --- Test Case 2: Censoring at observation end ---
  outcomeDates_censored <- outcomeDatesCurrentOutcome %>%
    dplyr::mutate(outcome_date = dplyr::if_else(person_id == 1, as.Date("2017-01-01"), outcome_date)) # Move P1 outcome after obs_end
  result_censored <- calculatePairSurvival(matchedDataFinal, cohortBaseForOutcome, outcomeDates_censored)
  
  # Person 1 (Exposed, Set 1): Index=2005-03-15, ObsEnd=2015-12-31, Outcome=2017-01-01
  # Outcome occurs after ObsEnd.
  # Exit = Obs End Date = 2015-12-31
  # Status = 0
  # Time = 2015-12-31 - 2005-03-15 = 3943 days
  p1_res_cen <- result_censored %>% dplyr::filter(person_id == 1)
  expect_equal(p1_res_cen$study_exit_date, as.Date("2015-12-31"))
  expect_equal(p1_res_cen$outcome_status, 0)
  expect_equal(p1_res_cen$time_to_outcome, 3943)
  
  # --- Test Case 3: Outcome before index date (should not happen if matching is correct, but test robustness) ---
  outcomeDates_before_index <- outcomeDatesCurrentOutcome %>%
    dplyr::mutate(outcome_date = dplyr::if_else(person_id == 7, as.Date("2009-10-01"), outcome_date)) # Move P7 outcome before index
  result_before <- calculatePairSurvival(matchedDataFinal, cohortBaseForOutcome, outcomeDates_before_index)
  
  # Person 7 (Control, Set 2): Index=2009-11-01, ObsEnd=2021-05-31, Outcome=2009-10-01
  # Outcome occurs before index.
  # Exit = Obs End Date = 2021-05-31 (as outcome_event_date is NA)
  # Status = 0
  # Time = 2021-05-31 - 2009-11-01 = 4229 days
  p7_res_bef <- result_before %>% dplyr::filter(person_id == 7)
  expect_equal(p7_res_bef$study_exit_date, as.Date("2021-05-31"))
  expect_equal(p7_res_bef$outcome_status, 0)
  expect_equal(p7_res_bef$time_to_outcome, 4229)
  
  # --- Test Case 4: No outcome recorded (NA) ---
  outcomeDates_no_outcome <- outcomeDatesCurrentOutcome %>%
    dplyr::filter(person_id != 5) # Remove outcome for P5
  result_no_outcome <- calculatePairSurvival(matchedDataFinal, cohortBaseForOutcome, outcomeDates_no_outcome)
  
  # Person 5 (Control, Set 1): Index=2005-03-15, ObsEnd=2016-05-31, Outcome=NA
  # Exit = Obs End Date = 2016-05-31
  # Status = 0
  # Time = 2016-05-31 - 2005-03-15 = 4095 days
  p5_s1_res_no <- result_no_outcome %>% dplyr::filter(person_id == 5, set_id == 1)
  expect_equal(p5_s1_res_no$study_exit_date, as.Date("2016-05-31"))
  expect_equal(p5_s1_res_no$outcome_status, 0)
  expect_equal(p5_s1_res_no$time_to_outcome, 4095)
  
  # Person 5 (Control, Set 2): Index=2009-11-01, ObsEnd=2016-05-31, Outcome=NA
  # Exit = Obs End Date = 2016-05-31
  # Status = 0
  # Time = 2016-05-31 - 2009-11-01 = 2403 days
  p5_s2_res_no <- result_no_outcome %>% dplyr::filter(person_id == 5, set_id == 2)
  expect_equal(p5_s2_res_no$study_exit_date, as.Date("2016-05-31"))
  expect_equal(p5_s2_res_no$outcome_status, 0)
  expect_equal(p5_s2_res_no$time_to_outcome, 2403)
  
  # --- Test Case 5: Empty input ---
  result_empty <- calculatePairSurvival(tibble::tibble(), cohortBaseForOutcome, outcomeDatesCurrentOutcome)
  expect_null(result_empty)
})

