library(DatabaseConnector)
library(SqlRender)
library(lubridate)
library(dplyr)
library(data.table)


getTrajectoriesForCondition <- function(cdm, conditionConceptIDs) {
  
  trajectories <- cdm$condition_occurrence %>%
    # Keep only rows for persons who have at least one of the conditionConceptIDs
    semi_join(
      cdm$condition_occurrence %>%
        filter(condition_concept_id %in% conditionConceptIDs) %>%
        distinct(person_id),
      by = "person_id"
    ) %>%
    inner_join(cdm$concept, by = c("condition_concept_id" = "concept_id"), keep = TRUE) %>%
    select(person_id, concept_id, concept_name, condition_start_date) %>%
    arrange(person_id, condition_start_date) %>%
    collect()
  
  return(trajectories)
}


getOccurrencesOfCondition <- function(cdm, conditionConceptID) {
  
  occurrences <- cdm$condition_occurrence %>%
    filter(condition_concept_id == conditionConceptID) %>%
    inner_join(cdm$concept, by = c("condition_concept_id" = "concept_id")) %>%
    select(person_id, concept_id, concept_name, condition_start_date) %>%
    arrange(person_id, condition_start_date) %>%
    collect()
  
  return(occurrences)
}

createStartToTargetConditionDF <- function(trajectories,
                                            startConditionConceptIds,
                                            targetConditionConceptIds,
                                            timeframes = c("1y" = 365, "3y" = 3 * 365, "5y" = 5 * 365)) {
  
  # Fetch trajectories and filter for start and target conditions
  trajectoriesForStartCondition <- trajectories
  
  trajectoriesOnlyStartAndTarget <- trajectoriesForStartCondition %>%
    filter(concept_id %in% startConditionConceptIds | concept_id %in% targetConditionConceptIds)
  
  # Separate start and target conditions
  startConditions <- trajectoriesOnlyStartAndTarget %>%
    filter(concept_id %in% startConditionConceptIds) %>%
    arrange(person_id, condition_start_date) %>%
    group_by(person_id) %>%
    mutate(start_condition_nr = row_number()) %>%
    ungroup()
  
  targetConditions <- trajectoriesOnlyStartAndTarget %>%
    filter(concept_id %in% targetConditionConceptIds)
  
  if (nrow(startConditions) == 0) {
    return(data.frame())
  }
  
  # Join start and target conditions on person_id
  # For each start condition, find the earliest target condition after it
  startTargetCombinations <- startConditions %>%
    left_join(targetConditions, by = "person_id", suffix = c("_start", "_target"), relationship = "many-to-many") %>%
    filter(is.na(condition_start_date_target) | condition_start_date_target >= condition_start_date_start) %>%
    mutate(time_diff_days = as.numeric(difftime(condition_start_date_target, condition_start_date_start, units = "days")))
  
  # Find the minimum time difference for each start condition occurrence
  minTimeDiffs <- startTargetCombinations %>%
    group_by(person_id, start_condition_nr) %>%
    summarize(min_time_diff_days = if(any(!is.na(time_diff_days))) min(time_diff_days, na.rm = TRUE) else NA, .groups = "drop")
  
  # Merge back to start conditions
  startConditions <- startConditions %>%
    left_join(minTimeDiffs, by = c("person_id", "start_condition_nr"))
  
  # Loop over the timeframes to create a flag column for each
  for (name in names(timeframes)) {
    threshold <- timeframes[name]
    colname <- paste0("target_condition_in_", name)
    startConditions <- startConditions %>%
      mutate(!!colname := !is.na(min_time_diff_days) & (min_time_diff_days <= threshold))
  }
  
  # Select required columns
  startToTargetCondition <- startConditions %>%
    select(person_id, start_condition_nr, starts_with("target_condition_in_"))
  
  return(startToTargetCondition)
}



calculateStartToTargetPercentages <- function(startToTargetCondition) {
  
  percentages <- startToTargetCondition %>%
    summarize(across(starts_with("target_condition_in_"), ~ mean(.x, na.rm = TRUE) * 100))
  
  percentages <- percentages %>%
    mutate(across(everything(), ~ round(.x, 2)))

  return(percentages)
}
