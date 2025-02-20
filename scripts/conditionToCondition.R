library(DatabaseConnector)
library(SqlRender)
library(lubridate)
library(dplyr)
library(data.table)


getTrajectoriesForCondition <- function(cdm, conditionConceptIDs) {
  
  trajectories <- cdm$condition_occurrence %>%
    # Keep only rows for persons who have the conditionConceptID
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


############### UNTESTED, BUT MAY BE FASTER
############### USES OLD LOGIC WHICH WAS NOT CORRECT

library(data.table)

createStartToTargetConditionDF_dt <- function(trajectories,
                                              start_condition_concept_id,
                                              target_condition_concept_id,
                                              timeframes = c("1y" = 365, "3y" = 3 * 365, "5y" = 5 * 365)) {
  
  # Convert to data.table
  setDT(trajectories)
  
  # Filter for start and target conditions
  trajectories_filtered <- trajectories[concept_id %in% c(start_condition_concept_id, target_condition_concept_id)]
  
  # Separate start and target conditions
  start_conditions <- trajectories_filtered[concept_id == start_condition_concept_id,
                                            .(person_id, condition_start_date)][
                                              , start_condition_nr := rowid(person_id)
                                            ]
  target_conditions <- trajectories_filtered[concept_id == target_condition_concept_id,
                                             .(person_id, condition_start_date)]
  
  # Handle empty cases
  if (nrow(start_conditions) == 0 || nrow(target_conditions) == 0) {
    return(data.table())
  }
  
  # Non-equi join (efficiently finds targets after starts)
  start_target_combinations <- target_conditions[start_conditions,
                                                 on = .(person_id, condition_start_date >= condition_start_date),
                                                 .(person_id, start_condition_nr, x.condition_start_date, i.condition_start_date),
                                                 allow.cartesian = TRUE]  #Needed if targets can be on same day
  
  # Calculate time difference
  start_target_combinations[, time_diff_days := as.numeric(x.condition_start_date - i.condition_start_date)]
  
  # Find minimum time difference for each start condition
  min_time_diffs <- start_target_combinations[, .(min_time_diff_days = min(time_diff_days)),
                                              by = .(person_id, start_condition_nr)]
  
  # Add time condition flags
  for (name in names(timeframes)) {
    threshold <- timeframes[name]
    colname <- paste0("target_condition_in_", name)
    min_time_diffs[, (colname) := !is.na(min_time_diff_days) & (min_time_diff_days <= threshold)]
  }
  
  return(min_time_diffs[, .(person_id, start_condition_nr, min_time_diff_days,
                            .SD), .SDcols=patterns("^target_condition_in_")])
}
