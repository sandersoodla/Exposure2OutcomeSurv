library(DatabaseConnector)
library(SqlRender)
library(lubridate)
library(dplyr)
library(data.table)


getTrajectoriesForCondition <- function(cdm, conditionConceptID) {
  
  trajectories <- cdm$condition_occurrence %>%
    # Keep only rows for persons who have the conditionConceptID
    semi_join(
      cdm$condition_occurrence %>%
        filter(condition_concept_id == conditionConceptID) %>%
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

createStartToTargetConditionDF4 <- function(trajectories, start_condition_concept_id, target_condition_concept_id) {
  
  # Fetch trajectories and filter for start and target conditions
  trajectories_for_start_condition <- trajectories
  
  trajectories_only_start_and_target <- trajectories_for_start_condition %>%
    filter(concept_id == start_condition_concept_id | concept_id == target_condition_concept_id)
  
  # Separate start and target conditions
  start_conditions <- trajectories_only_start_and_target %>%
    filter(concept_id == start_condition_concept_id) %>%
    arrange(person_id, condition_start_date) %>%
    group_by(person_id) %>%
    mutate(start_condition_nr = row_number()) %>%
    ungroup()
  
  target_conditions <- trajectories_only_start_and_target %>%
    filter(concept_id == target_condition_concept_id)
  
  if (nrow(start_conditions) == 0) {
    return(data.frame())
  }
  
  # Join start and target conditions on person_id
  # For each start condition, find the earliest target condition after it
  start_target_combinations <- start_conditions %>%
    inner_join(target_conditions, by = "person_id", suffix = c("_start", "_target"), relationship = "many-to-many") %>%
    filter(condition_start_date_target >= condition_start_date_start) %>%
    mutate(time_diff_days = as.numeric(difftime(condition_start_date_target, condition_start_date_start, units = "days")))
  
  # Find the minimum time difference for each start condition occurrence
  min_time_diffs <- start_target_combinations %>%
    group_by(person_id, start_condition_nr) %>%
    summarize(min_time_diff_days = min(time_diff_days), .groups = "drop")
  
  # Merge back to start conditions
  start_conditions <- start_conditions %>%
    left_join(min_time_diffs, by = c("person_id", "start_condition_nr"))
  
  # Compute boolean flags for target condition occurrences within time frames
  start_conditions <- start_conditions %>%
    mutate(
      target_condition_in_1y = !is.na(min_time_diff_days) & min_time_diff_days <= 365,
      target_condition_in_3y = !is.na(min_time_diff_days) & min_time_diff_days <= 3 * 365,
      target_condition_in_5y = !is.na(min_time_diff_days) & min_time_diff_days <= 5 * 365
    )
  
  # Select required columns
  start_to_target_condition <- start_conditions %>%
    select(
      person_id = person_id,
      start_condition_nr,
      target_condition_in_1y,
      target_condition_in_3y,
      target_condition_in_5y
    )
  
  return(start_to_target_condition)
}



calculateStartToTargetPercentages <- function(start_to_target_condition) {
  
  percentages <- data.table::data.table(
    target_percentage_1y = mean(start_to_target_condition$target_condition_in_1y, na.rm = TRUE) * 100,
    target_percentage_3y = mean(start_to_target_condition$target_condition_in_3y, na.rm = TRUE) * 100,
    target_percentage_5y = mean(start_to_target_condition$target_condition_in_5y, na.rm = TRUE) * 100
  )
  
  percentages[, target_percentage_1y := round(target_percentage_1y, 2)]
  percentages[, target_percentage_3y := round(target_percentage_3y, 2)]
  percentages[, target_percentage_5y := round(target_percentage_5y, 2)]

  return(percentages)
}


#createStartToTargetConditionDF <- function(connection, start_condition_concept_id, target_condition_concept_id) {
#  
#  trajectories_for_start_condition <- getTrajectoriesForCondition(connection, start_condition_concept_id)
#  
#  trajectories_only_start_and_target <- 
#    trajectories_for_start_condition %>% filter(concept_id == start_condition_concept_id | concept_id == target_condition_concept_id)
#  
#  trajectory_list_for_each_person <- split(trajectories_only_start_and_target, trajectories_only_start_and_target$person_id)
#  
#  
#  columns= c("person_id", "start_condition_nr", "target_condition_in_1y", "target_condition_in_3y", "target_condition_in_5y")
#  start_to_target_condition = data.frame(matrix(nrow=0, ncol = 5)) 
#  
#  
#  for (trajectory in trajectory_list_for_each_person) {
#    #if (!any(trajectory$concept_id == target_condition_concept_id)) next
#    
#    start_conditions <- subset(trajectory, concept_id == start_condition_concept_id)
#    target_conditions <- subset(trajectory, concept_id == target_condition_concept_id)
#    
#    if (nrow(target_conditions) == 0) {
#      for (i in 1:nrow(start_conditions)) {
#        row <- c(trajectory$person_id[1], i, FALSE, FALSE, FALSE)
#        start_to_target_condition <- rbind(start_to_target_condition, row)
#      }
#    }
#    
#    person_id <- trajectory$person_id[1]
#    target_condition_in_1y <- FALSE
#    target_condition_in_3y <- FALSE
#    target_condition_in_5y <- FALSE
#    start_condition_nr <- 1
#    
#    for (start_date in as.list(start_conditions$CONDITION_START_DATE)) {
#      start_plus_1y = start_date + years(1)
#      start_plus_3y = start_date + years(3)
#      start_plus_5y = start_date + years(5)
#      if (nrow(subset(target_conditions, CONDITION_START_DATE >= start_date & CONDITION_START_DATE <= start_plus_1y)) > 0) {
#        target_condition_in_5y <- TRUE
#        target_condition_in_3y <- TRUE
#        target_condition_in_1y <- TRUE
#      }
#      else if (nrow(subset(target_conditions, CONDITION_START_DATE >= start_date & CONDITION_START_DATE <= start_plus_3y)) > 0) {
#        target_condition_in_5y <- TRUE
#        target_condition_in_3y <- TRUE
#        target_condition_in_1y <- FALSE
#      }
#      else if (nrow(subset(target_conditions, CONDITION_START_DATE >= start_date & CONDITION_START_DATE <= start_plus_5y)) > 0) {
#        target_condition_in_5y <- TRUE
#        target_condition_in_3y <- FALSE
#        target_condition_in_1y <- FALSE
#      }
#      else {
#        target_condition_in_5y <- FALSE
#        target_condition_in_3y <- FALSE
#        target_condition_in_1y <- FALSE
#      }
#      
#      row <- c(person_id, start_condition_nr, target_condition_in_1y, target_condition_in_3y, target_condition_in_5y)
#      start_to_target_condition <- rbind(start_to_target_condition, row)
#      start_condition_nr <- start_condition_nr + 1
#    }
#    
#  }
#  colnames(start_to_target_condition) = columns
#  
#  return (start_to_target_condition)
#}
#
#
#
#
#createStartToTargetConditionDF2 <- function(connection, start_condition_concept_id, target_condition_concept_id) {
#  # Load trajectories for the start condition
#  trajectories_for_start_condition <- getTrajectoriesForCondition(connection, start_condition_concept_id)
#  
#  # Ensure CONDITION_START_DATE is in Date format
#  trajectories_for_start_condition$CONDITION_START_DATE <- as.Date(trajectories_for_start_condition$CONDITION_START_DATE)
#  
#  # Filter for rows with only start and target conditions
#  trajectories_only_start_and_target <- trajectories_for_start_condition %>%
#    filter(concept_id %in% c(start_condition_concept_id, target_condition_concept_id))
#  
#  # Convert to data.table for faster processing
#  trajectories_dt <- as.data.table(trajectories_only_start_and_target)
#  
#  # Split trajectories by person_id
#  trajectory_list <- split(trajectories_dt, by = "person_id")
#  
#  # Initialize an empty list to store results
#  results <- list()
#  
#  # Process each person's trajectory
#  for (trajectory in trajectory_list) {
#    start_conditions <- trajectory[concept_id == start_condition_concept_id]
#    target_conditions <- trajectory[concept_id == target_condition_concept_id]
#    
#    # If no target conditions, add rows with FALSE for all time windows
#    if (nrow(target_conditions) == 0) {
#      person_results <- start_conditions[, .(
#        person_id = person_id,
#        start_condition_nr = .I,
#        target_condition_in_1y = FALSE,
#        target_condition_in_3y = FALSE,
#        target_condition_in_5y = FALSE
#      )]
#      results[[length(results) + 1]] <- person_results
#      next
#    }
#    
#    # For each start condition, calculate time windows
#    person_results <- start_conditions[, {
#      start_date <- CONDITION_START_DATE
#      
#      target_in_1y <- any(target_conditions$CONDITION_START_DATE >= as.Date(start_date) &
#                            target_conditions$CONDITION_START_DATE <= (as.Date(start_date) + years(1)))
#      
#      target_in_3y <- any(target_conditions$CONDITION_START_DATE >= as.Date(start_date) &
#                            target_conditions$CONDITION_START_DATE <= (as.Date(start_date) + years(3)))
#      
#      target_in_5y <- any(target_conditions$CONDITION_START_DATE >= as.Date(start_date) &
#                            target_conditions$CONDITION_START_DATE <= (as.Date(start_date) + years(5)))
#      
#      .(
#        person_id = person_id[1],
#        start_condition_nr = .I,
#        target_condition_in_1y = target_in_1y,
#        target_condition_in_3y = target_in_3y,
#        target_condition_in_5y = target_in_5y
#      )
#    }, by = seq_len(nrow(start_conditions))]
#    results[[length(results) + 1]] <- person_results
#  }
#  
#  # Combine results and return as a data frame
#  final_result <- rbindlist(results, fill = TRUE)
#  return(as.data.frame(final_result))
#}





