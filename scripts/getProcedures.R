

getProceduresAfterStartCondition <- function(cdm, startConditionId) {
  # Get the first occurrence date of the start condition for each patient
  firstOccurrence <- cdm$condition_occurrence %>%
    filter(condition_concept_id == startConditionId) %>%
    group_by(person_id) %>%
    summarise(start_date = min(condition_start_date), .groups = "drop")
  
  # Join with procedure_occurrence to get procedures after the start date
  proceduresData <- cdm$procedure_occurrence %>%
    inner_join(firstOccurrence, by = "person_id") %>%
    filter(procedure_date >= start_date) %>%
    select(person_id, concept_id = procedure_concept_id, procedure_date) %>%
    left_join(cdm$concept %>% select(concept_id, concept_name), by = "concept_id") %>%
    arrange(person_id, procedure_date) %>%
    collect()
  
  return(proceduresData)
}
