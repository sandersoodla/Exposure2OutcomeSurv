
# Note: procedures after FIRST condition occurrence of start condition
getProceduresAfterStartCondition <- function(cdm, startConditionIds) {
  # Get the first occurrence date of the start condition for each patient
  firstOccurrence <- cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id %in% startConditionIds) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarise(start_date = min(condition_start_date), .groups = "drop")
  
  # Join with procedure_occurrence to get procedures after the start date
  proceduresData <- cdm$procedure_occurrence %>%
    dplyr::inner_join(firstOccurrence, by = "person_id") %>%
    dplyr::filter(procedure_date >= start_date) %>%
    dplyr::select(person_id, concept_id = procedure_concept_id, procedure_date) %>%
    dplyr::left_join(cdm$concept %>% dplyr::select(concept_id, concept_name), by = "concept_id") %>%
    dplyr::arrange(person_id, procedure_date) %>%
    dplyr::collect()
  
  return(proceduresData)
}
