

getPeopleWithCondition <- function(cdm, conceptId) {
  
  peopleWithCondition <- cdm$condition_occurrence %>%
         dplyr::filter(condition_concept_id == conceptId) %>%
         dplyr::distinct(person_id) %>%
         dplyr::collect()
  
  return(peopleWithCondition)
}


getFirstConditionDatesForPersons <- function(cdm, personIds, conceptIds) {
  if (length(personIds) == 0 || length(conceptIds) == 0) {
    return(tibble::tibble(person_id = integer(), concept_id = integer(), condition_start_date = as.Date(character())))
  }
  
  result <- cdm$condition_occurrence %>%
         dplyr::filter(person_id %in% personIds & condition_concept_id %in% conceptIds) %>%
         dplyr::group_by(person_id, condition_concept_id) %>%
         dplyr::summarise(condition_start_date = min(condition_start_date, na.rm = TRUE), .groups = "drop") %>%
         dplyr::collect() %>%
         dplyr::rename(concept_id = condition_concept_id) # Match output column name expectation
  
  return(result)
}


getTrajectoriesForCondition <- function(cdm, conditionConceptIDs) {
  
  trajectories <- cdm$condition_occurrence %>%
    # Keep only rows for persons who have at least one of the conditionConceptIDs
    dplyr::semi_join(
      cdm$condition_occurrence %>%
        dplyr::filter(condition_concept_id %in% conditionConceptIDs) %>%
        dplyr::distinct(person_id),
      by = "person_id"
    ) %>%
    dplyr::inner_join(cdm$concept, by = c("condition_concept_id" = "concept_id"), keep = TRUE) %>%
    dplyr::select(person_id, concept_id, concept_name, condition_start_date) %>%
    dplyr::arrange(person_id, condition_start_date) %>%
    dplyr::collect()
  
  return(trajectories)
}


getOccurrencesOfCondition <- function(cdm, conditionConceptID) {
  
  occurrences <- cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id == conditionConceptID) %>%
    dplyr::inner_join(cdm$concept, by = c("condition_concept_id" = "concept_id")) %>%
    dplyr::select(person_id, concept_id, concept_name, condition_start_date) %>%
    dplyr::arrange(person_id, condition_start_date) %>%
    dplyr::collect()
  
  return(occurrences)
}
