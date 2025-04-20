

getConditionName <- function(cdm, conditionConceptId) {

  if (is.na(conditionConceptId)) return(NULL)
  
  conditionConceptId <- as.integer(conditionConceptId)

  result <- cdm$concept %>%
    dplyr::filter(concept_id == conditionConceptId) %>%
    dplyr::select(concept_name) %>%
    dplyr::collect()

  if (nrow(result) > 0) {
    return(result$concept_name)  # Extract the concept name
  } else {
    return(NA)
  }
}


getConditionOccurrenceCount <- function(cdm, conditionConceptId) {
  
  conditionConceptId <- as.integer(conditionConceptId)
  
  conditionOccurrenceCount <- cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id == conditionConceptId) %>%
    dplyr::count() %>%
    dplyr::pull(n)
  
  return(conditionOccurrenceCount)
}


getAllConditions <- function(cdm) {
  allConditionConcepts <- cdm$concept %>%
    dplyr::filter(domain_id == 'Condition') %>%
    dplyr::select(concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  return(allConditionConcepts)
}


getAllConditionsWithOccurrences <- function(cdm) {
  allConditionConcepts <- cdm$condition_occurrence %>%
    dplyr::distinct(condition_concept_id) %>%
    dplyr::inner_join(cdm$concept, by = c("condition_concept_id" = "concept_id"), keep = TRUE) %>%
    dplyr::filter(domain_id == 'Condition') %>%
    dplyr::select(concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  return(allConditionConcepts)
}
