

getConditionName <- function(cdm, conditionConceptId) {

  if (is.na(conditionConceptId)) return(NULL)
  
  conditionConceptId <- as.integer(conditionConceptId)

  result <- cdm$concept %>%
    filter(concept_id == conditionConceptId) %>%
    select(concept_name) %>%
    collect()

  if (nrow(result) > 0) {
    return(result$concept_name)  # Extract the concept name
  } else {
    return(NA)
  }
}


getConditionOccurrenceCount <- function(cdm, conditionConceptId) {
  
  conditionConceptId <- as.integer(conditionConceptId)
  
  conditionOccurrenceCount <- cdm$condition_occurrence %>%
    filter(condition_concept_id == conditionConceptId) %>%
    count() %>%
    pull(n)
  
  return(conditionOccurrenceCount)
}


getAllConditions <- function(cdm) {
  allConditionConcepts <- cdm$concept %>%
    filter(domain_id == 'Condition') %>%
    select(concept_id, concept_name) %>%
    collect() %>%
    mutate(concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  return(allConditionConcepts)
}