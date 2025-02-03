

getConditionInfo <- function(cdm, conditionConceptId) {

  if (is.na(conditionConceptId)) return(NULL)

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