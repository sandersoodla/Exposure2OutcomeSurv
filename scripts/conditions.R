library(DatabaseConnector)
library(SqlRender)
library(lubridate)
library(dplyr)
library(data.table)


getPeopleWithCondition <- function(cdm, conceptId) {
  
  peopleWithCondition <- cdm$condition_occurrence %>%
         filter(condition_concept_id == conceptId) %>%
         distinct(person_id) %>%
         collect()
  
  return(peopleWithCondition)
}


getFirstConditionDatesForPersons <- function(cdm, personIds, conceptIds) {
  if (length(personIds) == 0 || length(conceptIds) == 0) {
    return(tibble(person_id = integer(), concept_id = integer(), condition_start_date = as.Date(character())))
  }
  
  result <- cdm$condition_occurrence %>%
         filter(person_id %in% personIds & condition_concept_id %in% conceptIds) %>%
         group_by(person_id, condition_concept_id) %>%
         summarise(condition_start_date = min(condition_start_date, na.rm = TRUE), .groups = "drop") %>%
         collect() %>%
         rename(concept_id = condition_concept_id) # Match output column name expectation
  
  return(result)
}


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
