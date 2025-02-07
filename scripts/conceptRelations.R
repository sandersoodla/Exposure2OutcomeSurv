library(dplyr)

getRelatedConcepts <- function(cdm, conceptId) {
  
  relatedConcepts <- cdm$concept_relationship %>%
    filter(concept_id_1 == conceptId) %>%
    select(concept_id_2, relationship_id) %>%
    inner_join(cdm$concept, by = c("concept_id_2" = "concept_id")) %>%
    select(concept_id = concept_id_2,  concept_name, relationship_id) %>%
    collect() %>%
    mutate(concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  # Get the input concept's information
  inputConcept <- cdm$concept %>%
    filter(concept_id == conceptId) %>%
    select(concept_id, concept_name) %>%
    collect() %>%
    mutate(relationship_id = "Input concept",
           concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  
  relatedConcepts <- bind_rows(inputConcept, relatedConcepts)

  return(relatedConcepts)
  
}

