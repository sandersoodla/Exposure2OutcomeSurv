
# Function to map source IDs to standard concept IDs
mapInputToStandardIds <- function(cdm, sourceIds) {
  # Query the concept_relationship table from the CDM
  mappingDf <- cdm$concept_relationship %>%
    dplyr::filter(relationship_id == "Maps to") %>%
    dplyr::filter(concept_id_1 %in% sourceIds) %>%
    dplyr::select(input_concept_id = concept_id_1, standard_concept_id = concept_id_2) %>%
    dplyr::collect()
  
  return(mappingDf)
}

getRelatedConcepts <- function(cdm, conceptId) {
  
  relatedConcepts <- cdm$concept_relationship %>%
    dplyr::filter(concept_id_1 == conceptId) %>%
    dplyr::select(concept_id_2, relationship_id) %>%
    dplyr::inner_join(cdm$concept, by = c("concept_id_2" = "concept_id")) %>%
    dplyr::select(concept_id = concept_id_2,  concept_name, relationship_id) %>%
    dplyr::collect() %>%
    dplyr::mutate(concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  # Get the input concept's information
  inputConcept <- cdm$concept %>%
    dplyr::filter(concept_id == conceptId) %>%
    dplyr::select(concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(relationship_id = "Input concept",
           concept_name_id = paste(concept_name, " (", concept_id, ")"))
  
  
  relatedConcepts <- dplyr::bind_rows(inputConcept, relatedConcepts)

  return(relatedConcepts)
  
}

