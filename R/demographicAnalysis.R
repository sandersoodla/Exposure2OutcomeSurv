
createPopulationPyramidForCondition <- function(cdm, conditionConceptId) {
  
  data <- cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id == conditionConceptId) %>%
    dplyr::inner_join(cdm$person, by = "person_id") %>%
    dplyr::select(person_id, gender_concept_id, condition_start_datetime, birth_datetime) %>%
    dplyr::collect()
  
  # Calculate age at condition occurrence
  data <- data %>%
    dplyr::mutate(age_at_occurrence = floor(lubridate::time_length(condition_start_datetime - birth_datetime, unit = "years")))
  
  # Map gender_concept_id to gender labels
  data <- data %>%
    dplyr::mutate(gender = dplyr::case_when(
      gender_concept_id == 8507 ~ "Male",
      gender_concept_id == 8532 ~ "Female",
      TRUE ~ "Other"
    ))
  
  # Aggregate counts by age and gender
  ageGenderCounts <- data %>%
    dplyr::group_by(age_at_occurrence, gender) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")
  
  # Negative values for Male gender
  ageGenderCounts <- ageGenderCounts %>%
    dplyr::mutate(count = ifelse(gender == "Male", -count, count))
  
  # Plot the population pyramid (ggplot2)
  pyramidPlot <- ggplot2::ggplot(ageGenderCounts, ggplot2::aes(x = age_at_occurrence, y = count, fill = gender)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = abs) +
    ggplot2::labs(title = paste("Population Pyramid for Condition Concept ID", conditionConceptId),
         x = "Age",
         y = "Number of Occurrences") +
    ggplot2::theme_minimal()
  
  return(pyramidPlot)
}


getPersonInfo <- function(cdm, personId) {
  
  personInfo <- cdm$person %>%
    dplyr::filter(person_id == personId) %>%
    dplyr::collect()
  
  return(personInfo)
}


getAllGendersAndBirthyears <- function(cdm) {
  
  genderAndYear <- cdm$person %>%
    dplyr::select(person_id, gender_concept_id, year_of_birth) %>%
    dplyr::collect()
  
  return(genderAndYear)
}
