library(lubridate)

createPopulationPyramidForCondition <- function(cdm, conditionConceptId) {
  
  data <- cdm$condition_occurrence %>%
    filter(condition_concept_id == conditionConceptId) %>%
    inner_join(cdm$person, by = "person_id") %>%
    select(person_id, gender_concept_id, condition_start_datetime, birth_datetime) %>%
    collect()
  
  # Calculate age at condition occurrence
  data <- data %>%
    mutate(age_at_occurrence = floor(time_length(condition_start_datetime - birth_datetime, unit = "years")))
  
  # Map gender_concept_id to gender labels
  data <- data %>%
    mutate(gender = case_when(
      gender_concept_id == 8507 ~ "Male",
      gender_concept_id == 8532 ~ "Female",
      TRUE ~ "Other"
    ))
  
  # Aggregate counts by age and gender
  ageGenderCounts <- data %>%
    group_by(age_at_occurrence, gender) %>%
    summarize(count = n(),.groups = "drop") %>%
    ungroup()
  
  # Negative values for Male gender
  ageGenderCounts <- ageGenderCounts %>%
    mutate(count = ifelse(gender == "Male", -count, count))
  
  # Plot the population pyramid (ggplot2)
  library(ggplot2)
  pyramidPlot <- ggplot(ageGenderCounts, aes(x = age_at_occurrence, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = abs) +
    labs(title = paste("Population Pyramid for Condition Concept ID", conditionConceptId),
         x = "Age",
         y = "Number of Occurrences") +
    theme_minimal()
  
  return(pyramidPlot)
}


getPersonInfo <- function(cdm, personId) {
  
  personInfo <- cdm$person %>%
    filter(person_id == personId) %>%
    collect()
  
  return(personInfo)
}
