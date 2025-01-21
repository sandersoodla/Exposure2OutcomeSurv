source("scripts/conditionToCondition.R")


createPopulationPyramidForCondition <- function(connection, conditionConceptID) {

  sql <- 
    SqlRender::render("SELECT c.person_id,
             c.condition_start_date,
             p.gender_concept_id,
             p.birth_datetime
      FROM condition_occurrence c
      JOIN person p ON c.person_id = p.person_id
      WHERE c.condition_concept_id = @condition_concept_id",
      condition_concept_id = conditionConceptID)
  
  data <- DatabaseConnector::querySql(connection, sql)
  
  # Calculate age at condition occurrence
  data$AGE <- floor(as.numeric(difftime(data$CONDITION_START_DATE, data$BIRTH_DATETIME, units = "days")) / 365.25)
  
  # Map gender_concept_id to gender labels
  data$GENDER <- ifelse(data$GENDER_CONCEPT_ID == 8507, "Male",
                        ifelse(data$GENDER_CONCEPT_ID == 8532, "Female", "Other"))
  
  # Aggregate counts by age and gender
  age_gender_counts <- data %>%
    group_by(AGE, GENDER) %>%
    summarise(COUNT = n()) %>%
    ungroup()
  
  
  # Negative values for Male gender 
  age_gender_counts <- age_gender_counts %>%
    mutate(COUNT = ifelse(GENDER == "Male", -COUNT, COUNT))
  
  
  # Plot the population pyramid
  pyramid_plot <- ggplot(age_gender_counts, aes(x = AGE, y = COUNT, fill = GENDER)) +
    geom_bar(data = subset(age_gender_counts, GENDER == "Male"), stat = "identity") +
    geom_bar(data = subset(age_gender_counts, GENDER == "Female"), stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = abs) +
    labs(title = paste("Population Pyramid for Condition Concept ID", conditionConceptID),
         x = "Age",
         y = "Number of Occurrences") +
    theme_minimal()
  
  return (pyramid_plot)
}


getPersonInfo <- function(connection, personID) {
  
  sqlPersonInfo <- 
    SqlRender::render("SELECT person_id, gender_concept_id, birth_datetime
                      FROM person
                      WHERE person_id = @id",
                      id = personID)
  
  personInfo <- DatabaseConnector::querySql(connection, sqlPersonInfo)
  
  return (personInfo)
}