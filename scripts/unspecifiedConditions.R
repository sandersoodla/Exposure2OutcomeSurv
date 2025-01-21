library(DatabaseConnector)
library(SqlRender)

getUnspecifiedConditionOccurrenceCount <- function(connection, filename) {
  
  unclassifiedConditions <- read.csv(filename, sep = "\t")
  
  sqlConditionsCount <- "SELECT concept_id, COUNT(concept_id), concept_name, vocabulary_id, concept_code, domain_id 
                                    FROM condition_occurrence
                                    JOIN concept ON condition_concept_id == concept_id
                                    GROUP BY concept_id
                                    HAVING COUNT(concept_id) >= 100"
  
  conditionsCount <- querySql(connection, render(sqlConditionsCount))
  
  unclassifiedInData <- merge(conditionsCount, unclassifiedConditions, by.x = "CONCEPT_ID", by.y = "Id")
  
  return (unclassifiedInData)
}


connection <- DatabaseConnector::connect(dbms = "sqlite", server = "~/GitHub/thesis/synthea/synthea100k.sqlite")
filename <- "Athena search.csv"

data <- getUnspecifiedConditionOccurrenceCount(connection, filename)