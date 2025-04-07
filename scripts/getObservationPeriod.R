library(dplyr)

getMaxObservationDates <- function(cdm, personIds) {
  if (length(personIds) == 0) return(as.Date(NA))
  
  processedPersonIds <- unique(as.numeric(personIds))
  
  individualDates <- cdm$observation_period %>%
    filter(person_id %in% processedPersonIds) %>%
    group_by(person_id) %>%
    summarise(max_date = max(observation_period_end_date, na.rm = TRUE), .groups = "drop") %>%
    collect()
  
  return(individualDates)
}