library(dplyr)

getObservationPeriods <- function(cdm, personIds) {
  processedPersonIds <- unique(as.numeric(personIds))
  
  obsPeriods <- cdm$observation_period %>%
    filter(person_id %in% processedPersonIds) %>%
    select(person_id, 
           obs_start_date = observation_period_start_date,
           obs_end_date = observation_period_end_date) %>%
    collect()
  
  return(obsPeriods)
}

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