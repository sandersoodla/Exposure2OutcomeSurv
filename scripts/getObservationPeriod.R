library(dplyr)

getMaxObservationEndDate <- function(cdm, personIds) {
  if (length(personIds) == 0) return(as.Date(NA))
  
  max_date <- cdm$observation_period %>%
    filter(person_id %in% personIds) %>%
    summarise(max_date = max(observation_period_end_date, na.rm = TRUE)) %>%
    pull(max_date) %>%
    first() # Get the single value
  
  return(max_date)
}