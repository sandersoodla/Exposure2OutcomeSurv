
getObservationPeriods <- function(cdm, personIds) {
  processedPersonIds <- unique(as.numeric(personIds))
  
  obsPeriods <- cdm$observation_period %>%
    dplyr::filter(person_id %in% processedPersonIds) %>%
    dplyr::select(person_id, 
           obs_start_date = observation_period_start_date,
           obs_end_date = observation_period_end_date) %>%
    dplyr::collect()
  
  return(obsPeriods)
}

getMaxObservationDates <- function(cdm, personIds) {
  if (length(personIds) == 0) return(as.Date(NA))
  
  processedPersonIds <- unique(as.numeric(personIds))
  
  individualDates <- cdm$observation_period %>%
    dplyr::filter(person_id %in% processedPersonIds) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarise(max_date = max(observation_period_end_date, na.rm = TRUE), .groups = "drop") %>%
    dplyr::collect()
  
  return(individualDates)
}