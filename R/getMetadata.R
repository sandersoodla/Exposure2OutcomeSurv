getMetadata <- function(cdm) {
  
  dbName <- Sys.getenv("DB_NAME")
  cdmSchema <- Sys.getenv("CDM_SCHEMA")
  
  personCount <- cdm$person %>%
    dplyr::count() %>%
    dplyr::pull(n) %>%
    as.integer()
  
  minYear <- cdm$observation_period %>%
    dplyr::arrange(observation_period_start_date) %>%  # Sort by start date
    dplyr::pull(observation_period_start_date) %>%  # Extract the dates
    dplyr::first() %>% # Get the first element (min)
    format("%Y")
  
  maxYear <- cdm$observation_period %>%
    dplyr::arrange(desc(observation_period_end_date)) %>% # Sort by end date, descending
    dplyr::pull(observation_period_end_date) %>% # Extract the dates
    dplyr::first() %>% # Get the first element (max)
    format("%Y")
  
  metadata <- list(
    dbName = dbName,
    schema = cdmSchema,
    personCount = personCount,
    minYear = minYear,
    maxYear = maxYear
  )
  
  return(metadata)
}