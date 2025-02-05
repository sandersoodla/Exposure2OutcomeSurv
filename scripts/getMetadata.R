getMetadata <- function(cdm) {
  
  dbName <- Sys.getenv("DB_NAME")
  cdmSchema <- Sys.getenv("CDM_SCHEMA")
  
  personCount <- cdm$person %>%
    count() %>%
    pull(n) %>%
    as.integer()
  
  minYear <- cdm$observation_period %>%
    arrange(observation_period_start_date) %>%  # Sort by start date
    pull(observation_period_start_date) %>%  # Extract the dates
    first() %>% # Get the first element (min)
    format("%Y")
  
  maxYear <- cdm$observation_period %>%
    arrange(desc(observation_period_end_date)) %>% # Sort by end date, descending
    pull(observation_period_end_date) %>% # Extract the dates
    first() %>% # Get the first element (max)
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