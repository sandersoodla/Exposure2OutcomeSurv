library(dplyr)

getAllTrajectories <- function(cdm) {
  
  trajectories <- cdm$condition_occurrence %>%
    inner_join(cdm$concept, by = c("condition_concept_id" = "concept_id"), keep = TRUE) %>%
    select(person_id, concept_id, concept_name, condition_start_date) %>%
    arrange(person_id, condition_start_date) %>%
    collect()
  
  return(trajectories)
}

# maybe move getTrajectoriesForCondition here too