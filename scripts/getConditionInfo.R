

getConditionInfo<-function(connection, 
                           conditionConceptId) {
  
  if (is.na(conditionConceptId)) return (NULL)
  
  sql <- SqlRender::render("SELECT concept_name FROM concept WHERE concept_id == @id",
                           id = conditionConceptId)
  
  # TODO translate dialect
  
  result <- DatabaseConnector::querySql(connection, sql)

  return (result[[1]])
}