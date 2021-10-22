map_classes_to_incidence_matrix <- function(classification, incidenceMatrix) {
  
  incidenceVector <- base::apply(incidenceMatrix, 2, base::which) %>%
    base::sapply(function(x) {if(base::length(x) == 0) NA else x})
  aggregatedClasses <- incidenceVector[classification]
  base::names(aggregatedClasses) <- NULL
  return(aggregatedClasses)
}
