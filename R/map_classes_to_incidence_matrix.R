map_classes_to_incidence_matrix <- function(classification, incidenceMatrix) {
  incidenceVector <- apply(incidenceMatrix, 2, which) %>%
    sapply(function(x) {if(length(x) == 0) NA else x})
  aggregatedClasses <- incidenceVector[classification]
  names(aggregatedClasses) <- NULL
  return(aggregatedClasses)
}
