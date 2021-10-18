reestimate_GMM <- function(rawGMM, incidenceMatrix, agent, params) {
  aggregatedClasses <- map_classes_to_incidence_matrix(rawGMM$classification, incidenceMatrix)
  G <- apply(incidenceMatrix, 1, sum) %>% sapply(list)
  GMM <- MclustDA(data = rawGMM$data[!is.na(aggregatedClasses) ,],
                  class = aggregatedClasses %>% na.exclude,
                  G = G,
                  modelNames = get_model_names(agent, params))
  return(GMM)
}
