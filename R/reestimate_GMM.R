reestimate_GMM <- function(rawGMM, incidenceMatrix, agent, params) {
  
  aggregatedClasses <- map_classes_to_incidence_matrix(rawGMM$classification, incidenceMatrix)
  G <- base::apply(incidenceMatrix, 1, base::sum) %>% base::sapply(list)
  GMM <- mclust::MclustDA(data = rawGMM$data[!base::is.na(aggregatedClasses) ,],
                          class = aggregatedClasses %>% stats::na.exclude(),
                          G = G,
                          modelNames = get_model_names(agent, params),
                          verbose = FALSE)
  return(GMM)
}
