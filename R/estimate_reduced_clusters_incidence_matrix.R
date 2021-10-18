estimate_reduced_clusters_incidence_matrix <- function(fullClusters, purityRepetitions, purityThreshold) {
  if (ncol(fullClusters) == 1) {
    return(collapsed_incidence_matrix(fullClusters))
  }
  purityMat <- compute_purity_matrix(fullClusters, purityRepetitions)
  nClusters <- estimate_number_of_clusters_from_purity_matrix(purityMat, purityThreshold)
  if (nClusters == 1) {
    return(collapsed_incidence_matrix(fullClusters))
  }
  while({incidenceMatrix <- reduced_clusters_incidence_matrix(fullClusters, nClusters);
  get_reduced_clusters(fullClusters, incidenceMatrix) %>% compute_purity %>% is.na}) {}
  return(incidenceMatrix)
}
