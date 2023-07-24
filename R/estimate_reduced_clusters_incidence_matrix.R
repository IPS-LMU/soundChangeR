estimate_reduced_clusters_incidence_matrix <- function(fullClusters, purityThreshold, params) {
  
  if (base::ncol(fullClusters) == 1) {
    return(collapsed_incidence_matrix(fullClusters))
  }
  purityMat_and_incidenceMat <- compute_purity_matrix(fullClusters, params)
  purityMat = purityMat_and_incidenceMat[[1]]
  incidenceMatrix = purityMat_and_incidenceMat[[2]]

  nClusters <- estimate_number_of_clusters_from_purity_matrix(purityMat, purityThreshold)
  if (nClusters == 1) {
    return(collapsed_incidence_matrix(fullClusters))
  }
  return(incidenceMatrix[[nClusters - 1]])
}
