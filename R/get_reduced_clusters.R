get_reduced_clusters <- function(fullClusters, incidenceMatrix) {
  
  fullClusters %*% base::t(incidenceMatrix)
}
