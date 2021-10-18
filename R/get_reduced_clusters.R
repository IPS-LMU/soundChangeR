get_reduced_clusters <- function(fullClusters, incidenceMatrix) {
  fullClusters %*% t(incidenceMatrix)
}
