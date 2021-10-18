reduced_clusters_incidence_matrix <- function(fullClusters, rank) {
  if (any(fullClusters <0)) {stop("reduced_word_clusters_incidence_matrix: fullClusters values cannot be negative")}
  if (rank > ncol(fullClusters)) {stop("reduced_clusters_incidence_matrix: rank cannot exceed number of columns of fullClusters")}
  zeroCols <- apply(fullClusters, 2, sum) == 0
  if (rank > ncol(fullClusters) - sum(zeroCols)) {stop("reduced_clusters_incidence_matrix: too many zero columns")}
  if (sum(zeroCols) > 0) {fullClusters <- fullClusters[, !zeroCols]}
  if (rank == 1) {
    incidenceMatrix <- matrix(TRUE, nrow = 1, ncol = ncol(fullClusters))
  } else {
    nmfObj <- nmf(x = fullClusters, rank = rank, method = "nsNMF")
    incidenceMatrix <- nmfObj %>% coef() %>%  apply(2, logical_max)
  }
  if (sum(zeroCols) == 0) {
    return(incidenceMatrix)
  } else {
    incidenceMatrixZeroCols <- matrix(FALSE, nrow = rank, ncol = length(zeroCols))
    incidenceMatrixZeroCols[, !zeroCols] <- incidenceMatrix
    return(incidenceMatrixZeroCols)
  }
}
