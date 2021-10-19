reduced_clusters_incidence_matrix <- function(fullClusters, rank) {
  
  if (base::any(fullClusters < 0)) {stop("reduced_word_clusters_incidence_matrix: fullClusters values cannot be negative")}
  if (rank > base::ncol(fullClusters)) {stop("reduced_clusters_incidence_matrix: rank cannot exceed number of columns of fullClusters")}
  zeroCols <- base::apply(fullClusters, 2, base::sum) == 0
  if (rank > base::ncol(fullClusters) - base::sum(zeroCols)) {stop("reduced_clusters_incidence_matrix: too many zero columns")}
  if (base::sum(zeroCols) > 0) {fullClusters <- fullClusters[, !zeroCols]}
  if (rank == 1) {
    incidenceMatrix <- base::matrix(TRUE, nrow = 1, ncol = base::ncol(fullClusters))
  } else {
    nmfObj <- NMF::nmf(x = fullClusters, rank = rank, method = "nsNMF")
    incidenceMatrix <- nmfObj %>% NMF::coef() %>% base::apply(2, logical_max)
  }
  if (base::sum(zeroCols) == 0) {
    return(incidenceMatrix)
  } else {
    incidenceMatrixZeroCols <- base::matrix(FALSE, nrow = rank, ncol = base::length(zeroCols))
    incidenceMatrixZeroCols[, !zeroCols] <- incidenceMatrix
    return(incidenceMatrixZeroCols)
  }
}
