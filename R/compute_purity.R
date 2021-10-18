compute_purity <- function(clustersMat, summaryFunc = min) {
  whichMax <- clustersMat %>% apply(1, which.max)
  sapply(seq_len(ncol(clustersMat)), function(j) {
    sum(clustersMat[whichMax == j, j]) / sum(clustersMat[, j])
  }) %>% summaryFunc
}
