compute_purity <- function(clustersMat, summaryFunc = base::min) {
  
  whichMax <- clustersMat %>% base::apply(1, base::which.max)
  base::sapply(base::seq_len(base::ncol(clustersMat)), function(j) {
    base::sum(clustersMat[whichMax == j, j]) / base::sum(clustersMat[, j])
  }) %>% summaryFunc()
}
