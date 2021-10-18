compute_purity_matrix <- function(fullClusters, reps) {
  purity <- base::matrix(0, nrow = base::ncol(fullClusters)-1, ncol = reps)
  for (r in 2:base::ncol(fullClusters)) {
    for (rr in 1:reps) {
      purity[r-1, rr] <- reduced_clusters_incidence_matrix(fullClusters, r) %>%
        get_reduced_clusters(fullClusters, .) %>%
        compute_purity()
    }
  }
  return(purity)
}
