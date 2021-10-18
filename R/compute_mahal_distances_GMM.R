compute_mahal_distances_GMM <- function(GMM, features) {
  
  if (GMM$d == 1) {
    purrr::map2(GMM$parameters$mean,
                GMM$parameters$variance$sigmasq,
                ~ stats::mahalanobis(features, .x, .y, tol = 1e-20)) %>% base::unlist()
  } else {
    purrr::map2(base::lapply(1:GMM$G, function(g) {GMM$parameters$mean[,g] %>% base::t()}),
                base::lapply(1:GMM$G, function(g) {GMM$parameters$variance$sigma[,,g] %>% water_filling()}),
                ~ stats::mahalanobis(features, .x, .y)) %>% base::unlist()
  }
}
