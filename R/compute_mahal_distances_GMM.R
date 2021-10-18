compute_mahal_distances_GMM <- function(GMM, features) {
  if (GMM$d == 1) {
    map2(GMM$parameters$mean,
         GMM$parameters$variance$sigmasq,
         ~ mahalanobis(features, .x, .y, tol = 1e-20)) %>% unlist
  } else {
    map2(lapply(1:GMM$G, function(g) {GMM$parameters$mean[,g] %>% t}),
         lapply(1:GMM$G, function(g) {GMM$parameters$variance$sigma[,,g] %>% water_filling}),
         ~ mahalanobis(features, .x, .y)) %>% unlist
  }
}
