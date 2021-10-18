get_mean_cov_from_GMM_component <- function(GMM, GIdx) {

  if (GMM$d == 1) {
    list(mean = GMM$parameters$mean[GIdx],
         cov =  matrix(GMM$parameters$variance$sigmasq[GIdx]))
  } else {
    list(mean = GMM$parameters$mean[, GIdx] %>% t,
         cov =  GMM$parameters$variance$sigma[, , GIdx])
  }
}
