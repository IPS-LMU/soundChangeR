get_mean_cov_from_GMM_component <- function(GMM, GIdx) {

  if (GMM$d == 1) {
    base::list(mean = GMM$parameters$mean[GIdx],
               cov = base::matrix(GMM$parameters$variance$sigmasq[GIdx]))
  } else {
    base::list(mean = GMM$parameters$mean[, GIdx] %>% base::t(),
               cov = GMM$parameters$variance$sigma[, , GIdx])
  }
}
