mahalanobis_distance <- function(exemplar, features, phoneme, agent, params) {
  
  mahalDist <- compute_mahal_distance(agent, features, phoneme, params)
  mahalDist <= stats::qchisq(p = params[["mahalanobisProbThreshold"]], df = get_cache_value(agent, "nFeatures"))
}
