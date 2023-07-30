mahalanobis_distance <- function(exemplar, features, phoneme, agent, params) {
  
  mahalDist <- compute_mahal_distance(agent, features, phoneme, params)
  # Warum wird hier die quantil-function benötigt?
  # qchisq gibt die quantile zurück
  mahalDist <= stats::qchisq(p = params[["mahalanobisProbThreshold"]], df = get_cache_value(agent, "nFeatures"))
}
