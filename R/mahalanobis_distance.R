mahalanobis_distance <- function(exemplar, features, label, agent, params) {
  
  mahalDist <- compute_mahal_distance(agent, features, label, params[["perceptionModels"]])
  mahalDist <= stats::qchisq(p = params[["mahalanobisProbThreshold"]], df = get_cache_value(agent, "nFeatures"))
}
