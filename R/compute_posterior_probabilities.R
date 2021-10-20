compute_posterior_probabilities <- function(agent, features, method) {

  if (method == "qda") {
    if (!is_cache_valid(agent, "qda")) {
      update_cache(agent, "qda", compute_qda)
    }
    stats::predict(get_cache_value(agent, "qda"), features)$posterior
  } else if (method == "GMM") {
    stats::predict(get_cache_value(agent, "GMM"), features)$z[1, , drop=FALSE]
  } else {
    NULL
  }
}
