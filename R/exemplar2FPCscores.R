exemplar2FPCscores <- function(exemplar, agent, params) {
  matrix(
    compute_FPCscores(one_exemplar2fd(exemplar),
                      get_cache_value(agent, "FPCA"),
                      get_cache_value(agent, "nFeatures")),
    nrow = 1
  )
}
