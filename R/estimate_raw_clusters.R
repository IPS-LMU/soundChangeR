estimate_raw_clusters <- function(agent, params) {
  Mclust(as.matrix(agent$features)[agent$memory$valid, , drop = FALSE],
         modelNames = get_model_names(agent, params))
}
