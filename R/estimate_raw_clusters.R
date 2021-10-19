estimate_raw_clusters <- function(agent, params) {
  mclust::Mclust(base::as.matrix(agent$features)[agent$memory$valid, , drop = FALSE],
                 modelNames = get_model_names(agent, params))
}
