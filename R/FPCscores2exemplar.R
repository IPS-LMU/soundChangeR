FPCscores2exemplar <- function(features, agent, params) {
  one_fd2exemplar(
    FPCscores2fd(features, get_cache_value(agent, "FPCA"))
  )
}
