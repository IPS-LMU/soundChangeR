update_features <- function(agent, features) {
  if (nrow(features) != agent$memory[valid == TRUE, .N]) {
    stop(paste("update_features: mismatching number of rows in given features and memory:",
               nrow(features),
               agent$memory[valid == TRUE, .N]))
  }
  nPold <- ncol(agent$features)
  nPnew <- ncol(features)
  if (nPold > nPnew) {
    agent$features[, (nPnew+1):nPold := NULL]
  }
  agent$features[agent$memory$valid, paste0("P", 1:nPnew) := features %>% as.data.frame]
  set_cache_value(agent, "nFeatures", nPnew)
}
