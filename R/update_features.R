update_features <- function(agent, features) {
  
  if (base::nrow(features) != agent$memory[valid == TRUE, .N]) {
    stop(base::paste("update_features: mismatching number of rows in given features and memory:",
                     base::nrow(features),
                     agent$memory[valid == TRUE, .N]))
  }
  nPold <- base::ncol(agent$features)
  nPnew <- base::ncol(features)
  if (nPold > nPnew) {
    agent$features[, (nPnew+1):nPold := NULL]
  }
  agent$features[agent$memory$valid, base::paste0("P", 1:nPnew) := features %>% base::as.data.frame()]
  set_cache_value(agent, "nFeatures", nPnew)
}
