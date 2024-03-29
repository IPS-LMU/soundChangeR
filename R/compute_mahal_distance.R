compute_mahal_distance <- function(agent, features, phoneme, params) {
  
  if (!params[["useFlexiblePhonology"]]) {
    stats::mahalanobis(features,
                       base::apply(base::as.matrix(agent$features)[agent$memory$valid == TRUE & 
                                                                     agent$memory$phoneme == phoneme, , drop = FALSE], 2, base::mean),
                       stats::cov(base::as.matrix(agent$features)[agent$memory$valid == TRUE &
                                                                    agent$memory$phoneme == phoneme, , drop = FALSE]), tol = 1e-100)

  } else {
    GMM <- get_cache_value(agent, "GMM")

    tryCatch({
      compute_mahal_distances_GMM(GMM$models[[phoneme]], features) %>% base::min()
    }, error = function(c) {
      write_log(base::paste("compute_mahal_distances_GMM", "phoneme", phoneme, conditionMessage(c), 
                            base::paste(conditionCall(c), collapse = " "), sep = "\n"), params)
      dump_obj(GMM, "GMM", params)
      stop(c)
    })
  }
}
