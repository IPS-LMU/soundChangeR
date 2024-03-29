get_method_register <- function() {
	
  methodReg <- data.table::rbindlist(base::list(
    data.table::data.table(
      method = "identity",
      compute_features = exemplar2matrix,
      exemplar2features = exemplar2matrix,
      features2exemplar = rowMatrix2exemplar,
      memoryIntakeStrategy = base::list(base::list(
        acceptAll = accept_all,
        mahalanobisDistance = mahalanobis_distance,
        maxPosteriorProb = max_posterior_prob,
        posteriorProbThr = posterior_prob_thr
      )),
      cacheEntries = base::list(NA_character_)
    )
  )) %>% data.table::setkey(method)
  return(methodReg)
}
