estimate_GMM <- function(agent, params) {
  
  write_log(agent$speaker, params)
  rawGMM <- estimate_raw_clusters(agent, params)
  fullWordClusters <- get_full_word_clusters(rawGMM, agent, params)
  reducedWordClustersIncidenceMatrix <- estimate_reduced_clusters_incidence_matrix(
    fullWordClusters, params[["purityRepetitions"]], params[["purityThreshold"]])
  reducedWordClusters <- get_reduced_clusters(fullWordClusters, reducedWordClustersIncidenceMatrix)
  excludedClass <- reducedWordClusters %>% base::apply(2, base::sum)
  excludedClassIdx <- (excludedClass < rawGMM$d) %>% base::which()
  if (base::length(excludedClassIdx) > 0) {
    write_log(base::paste("excludedClassIdx", excludedClassIdx), params)
    reducedWordClustersIncidenceMatrix <- reducedWordClustersIncidenceMatrix[-excludedClassIdx, , drop = FALSE]
    reducedWordClusters <- reducedWordClusters[, -excludedClassIdx, drop = FALSE]
    excludedTokenIdx <- map_classes_to_incidence_matrix(rawGMM$classification, reducedWordClustersIncidenceMatrix) %>%
      base::is.na() %>% base::which()
    agent$memory[base::which(agent$memory$valid)[excludedTokenIdx], valid := FALSE]
    write_log(base::paste("excludedTokenIdx", excludedTokenIdx), params)
  }
  tryCatch({
    GMM <- reestimate_GMM(rawGMM, reducedWordClustersIncidenceMatrix, agent, params)
  }, error = function(c) {
    write_log(base::paste(base::conditionMessage(c), base::conditionCall(c), sep = "\n"), params)
    dump_obj(rawGMM, "rawGMM", params)
    dump_obj(reducedWordClustersIncidenceMatrix, "reducedWordClustersIncidenceMatrix", params)
    stop(c)
  })
  set_cache_value(agent, "GMM", GMM)
  
  wordLabels <- assign_words_to_phonemes(reducedWordClusters)
  agent$memory[wordLabels, on = "word", phoneme := i.phoneme]
}
