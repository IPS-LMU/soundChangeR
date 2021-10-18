estimate_GMM <- function(agent, params) {
  write_log(agent$speaker, agent, params)
  rawGMM <- estimate_raw_clusters(agent, params)
  fullWordClusters <- get_full_word_clusters(rawGMM, agent, params)
  reducedWordClustersIncidenceMatrix <- estimate_reduced_clusters_incidence_matrix(
    fullWordClusters, params[["purityRepetitions"]], params[["purityThreshold"]])
  reducedWordClusters <- get_reduced_clusters(fullWordClusters, reducedWordClustersIncidenceMatrix)
  
  excludedClassIdx <- reducedWordClusters %>% apply(2, sum) %>% `<`(rawGMM$d) %>% which
  if (length(excludedClassIdx) > 0) {
    write_log(paste("excludedClassIdx", excludedClassIdx),  agent, params)
    reducedWordClustersIncidenceMatrix <- reducedWordClustersIncidenceMatrix[-excludedClassIdx, , drop = FALSE]
    reducedWordClusters <- reducedWordClusters[, -excludedClassIdx, drop = FALSE]
    excludedTokenIdx <- map_classes_to_incidence_matrix(rawGMM$classification, reducedWordClustersIncidenceMatrix) %>%
      is.na %>% which
    agent$memory[which(agent$memory$valid)[excludedTokenIdx], valid := FALSE]
    write_log(paste("excludedTokenIdx", excludedTokenIdx), agent, params)
  }
  tryCatch({
    GMM <- reestimate_GMM(rawGMM, reducedWordClustersIncidenceMatrix, agent, params)
  }, error = function(c) {
    write_log(paste(conditionMessage(c), conditionCall(c), sep = "\n"), agent, params)
    dump_obj(rawGMM, "rawGMM", agent, params)
    dump_obj(reducedWordClustersIncidenceMatrix, "reducedWordClustersIncidenceMatrix", agent, params)
    stop(c)
  })
  set_cache_value(agent, "GMM", GMM)
  
  wordLabels <- assign_words_to_labels(reducedWordClusters)
  agent$memory[wordLabels, on = "word", label := i.label]
}
