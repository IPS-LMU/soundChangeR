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
    stop(paste(names(agent$memory), collapse = "------"))
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
 
   # bis hierhin kein Fehler ( sowohl mit 1 Cuespace als auch mit 2 Cuespace)
   # Variable ist auch gefüllt
  #stop(GMM$d)
  #stop(head(summary(GMM)))

  #jpeg('GMM_plot_estGMM_mit2Cuespaces_classification_kl.jpg')
  #plot(GMM, what = "classification")
  #dev.off()
  #stop("Bild fertig")
  
  dim_set = names(agent)[startsWith(names(agent), "feature_set")]
  dim_set =  substr(dim_set,nchar(dim_set)-4, nchar(dim_set))
  # So sind alle Subphoneme in separaten Cuespaces
  wordLabels <- assign_words_to_phonemes(reducedWordClusters)

  agent$memory[wordLabels, on = "word", paste0("phoneme",dim_set) := i.phoneme]
 
}
