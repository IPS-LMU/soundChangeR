update_features <- function(agent, features) {
  # features haben diese Form: [[exemplar_matrix1], [exemplar_matrix2]] bzw. [exemplar_matrix1, [0]]
  # matrizen können mit cbind aneinandergehängt werden

  if (base::nrow(cbind(features[[1]], features[[2]])) != agent$memory[valid == TRUE, .N]) {
    stop(base::paste("update_features: mismatching number of rows in given features and memory:",
                     base::nrow(features),
                     agent$memory[valid == TRUE, .N]))
  }
  nPold <- if (is.null(agent$feature_set2)) base::ncol(agent$feature_set1) else base::ncol(agent$feature_set1) + base::ncol(agent$feature_set2)
  nPnew <- base::ncol(cbind(features[[1]], features[[2]])) #cbind, da damit die exemplare aus beiden cue-spaces miteinander verbunden werden

  if (nPold > nPnew) {
    # löscht alle Spalten von nPnew+1 bis nPold --> ist noch ein Überbleibsel aus FPCA-Implementierung (kann ignoriert werden)
    agent$features[, (nPnew+1):nPold := NULL]
  }
  # agent feature werden intern in P1_1-P1_3 und P2_1 - P2_3 benannt (beim Bsp von 2 Cuespaces aus je drei DCT Koeffizienten) benannt
  if(is.null(agent$feature_set2)){
    agent$feature_set1[agent$memory$valid, base::paste0("P1_", 1:nPnew) := features[[1]] %>% base::as.data.frame()]
  }else{
    agent$feature_set1[agent$memory$valid, base::paste0("P1_", 1:as.integer(nPnew/2)) := features[[1]] %>% base::as.data.frame()]
    agent$feature_set2[agent$memory$valid, base::paste0("P2_", 1:as.integer(nPnew/2)):= features[[2]] %>% base::as.data.frame()]
  }
  #stop("test7")
  # oben: "P1_"; "P2_" derzeit hard gecoded --> später über params lösen
  #stop(paste(names(agent$feature_set1), collapse ="-"))
  set_cache_value(agent, "nFeatures", nPnew)
}
