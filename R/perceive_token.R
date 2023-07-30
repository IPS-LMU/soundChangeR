perceive_token <- function(agent, producedToken, interactionsLog, nrSim, params) {

  if (base::is.null(producedToken)) {
    return()
  }

  perceiverPhoneme_set1 <- base::unique(agent$memory$phoneme_set1[agent$memory$word == producedToken$word & agent$memory$valid == TRUE])
  perceiverPhoneme_set2 <- base::unique(agent$memory$phoneme_set2[agent$memory$word == producedToken$word & agent$memory$valid == TRUE])
# TODO (vanessa): Perceiverphoneme im folgenden anpassen
# Ausnahme: write_memory, das sollte angepasst sein (aber noch nicht getestet)


  features <- exemplar2features(producedToken$exemplar, agent, params)

  if (base::length(perceiverPhoneme) == 0) {
    perceiverPhoneme <- base::names(base::which.max(base::table(agent$memory$phoneme[agent$memory$valid == TRUE][
      FNN::knnx.index(agent$features[agent$memory$valid == TRUE,], features, params[["perceptionOOVNN"]])
    ])))
  }

  memorise <- TRUE
  for (strategy in params[["memoryIntakeStrategy"]]) {
    memorise <- memory_intake_strategy(strategy, producedToken$exemplar, features, perceiverPhoneme, agent, params)
    # memorise enthält die Aussage, ob das Token memorisiert werden soll oder nicht (hat also den Wert True/False?)
    if (!memorise) break
  }

  if (stats::runif(1) < params[["forgettingRate"]]) {
    candidateRow <- base::sample(base::which(agent$memory$valid == TRUE), 1)
    candidateWord <- agent$memory$word[candidateRow]
    # überprüfen, ob noch genügend Token vorhanden sind, wenn ein Token dieses Wortes geschlöscht wir --> falls ja, wird das Exemplar geslöscht
    if (base::sum(agent$memory$word == candidateWord & agent$memory$valid, na.rm = TRUE) >= params[["minTokens"]]) {
      data.table::set(agent$memory, candidateRow, "valid", FALSE)
      set_cache_value(agent, "nForgotten", get_cache_value(agent, "nForgotten") + 1)
    }
  }

  if (memorise) {
    rowToWrite <- row_to_write(agent, producedToken, params)
    # Vanessa: hier wurde perceiver_phoneme schon angepasst
    write_memory(agent, params, producedToken, rowToWrite, c(perceiverPhoneme_set1, perceiverPhoneme_set2))
    set_cache_value(agent, "nAccepted", get_cache_value(agent, "nAccepted") + 1)
    if (base::any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr"))) {
      invalidate_cache(agent, "qda")
    }
  }

  write_interactions_log(interactionsLog, producedToken, agent, perceiverPhoneme, memorise, strategy, features, nrSim)

  if (get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    update_features(agent, compute_features(agent, params))
    if (base::any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr"))) {
      invalidate_cache(agent, "qda")
    }
  }

  if (params[["useFlexiblePhonology"]] && get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    estimate_GMM(agent, params)
  }
}
