perceive_token <- function(agent, producedToken, interactionsLog, nrSim, params, isNotOwnToken) {

  if (is.null(producedToken)) {
    return()
  }

  perceiverLabel <- unique(agent$memory$label[agent$memory$word == producedToken$word & agent$memory$valid == TRUE])

  features <- exemplar2features(producedToken$exemplar, agent, params)

  if (length(perceiverLabel) == 0) {
    perceiverLabel <- names(which.max(table(agent$memory$label[agent$memory$valid == TRUE][
      knnx.index(agent$features[agent$memory$valid == TRUE,], features, params[["perceptionOOVNN"]])
    ])))
  }

  memorise <- TRUE
  for (strategy in params[["memoryIntakeStrategy"]]) {
    memorise <- memory_intake_strategy(strategy, producedToken$exemplar, features, perceiverLabel, agent, params)
    if (!memorise) break
  }

  if (runif(1) < params[["forgettingRate"]]) {
    candidateRow <- sample(which(agent$memory$valid == TRUE), 1)
    candidateWord <- agent$memory$word[candidateRow]
    if (sum(agent$memory$word == candidateWord & agent$memory$valid, na.rm = TRUE) >= params[["productionMinTokens"]]) {
      set(agent$memory, candidateRow, "valid", FALSE)
      set_cache_value(agent, "nForgotten", get_cache_value(agent, "nForgotten") + 1)
    }
  }

  if (memorise) {
    rowToWrite <- row_to_write(agent, producedToken, params)

    write_memory(agent, producedToken, rowToWrite, perceiverLabel)
    write_features(agent, features, rowToWrite)

    set_cache_value(agent, "nAccepted", get_cache_value(agent, "nAccepted") + 1)

    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }

  if (isNotOwnToken) {
    write_interactions_log(interactionsLog, producedToken, agent, perceiverLabel, memorise, strategy, nrSim)
  }

  if (get_cache_value(agent, "nAccepted") %% params[["computeFeaturesInterval"]] == 0) {
    update_features(agent, compute_features(agent, params))
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }

  if (grepl("^GMM(s)?", params[["perceptionModels"]]) && get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    estimate_GMM(agent, params)
  }

  if (params[["splitAndMerge"]] == T && get_cache_value(agent, "nAccepted") %% params[["splitAndMergeInterval"]] == 0) {
    splitandmerge(agent, params, full = FALSE)
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }
}
