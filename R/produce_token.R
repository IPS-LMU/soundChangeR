produce_token <- function(agent, params) {

  producedWord <- choose_word(agent$memory)
  if (base::is.null(producedWord)) {
    return (NULL)
  }

  producedLabel <- agent$memory$label[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]
  producedInitial <- agent$initial$initial[agent$initial$word == producedWord]

  if (base::length(producedInitial) == 0) {
    write_log(paste("initial for word", producedWord, "unknown to agent", agent$agentID, agent$speaker), params)
    producedInitial <- agent$initial$initial %>% base::unique() %>% base::sample(1)
  }
  nrOfTimesHeard <- agent$memory$nrOfTimesHeard[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  if (params[["productionBasis"]] == "word") {
    basisIdx <- base::which(agent$memory$word == producedWord & agent$memory$valid == TRUE)
  } else if (params[["productionBasis"]] == "label") {
    basisIdx <- base::which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
  }
  basisTokens <- base::as.matrix(agent$features)[basisIdx, , drop = FALSE]

  if (params[["productionResampling"]]) {
    nExtraTokens <- params[["productionMinTokens"]] - base::length(basisIdx)
    if (nExtraTokens > 0) {
      extendedIdx <- NULL
      if (params[["productionResamplingFallback"]] == "label") {
        extendedIdx <- base::which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
      }
      extraTokens <- smote_resampling(agent$features, extendedIdx, basisIdx, params[["productionSMOTENN"]], nExtraTokens)
      basisTokens <- base::rbind(basisTokens, extraTokens)
    }
  }
  gaussParams <- estimate_gaussian(basisTokens)

  features <- mvtnorm::rmvnorm(1, gaussParams$mean, gaussParams$cov)
  producedToken <- data.table::data.table(word = producedWord,
                                          label = producedLabel,
                                          initial = producedInitial,
                                          exemplar = features2exemplar(features, agent, params),
                                          nrOfTimesHeard = nrOfTimesHeard,
                                          producerID = agent$agentID)
  return(producedToken)
}
