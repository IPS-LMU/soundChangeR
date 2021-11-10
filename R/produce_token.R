produce_token <- function(agent, params) {

  producedWord <- choose_word(agent$memory)
  if (base::is.null(producedWord)) {
    return (NULL)
  }

  producedPhoneme <- agent$memory$phoneme[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  # if (base::length(producedInitial) == 0) {
  #   write_log(paste("initial for word", producedWord, "unknown to agent", agent$agentID, agent$speaker), params)
  #   producedInitial <- agent$initial$initial %>% base::unique() %>% base::sample(1)
  # }
  nrOfTimesHeard <- agent$memory$nrOfTimesHeard[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  basisIdx <- base::which(agent$memory$word == producedWord & agent$memory$valid == TRUE)
  basisTokens <- base::as.matrix(agent$features)[basisIdx, , drop = FALSE]

  if (params[["useSMOTE"]]) {
    nExtraTokens <- params[["minTokens"]] - base::length(basisIdx)
    if (nExtraTokens > 0) {
      extendedIdx <- NULL
      if (params[["fallBackOnPhoneme"]]) {
        extendedIdx <- base::which(agent$memory$phoneme == producedPhoneme & agent$memory$valid == TRUE)
      }
      extraTokens <- smote_resampling(agent$features, extendedIdx, basisIdx, params[["SMOTENN"]], nExtraTokens)
      basisTokens <- base::rbind(basisTokens, extraTokens)
    }
  }
  gaussParams <- estimate_gaussian(basisTokens)

  features <- mvtnorm::rmvnorm(1, gaussParams$mean, gaussParams$cov)
  producedToken <- data.table::data.table(word = producedWord,
                                          phoneme = producedPhoneme,
                                          exemplar = features2exemplar(features, agent, params),
                                          nrOfTimesHeard = nrOfTimesHeard,
                                          producerID = agent$agentID)
  return(producedToken)
}
