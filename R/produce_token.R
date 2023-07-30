produce_token <- function(agent, params) {

  producedWord <- choose_word(agent$memory)
  if (base::is.null(producedWord)) {
    return (NULL)
  }
  producedPhoneme_set1 <- agent$memory$phoneme_set1[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]
  producedPhoneme_set2 <- agent$memory$phoneme_set2[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  # if (base::length(producedInitial) == 0) {
  #   write_log(paste("initial for word", producedWord, "unknown to agent", agent$agentID, agent$speaker), params)
  #   producedInitial <- agent$initial$initial %>% base::unique() %>% base::sample(1)
  # }
  nrOfTimesHeard <- agent$memory$nrOfTimesHeard[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  basisIdx <- base::which(agent$memory$word == producedWord & agent$memory$valid == TRUE)
  basisTokens <- base::as.matrix(cbind(agent$feature_set1, agent$feature_set2))[basisIdx, , drop = FALSE]

  if (params[["useSMOTE"]]) {
    nExtraTokens <- params[["minTokens"]] - base::length(basisIdx)
    if (nExtraTokens > 0) {
      extendedIdx <- NULL
      if (params[["fallBackOnPhoneme"]]) { #### nicht sicher, ob die anpassung stimmt
        extendedIdx <- base::which(agent$memory$phoneme_set1 == producedPhoneme_set1 & agent$memory$phoneme_set2 == producedPhoneme_set2 & agent$memory$valid == TRUE)
      }
      extraTokens <- smote_resampling(cbind(agent$feature_set1, agent$feature_set2), extendedIdx, basisIdx, params[["SMOTENN"]], nExtraTokens)
      basisTokens <- base::rbind(basisTokens, extraTokens)
    }
  }
  gaussParams <- estimate_gaussian(basisTokens)

  features <- mvtnorm::rmvnorm(1, gaussParams$mean, gaussParams$cov)
  producedToken <- data.table::data.table(word = producedWord,
                                          phoneme_set1 = producedPhoneme_set1,
                                          phoneme_set2 = producedPhoneme_set2,
                                          exemplar = features2exemplar(features, agent, params),
                                          nrOfTimesHeard = nrOfTimesHeard,
                                          producerID = agent$agentID)
  
  
  return(producedToken)
}
