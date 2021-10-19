produce_token <- function(agent, params) {

  producedWord <- choose_word(agent$memory)
  if (base::is.null(producedWord)) {
    return (NULL)
  }

  producedLabel <- agent$memory$label[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]
  producedInitial <- agent$initial$initial[agent$initial$word == producedWord]

  if (base::length(producedInitial) == 0) {
    base::cat("initial for word", producedWord, "unknown to agent", agent$agentID, agent$speaker, "\n")
    producedInitial <- agent$initial$initial %>% base::unique() %>% base::sample(1)
  }
  nrOfTimesHeard <- agent$memory$nrOfTimesHeard[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  if (base::grepl("^(target)?[wW]ord$", params[["productionBasis"]])) {
    basisIdx <- base::which(agent$memory$word == producedWord & agent$memory$valid == TRUE)
  } else if (grepl("^(target)?([lL]abel|[pP]honeme)$", params[["productionBasis"]])) {
    basisIdx <- base::which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
  }
  basisTokens <- base::as.matrix(agent$features)[basisIdx, , drop = FALSE]

  if (!base::is.null(params[["productionResampling"]])) {
    if (base::grepl("SMOTE", params[["productionResampling"]], ignore.case = TRUE)) {
      nExtraTokens <- params[["productionMinTokens"]] - base::length(basisIdx)
      if (nExtraTokens > 0) {
        extendedIdx <- NULL
        if (base::grepl("label|phoneme", params[["productionResamplingFallback"]], ignore.case = TRUE)) {
          extendedIdx <- base::which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
        }
        extraTokens <- smote_resampling(agent$features, extendedIdx, basisIdx, params[["productionSMOTENN"]], nExtraTokens)
        basisTokens <- base::rbind(basisTokens, extraTokens)
      }
    } else {
      stop(base::paste("produce_token: unrecognised productionResampling method:", params[["productionResampling"]]))
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
