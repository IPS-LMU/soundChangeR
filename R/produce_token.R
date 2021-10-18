produce_token <- function(agent, params) {

  producedWord <- choose_word(agent$memory)
  if (is.null(producedWord)) {
    return (NULL)
  }

  producedLabel <- agent$memory$label[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]
  producedInitial <- agent$initial$initial[agent$initial$word == producedWord]

  if (length(producedInitial) == 0) {
    cat("initial for word", producedWord, "unknown to agent", agent$agentID, agent$speaker, "\n")
    producedInitial <- agent$initial$initial %>% unique %>% sample(1)
  }
  nrOfTimesHeard <- agent$memory$nrOfTimesHeard[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  if (grepl("^(target)?[wW]ord$", params[["productionBasis"]])) {
    basisIdx <- which(agent$memory$word == producedWord & agent$memory$valid == TRUE)
  } else if (grepl("^(target)?([lL]abel|[pP]honeme)$", params[["productionBasis"]])) {
    basisIdx <- which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
  }
  basisTokens <- as.matrix(agent$features)[basisIdx, , drop = FALSE]

  if (!is.null(params[["productionResampling"]])) {
    if (grepl("SMOTE", params[["productionResampling"]], ignore.case = TRUE)) {
      nExtraTokens <- params[["productionMinTokens"]] - length(basisIdx)
      if (nExtraTokens > 0) {
        extendedIdx <- NULL
        if (grepl("label|phoneme", params[["productionResamplingFallback"]], ignore.case = TRUE)) {
          extendedIdx <- which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
        }
        extraTokens <- smote_resampling(agent$features, extendedIdx, basisIdx, params[["productionSMOTENN"]], nExtraTokens)
        basisTokens <- rbind(basisTokens, extraTokens)
      }
    } else {
      stop(paste("produce_token: unrecognised productionResampling method:", params[["productionResampling"]]))
    }
  }
  gaussParams <- estimate_gaussian(basisTokens)

  features <- rmvnorm(1, gaussParams$mean, gaussParams$cov)
  producedToken <- data.table(word = producedWord,
                              label = producedLabel,
                              initial = producedInitial,
                              exemplar = features2exemplar(features, agent, params),
                              nrOfTimesHeard = nrOfTimesHeard,
                              producerID = agent$agentID)
  return(producedToken)
}
