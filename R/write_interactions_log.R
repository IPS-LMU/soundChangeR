write_interactions_log <- function(interactionsLog, producedToken, perceiver, perceiverPhoneme_, memorise, strategy, features, nrSim) {

  rowToWrite <- base::which(interactionsLog$valid == FALSE)[1]
  interactionsLog[rowToWrite, `:=`(
    word = producedToken$word,
    producerID = producedToken$producerID,
    producerPhoneme = producedToken$phoneme,
    producerNrOfTimesHeard = producedToken$nrOfTimesHeard,
    perceiverID = perceiver$agentID,
    perceiverPhoneme = perceiverPhoneme_,
    perceiverNrOfTimesHeard = {
      if (memorise) {
        perceiver$memory$nrOfTimesHeard[perceiver$memory$word == producedToken$word & perceiver$memory$valid == TRUE][1]
      } else {
        base::as.integer(base::max(1, perceiver$memory$nrOfTimesHeard[perceiver$memory$word == producedToken$word & perceiver$memory$valid == TRUE][1]))
      }
    },
    accepted = memorise,
    rejectionCriterion = base::ifelse(memorise, NA_character_, strategy),
    valid = TRUE
  )] %>% 
    .[rowToWrite, base::paste0("P", 1:base::ncol(features)) := base::as.list(features)]
}
