write_interactions_log <- function(interactionsLog, producedToken, perceiver, perceiverLabel_, memorise, strategy,  nrSim) {

  rowToWrite <- base::which(interactionsLog$valid == FALSE)[1]
  interactionsLog[rowToWrite, `:=`(
    word = producedToken$word,
    producerID = producedToken$producerID,
    producerLabel = producedToken$label,
    producerNrOfTimesHeard = producedToken$nrOfTimesHeard,
    perceiverID = perceiver$agentID,
    perceiverLabel = perceiverLabel_,
    perceiverNrOfTimesHeard = {
      if (memorise) {
        perceiver$memory$nrOfTimesHeard[perceiver$memory$word == producedToken$word & perceiver$memory$valid == TRUE][1]
      } else {
        base::as.integer(base::max(1, perceiver$memory$nrOfTimesHeard[perceiver$memory$word == producedToken$word & perceiver$memory$valid == TRUE][1]))
      }
    },
    accepted = memorise,
    rejectionCriterion = base::ifelse(memorise, NA_character_, strategy),
    simulationNr = nrSim,
    valid = TRUE
  )]
}
