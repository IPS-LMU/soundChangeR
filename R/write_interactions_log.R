write_interactions_log <- function(interactionsLog, producedToken, perceiver, perceiverPhoneme_, strategy, features, nrSim, memorise, perceiverPhoneme_filtered_) {
  
  memorise <- memorise

  
  rowToWrite <- base::which(interactionsLog$valid == FALSE)[1]
  interactionsLog[rowToWrite, `:=`(
    word = producedToken$word,
    producerID = producedToken$producerID,
    producerPhoneme = producedToken$phoneme,
    producerNrOfTimesHeard = producedToken$nrOfTimesHeard,
    perceiverID = perceiver$agentID,
    perceiverPhoneme = perceiverPhoneme_,
    perceiverPhoneme_filtered = perceiverPhoneme_filtered_, 
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
  #  considered_instances = list(memorise_result[2]) #### this is something I added for checking
  )] %>% 
    .[rowToWrite, base::paste0("P", 1:base::ncol(features)) := base::as.list(features)]
}
