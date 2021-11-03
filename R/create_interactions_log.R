create_interactions_log <- function(nrOfInteractions) {

  interactionsLog <- data.table::data.table(word = NA_character_, 
                                            producerID = NA_integer_, 
                                            producerPhoneme = NA_character_,
                                            producerNrOfTimesHeard = NA_integer_, 
                                            perceiverID = NA_integer_,
                                            perceiverPhoneme = NA_character_, 
                                            perceiverNrOfTimesHeard = NA_integer_,
                                            accepted = NA, 
                                            rejectionCriterion = NA_character_, 
                                            simulationNr = NA_integer_, 
                                            valid = NA)[0]

  data.table::rbindlist(base::list(
    interactionsLog, data.table::data.table(base::matrix(nrow = nrOfInteractions, ncol = ncol(interactionsLog)))
  ), use.names = FALSE) %>%
    .[, valid := FALSE] %>%
    .[]
}
