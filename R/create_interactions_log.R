create_interactions_log <- function(params) {

  interactionsLog <- data.table::data.table(word = NA_character_, 
                                            producerID = NA_integer_, 
                                            producerPhoneme_set1 = NA_character_,
                                            producerPhoneme_set2 = NA_character_,
                                            producerNrOfTimesHeard = NA_integer_, 
                                            perceiverID = NA_integer_,
                                            perceiverPhoneme_set1 = NA_character_, 
                                            perceiverPhoneme_set2 = NA_character_, 
                                            perceiverNrOfTimesHeard = NA_integer_,
                                            accepted = NA, 
                                            rejectionCriterion = NA_character_,
                                            valid = NA)[0]

  # unten müssen auch die features geändert werden
  data.table::rbindlist(base::list(
    interactionsLog, data.table::data.table(base::matrix(nrow = params[["interactionsPerSnapshot"]], ncol = ncol(interactionsLog)))
  ), use.names = FALSE) %>%
    .[, valid := FALSE] %>%
    .[, base::c(paste0("P1_", 1:base::length(c(params[["feature_set1"]]))), paste0("P2_", 1:length(params[["feature_set2"]]))) := NA_real_] %>% 
    .[]
}
