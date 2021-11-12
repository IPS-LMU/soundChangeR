write_memory <- function(agent, params, producedToken, rowToWrite, phoneme_) {

  updatedNrOfTimesHeard <- 1 + base::max(0, agent$memory$nrOfTimesHeard[
    agent$memory$word == producedToken$word & agent$memory$valid == TRUE
  ][1], na.rm = TRUE)

  agent$memory[rowToWrite, `:=`(
    word = producedToken$word,
    exemplar = producedToken$exemplar,
    phoneme = phoneme_,
    valid = TRUE,
    producerID = producedToken$producerID
  )]
  agent$memory[agent$memory$word == producedToken$word & agent$memory$valid == TRUE,
               nrOfTimesHeard := updatedNrOfTimesHeard]
  write_features(agent, exemplar2features(producedToken$exemplar, agent, params), rowToWrite)
}
