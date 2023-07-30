write_memory <- function(agent, params, producedToken, rowToWrite, phoneme_) {

  updatedNrOfTimesHeard <- 1 + base::max(0, agent$memory$nrOfTimesHeard[
    agent$memory$word == producedToken$word & agent$memory$valid == TRUE
  ][1], na.rm = TRUE)

  agent$memory[rowToWrite, `:=`(
    word = producedToken$word,
    exemplar = producedToken$exemplar,
    phoneme_set1 = phoneme_[1],
    phoneme_set2 = phoneme_[2],
    valid = TRUE,
    producerID = producedToken$producerID
  )]
  agent$memory[agent$memory$word == producedToken$word & agent$memory$valid == TRUE,
               nrOfTimesHeard := updatedNrOfTimesHeard]
               #exemplar2features gibt eine Matrix zurück: die linken Spaöten gehören zu Cuespace 1 die rechten zu Cuespace 2
  write_features(agent, exemplar2features(producedToken$exemplar, agent, params), rowToWrite)
}
