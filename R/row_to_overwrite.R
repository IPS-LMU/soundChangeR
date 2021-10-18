row_to_overwrite <- function(perceiver, producedToken, params) {

  if (params[["memoryRemovalStrategy"]] == "timeDecay") {
    rowToOverwrite <- which(perceiver$memory$word == producedToken$word)[
      which.min(perceiver$memory$timeStamp[perceiver$memory$word == producedToken$word])
    ]
  } else if (params[["memoryRemovalStrategy"]] == "outlierRemoval") {
    tdat.mahal <- train(as.matrix(perceiver$features)[perceiver$memory$label == perceiverLabel, , drop = FALSE])
    rowToOverwrite <- which(perceiver$memory$word == producedToken$word)[
      which.max(emuR::distance(as.matrix(perceiver$features)[perceiver$memory$word == producedToken$word, , drop = FALSE], tdat.mahal, metric = "mahal"))
    ]
  } else if (params[["memoryRemovalStrategy"]] == "random") {
    rowToOverwrite <- sample(which(perceiver$memory$word == producedToken$word), 1)
  }
  return(rowToOverwrite)
}
