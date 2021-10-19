row_to_overwrite <- function(perceiver, producedToken, params) {

  if (params[["memoryRemovalStrategy"]] == "timeDecay") {
    rowToOverwrite <- base::which(perceiver$memory$word == producedToken$word)[
      base::which.min(perceiver$memory$timeStamp[perceiver$memory$word == producedToken$word])
    ]
  } else if (params[["memoryRemovalStrategy"]] == "outlierRemoval") {
    tdat.mahal <- emuR::train(base::as.matrix(perceiver$features)[perceiver$memory$label == perceiverLabel, , drop = FALSE])
    rowToOverwrite <- base::which(perceiver$memory$word == producedToken$word)[
      base::which.max(emuR::distance(base::as.matrix(perceiver$features)[perceiver$memory$word == producedToken$word, , drop = FALSE], tdat.mahal, metric = "mahal"))
    ]
  } else if (params[["memoryRemovalStrategy"]] == "random") {
    rowToOverwrite <- base::sample(base::which(perceiver$memory$word == producedToken$word), 1)
  }
  return(rowToOverwrite)
}
