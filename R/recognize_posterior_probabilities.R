recognize_posterior_probabilities <- function(posteriorProb, phoneme, method, params) {
  
  if (method == "maxPosteriorProb") {
    base::colnames(posteriorProb)[base::which.max(posteriorProb)] == phoneme
  } else if (method == "posteriorProbThr") {
    posteriorProb[, phoneme] >= params[["posteriorProbThreshold"]]
  }
}
