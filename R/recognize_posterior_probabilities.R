recognize_posterior_probabilities <- function(posteriorProb, phoneme, method, ...) {
  
  if (method == "maxPosteriorProb") {
    base::colnames(posteriorProb)[base::which.max(posteriorProb)] == phoneme
  } else if (method == "posteriorProbThr") {
    posteriorProb[, phoneme] >= base::list(...)[["posteriorProbThreshold"]]
  }
}
