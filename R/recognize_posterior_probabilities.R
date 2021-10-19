recognize_posterior_probabilities <- function(posteriorProb, label, method, ...) {
  
  if (method == "maxPosteriorProb") {
    base::colnames(posteriorProb)[base::which.max(posteriorProb)] == label
  } else if (method == "posteriorProbThr") {
    posteriorProb[, label] >= base::list(...)[["posteriorProbThr"]]
  }
}
