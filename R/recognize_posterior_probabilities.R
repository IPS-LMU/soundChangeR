recognize_posterior_probabilities <- function(posteriorProb, label, method, ...) {
  if (method == "maxPosteriorProb") {
    colnames(posteriorProb)[which.max(posteriorProb)] == label
  } else if (method == "posteriorProbThr") {
    posteriorProb[, label] >= list(...)[["posteriorProbThr"]]
  }
}
