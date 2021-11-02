max_posterior_prob <- function(exemplar, features, phoneme, agent, params) {
  
  posteriorProb <- compute_posterior_probabilities(agent, features, params[["posteriorProbMethod"]])
  recognize_posterior_probabilities(posteriorProb, phoneme, "maxPosteriorProb")
}
