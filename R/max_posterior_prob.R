max_posterior_prob <- function(exemplar, features, label, agent, params) {
  
  posteriorProb <- compute_posterior_probabilities(agent, features, params[["posteriorProbMethod"]])
  recognize_posterior_probabilities(posteriorProb, label, "maxPosteriorProb")
}
