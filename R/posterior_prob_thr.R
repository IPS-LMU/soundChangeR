posterior_prob_thr <- function(exemplar, features, label, agent, params) {
  posteriorProb <- compute_posterior_probabilities(agent, features, params[["posteriorProbMethod"]])
  recognize_posterior_probabilities(posteriorProb, label, "posteriorProbThr", posteriorProbThr = params[["posteriorProbThr"]])
}
