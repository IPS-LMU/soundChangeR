memory_intake_strategy <- function(strategy, exemplar, features, phoneme, agent, params) {
  
  methodReg <- get_method_register()
  methodReg[params[["featureExtractionMethod"]], memoryIntakeStrategy][[1]][[strategy]](exemplar, features, phoneme, agent, params)
  # führt je nachdem welchen Wert strategy hat eine dieser Funktionen aus: accept_all(), mahalanobis_distance(), max_posterior_prob(),posterior_prob_thr
  # die Argumente sind exempplar, features, phoneme, agent, params
}
