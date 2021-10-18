memory_intake_strategy <- function(strategy, exemplar, features, label, agent, params) {
  methodReg[params[["featureExtractionMethod"]], memoryIntakeStrategy][[1]][[strategy]](exemplar, features, label, agent, params)
}
