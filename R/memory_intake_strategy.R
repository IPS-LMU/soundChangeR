memory_intake_strategy <- function(strategy, exemplar, features, label, agent, params) {
  
  methodReg <- get_method_register()
  methodReg[params[["featureExtractionMethod"]], memoryIntakeStrategy][[1]][[strategy]](exemplar, features, label, agent, params)
}
