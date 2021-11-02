memory_intake_strategy <- function(strategy, exemplar, features, phoneme, agent, params) {
  
  methodReg <- get_method_register()
  methodReg[params[["featureExtractionMethod"]], memoryIntakeStrategy][[1]][[strategy]](exemplar, features, phoneme, agent, params)
}
