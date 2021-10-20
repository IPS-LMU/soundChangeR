features2exemplar  <- function(features, agent, params) {

  methodReg <- get_method_register()
  methodReg[params[["featureExtractionMethod"]], features2exemplar][[1]](features, agent, params)
}
