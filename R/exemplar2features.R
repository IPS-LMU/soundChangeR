exemplar2features <- function(exemplar, agent, params) {

  methodReg <- get_method_register()
  methodReg[params[["featureExtractionMethod"]], exemplar2features][[1]](exemplar, agent, params)
}
