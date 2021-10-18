exemplar2features <- function(exemplar, agent, params) {
  methodReg[params[["featureExtractionMethod"]], exemplar2features][[1]](exemplar, agent, params)
}
