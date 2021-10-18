features2exemplar  <- function(features, agent, params) {
  methodReg[params[["featureExtractionMethod"]], features2exemplar][[1]](features, agent, params)
}
