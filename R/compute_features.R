compute_features <- function(agent, params) {
  methodReg[params[["featureExtractionMethod"]], compute_features][[1]](agent$memory[valid == TRUE, exemplar], agent, params)
}
