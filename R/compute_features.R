compute_features <- function(agent, params) {

  methodReg <- get_method_register()
  methodReg[params[["featureExtractionMethod"]], compute_features][[1]](agent$memory[valid == TRUE, exemplar], agent, params)
}
