compute_features <- function(agent, params) {

  methodReg <- get_method_register()

 # gibt die Exemplare als Matrix zurück
  exemplar_matrix_set1 = methodReg[params[["featureExtractionMethod"]], compute_features][[1]](agent$memory[valid == TRUE, exemplar_set1], agent, params)
  #stop("test5")
  if(params[["dim_cuespace"]] == 2){
    exemplar_matrix_set2 = methodReg[params[["featureExtractionMethod"]], compute_features][[1]](agent$memory[valid == TRUE, exemplar_set2], agent, params)
    return(list(exemplar_matrix_set1, exemplar_matrix_set2))
  }
  else{
    return(list(exemplar_matrix_set1, c()))
  }
  
}
