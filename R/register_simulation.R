register_simulation <- function(params) {
  # params schaut soweit gut aus; wozu sind dann die Folgenden Zeilen nötig?
  params[["phoneme_set1"]] <- base::as.character(params[["phoneme_set1"]])
  if(params[["dim_cuespace"]] == 2){
     params[["phoneme_set2"]] <- base::as.character(params[["phoneme_set2"]])
  }
  rlist::list.save(params, base::file.path(params[["rootLogDir"]], params[["simulationName"]], "params.yaml"))
  params[["completed"]] <- FALSE
  regFile <- base::file.path(params[["rootLogDir"]], "simulations_register.rds")
  rlist::list.load(regFile) %>%
    rlist::list.append(params) %>%
    rlist::list.save(regFile)
}
