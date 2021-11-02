register_simulation <- function(params) {

  params[["phoneme"]] <- base::as.character(params[["phoneme"]])
  rlist::list.save(params, base::file.path(params[["rootLogDir"]], params[["simulationName"]], "params.yaml"))
  params[["completed"]] <- FALSE
  regFile <- base::file.path(params[["rootLogDir"]], "simulations_register.rds")
  rlist::list.load(regFile) %>%
    rlist::list.append(params) %>%
    rlist::list.save(regFile)
}
