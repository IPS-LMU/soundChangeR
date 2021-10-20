register_simulation <- function(params) {

  params[["initial"]] <- base::as.character(params[["initial"]])
  params[["label"]] <- base::as.character(params[["label"]])
  rlist::list.save(params, base::file.path(params[["rootLogDir"]], params[["simulationName"]], "params.yaml"))
  params[["completed"]] <- FALSE
  regFile <- base::file.path(params[["rootLogDir"]], "simulations_register.rds")
  rlist::list.load(regFile) %>%
    rlist::list.append(params) %>%
    rlist::list.save(regFile)
}
