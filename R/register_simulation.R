register_simulation <- function(params) {

  params[["initial"]] <- base::as.character(params[["initial"]])
  params[["label"]] <- base::as.character(params[["label"]])
  rlist::list.save(params, base::file.path(params[["rootLogDir"]], params[["simulationName"]], PARAMS_FILENAME))
  params[["completed"]] <- FALSE
  regFile <- base::file.path(params[["rootLogDir"]], SIM_REG_FILENAME)
  rlist::list.load(regFile) %>%
    rlist::list.append(params) %>%
    rlist::list.save(regFile)
}
