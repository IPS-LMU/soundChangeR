register_simulation <- function(params) {

  params[["initial"]] <- as.character(params[["initial"]])
  params[["label"]] <- as.character(params[["label"]])
  list.save(params, file.path(params[["rootLogDir"]], params[["simulationName"]], PARAMS_FILENAME))
  params[["completed"]] <- FALSE
  regFile <- file.path(params[["rootLogDir"]], SIM_REG_FILENAME)
  list.load(regFile) %>%
    list.append(params) %>%
    list.save(regFile)
}
