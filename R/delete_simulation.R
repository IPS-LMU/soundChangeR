delete_simulation <- function(simulationName_, rootLogDir) {

  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  list.load(regFile) %>%
    list.exclude(simulationName == simulationName_) %>%
    list.save(regFile)
}
