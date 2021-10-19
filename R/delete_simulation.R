delete_simulation <- function(simulationName_, rootLogDir) {

  regFile <- base::file.path(rootLogDir, SIM_REG_FILENAME)
  rlist::list.load(regFile) %>%
    rlist::list.exclude(simulationName == simulationName_) %>%
    rlist::list.save(regFile)
}
