purge_simulation <- function(simulationName_, rootLogDir) {

  delete_simulation(simulationName_, rootLogDir)
  system(paste("rm -rf", file.path(rootLogDir, simulationName_)))
}
