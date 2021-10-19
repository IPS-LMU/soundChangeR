purge_simulation <- function(simulationName_, rootLogDir) {

  delete_simulation(simulationName_, rootLogDir)
  base::system(base::paste("rm -rf", base::file.path(rootLogDir, simulationName_)))
}
