#' Purge single simulation from logging directory and simulation register
#'
#' @param simulationName name of the simulation
#' @param rootLogDir logging directory
#'
#' @export
purge_simulation <- function(simulationName, rootLogDir) {

  delete_simulation(simulationName, rootLogDir)
  base::system(base::paste("rm -rf", base::file.path(rootLogDir, simulationName)))
}
