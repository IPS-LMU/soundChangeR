#' Purge single simulation from logging directory and simulation register
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param simulationName name of the simulation
#'
#' @export
purge_simulation <- function(rootLogDir, simulationName) {

  delete_simulation(rootLogDir, simulationName)
  base::system(base::paste("rm -rf", base::file.path(rootLogDir, simulationName)))
}
