#' Purge single simulation from logging directory and simulation register
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param simName name of the simulation
#'
#' @export
purge_simulation <- function(rootLogDir, simName) {

  delete_simulation(rootLogDir, simName)
  base::system(base::paste("rm -rf", base::file.path(rootLogDir, simName)))
}
