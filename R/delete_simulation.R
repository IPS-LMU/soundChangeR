#' Delete simulation only from simulation register
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param simName name of the simulation
#'
#' @export
delete_simulation <- function(rootLogDir, simName) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  rlist::list.load(regFile) %>%
    rlist::list.exclude(simulationName == simName) %>%
    rlist::list.save(regFile)
}
