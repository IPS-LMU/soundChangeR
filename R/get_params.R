#' Load parameter list saved in a simulation directory
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param simName name of the simulation
#'
#' @export
get_params <- function(rootLogDir, simName) {

  rlist::list.load(base::file.path(rootLogDir, simName, "params.yaml"))
}
