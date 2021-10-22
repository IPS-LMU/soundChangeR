#' Load parameter list saved in a simulation directory
#'
#' @param rootLogDir logging directory
#' @param simulationName name of the simulation
#'
#' @export
#'
#' @examples get_params("./logDir", "ABM20211022152431")
get_params <- function(rootLogDir, simulationName) {

  rlist::list.load(base::file.path(rootLogDir, simulationName, "params.yaml"))
}
