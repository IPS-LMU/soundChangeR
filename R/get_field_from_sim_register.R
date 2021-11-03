#' Get values for field(s) in simulation register for all registered simulations
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param ... vector of arguments of run_simulation()
#'
#' @return list with as many entries as there are simulations in the register where values are the values of the fields entered as argument to this function
#' @export
get_field_from_sim_register <- function(rootLogDir, ...) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  rlist::list.map(reg, ...)
}
