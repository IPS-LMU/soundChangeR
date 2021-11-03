#' Title
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param ... field names
#'
#' @return
#' @export
get_field_from_sim_register <- function(rootLogDir, ...) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  rlist::list.map(reg, ...)
}
