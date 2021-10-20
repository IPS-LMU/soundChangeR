get_field_from_sim_register <- function(rootLogDir, ...) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  rlist::list.map(reg, ...)
}
