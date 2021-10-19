get_field_from_sim_register <- function(rootLogDir, ...) {

  regFile <- base::file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- rlist::list.load(regFile)
  rlist::list.map(reg, ...)
}
