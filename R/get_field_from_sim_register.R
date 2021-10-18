get_field_from_sim_register <- function(rootLogDir, ...) {

  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  list.map(reg, ...)
}
