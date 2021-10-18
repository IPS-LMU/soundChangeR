create_simulation_register <- function(rootLogDir, force = FALSE) {
  if (!file.exists(file.path(rootLogDir, SIM_REG_FILENAME)) | force) {
    list.save(list(), file.path(rootLogDir, SIM_REG_FILENAME))
  }
}
