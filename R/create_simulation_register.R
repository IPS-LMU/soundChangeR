create_simulation_register <- function(rootLogDir, force = FALSE) {
  if (!base::file.exists(base::file.path(rootLogDir, SIM_REG_FILENAME)) | force) {
    rlist::list.save(base::list(), base::file.path(rootLogDir, SIM_REG_FILENAME))
  }
}
