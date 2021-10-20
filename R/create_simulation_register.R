create_simulation_register <- function(rootLogDir, force = FALSE) {
	
  if (!base::file.exists(base::file.path(rootLogDir, "simulations_register.rds")) | force) {
    rlist::list.save(base::list(), base::file.path(rootLogDir, "simulations_register.rds"))
  }
}
