set_completed <- function(simulationName_, rootLogDir) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  i <- reg %>% rlist::list.findi(simulationName == simulationName_)
  reg[[i]]["completed"] <- TRUE
  rlist::list.save(reg, regFile)
}
