set_completed <- function(rootLogDir, simName) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  i <- reg %>% rlist::list.findi(simulationName == simName)
  reg[[i]]["completed"] <- TRUE
  rlist::list.save(reg, regFile)
}
