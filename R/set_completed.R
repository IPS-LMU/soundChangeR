set_completed <- function(simulationName_, rootLogDir) {

  regFile <- base::file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- rlist::list.load(regFile)
  i <- reg %>% rlist::list.findi(simulationName == simulationName_)
  reg[[i]]["completed"] <- TRUE
  rlist::list.save(reg, regFile)
}
