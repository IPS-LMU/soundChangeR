set_completed <- function(simulationName_, rootLogDir) {

  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  i <- reg %>% list.findi(simulationName == simulationName_)
  reg[[i]]["completed"] <- TRUE
  list.save(reg, regFile)
}
