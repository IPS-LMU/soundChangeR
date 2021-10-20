delete_simulation <- function(simulationName_, rootLogDir) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  rlist::list.load(regFile) %>%
    rlist::list.exclude(simulationName == simulationName_) %>%
    rlist::list.save(regFile)
}
