get_params <- function(rootLogDir, simulationName) {

  rlist::list.load(base::file.path(rootLogDir, simulationName, "params.yaml"))
}
