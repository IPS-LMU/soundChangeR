get_params <- function(rootLogDir, simulationName) {
  # This function loads the list of parameters in params.yaml
  # for a specific simulation.
  # No function call (utility function).
  #
  # Args:
  #    - rootLogDir: the path to the root logging directory
  #    - simulationName: name of the simulation as a string
  #
  # Returns:
  #    - nothing.
  #

  list.load(file.path(rootLogDir, simulationName, PARAMS_FILENAME))
}
