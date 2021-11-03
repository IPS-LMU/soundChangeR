#' Find simulations based on their parameters
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#' @param ... any number of arguments, where the argument name must be from the list of model parameters, connected to a value with a logical operator, e.g. parameter == value
#' @param condList list of parameter names and values, e.g. parameter = value; default is NULL.
#'
#' @return names of simulations that fit the given parameters
#' @export
filter_simulations <- function(rootLogDir, ..., condList = NULL) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  if (!base::is.null(condList)) {
    matching <- base::sapply(reg, function(r) {
      base::sapply(base::names(condList), function(k) r[[k]] == condList[[k]]) %>% base::all()
    })
  } else {
    matching <- rlist::list.is(reg, ...)
  }
  reg[matching] %>% rlist::list.select(simulationName) %>% base::unlist()
}
