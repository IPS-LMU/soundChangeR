#' Purge uncompleted simulations from logging directory and simulations register
#'
#' @param rootLogDir root logging directory, as set by argument rootLogDir in run_simulation()
#'
#' @export
purge_uncompleted_simulations <- function(rootLogDir) {

  base::lapply(filter_simulations(rootLogDir, completed == FALSE),
               function(simName) {
                 purge_simulation(rootLogDir, simName)
                 }
               )
}
