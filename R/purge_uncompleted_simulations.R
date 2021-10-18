purge_uncompleted_simulations <- function(rootLogDir) {

  lapply(filter_simulations(rootLogDir, completed == FALSE),
         function(simName) {
           purge_simulation(simName, rootLogDir)
         })
}
