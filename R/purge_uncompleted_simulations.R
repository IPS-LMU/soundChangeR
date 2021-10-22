#' Purge uncompleted simulations from logging directory and simulations register
#'
#' @param rootLogDir logging directory, i.e. where all the "ABM..." folders are stored
#'
#' @export
#'
#' @examples purge_uncompleted_simulations("./logDir")
purge_uncompleted_simulations <- function(rootLogDir) {

  base::lapply(filter_simulations(rootLogDir, completed == FALSE),
               function(simName) {
                 purge_simulation(simName, rootLogDir)
                 }
               )
}
