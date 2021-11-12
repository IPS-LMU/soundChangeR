#' Load population as data table
#'
#' @param logDir path to simulation
#' @param runs run(s) to be loaded, i.e. number or vector of numbers
#' @param snaps snapshot(s) to be loaded, i.e. number or vector of numbers
#'
#' @export
load_pop <- function(logDir, runs, snaps) {

  load_logs(logName = "pop", logDir, runs, snaps) %>% 
    dplyr::filter(valid == T) %>% 
    dplyr::select(-valid)
}
