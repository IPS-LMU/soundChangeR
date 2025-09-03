#' Load sub-population as data table
#'
#' @param logDir path to simulation
#' @param runs run(s) to be loaded, i.e. number or vector of numbers
#' @param snaps snapshot(s) to be loaded, i.e. number or vector of numbers
#'
#' @export
load_subpop <- function(logDir, runs, snaps) {
  
  load_logs(logName = "sub_pop", logDir, runs, snaps) %>% 
    dplyr::filter(valid == T) %>% 
    dplyr::select(-valid)
}
