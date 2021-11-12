#' Load interactions log as data.table
#'
#' @param logDir path to simulation
#' @param runs run(s) to be loaded, i.e. number or vector of numbers
#' @param snaps snapshot(s) to be loaded, i.e. number or vector of numbers
#'
#' @export
load_intLog <- function(logDir, runs, snaps) {

  load_logs(logName = "intLog", logDir, runs, snaps) %>% 
    dplyr::filter(valid == T) %>% 
    dplyr::select(-valid)
}
