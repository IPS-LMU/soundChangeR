#' Load population as data table
#'
#' @param logDir logging directory
#' @param runs number of independent parallel runs, i.e. params$multipleABMRuns or 1 in case of single run
#' @param snaps number of snapshots, i.e. params$nrOfSnapshots
#'
#' @export
#'
#' @examples load_pop("ABM20211022152431", 1, 100)
load_pop <- function(logDir, runs, snaps) {

  load_logs(logName = "pop", logDir, runs, snaps)
}
