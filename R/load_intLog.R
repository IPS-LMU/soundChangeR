#' Load interactions log as data.table
#'
#' @param logDir logging directory
#' @param runs number of independent parallel runs, i.e. params$multipleABMRuns or 1 in case of single run
#' @param snaps number of snapshots, i.e. params$nrOfSnapshots
#'
#' @export
#'
#' @examples load_intLog("ABM20211022152431", 1, 100)
load_intLog <- function(logDir, runs, snaps) {

  load_logs(logName = "intLog", logDir, runs, snaps)
}
