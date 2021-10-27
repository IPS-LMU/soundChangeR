#' Load cache as data.table
#'
#' @param logDir logging directory
#' @param runs number of independent parallel runs, i.e. params$multipleABMRuns or 1 in case of single run
#' @param snaps number of snapshots, i.e. params$nrOfSnapshots
#'
#' @export
load_cache <- function(logDir, runs, snaps) {

  load_logs(logName = "cache", logDir, runs, snaps)
}
