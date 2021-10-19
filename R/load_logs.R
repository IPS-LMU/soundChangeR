load_logs <- function(logName, logDir, runs, snaps) {
  
  dplyr::bind_rows(base::sapply(runs %>% base::as.character(), function(run) {
    dplyr::bind_rows(base::sapply(snaps %>% base::as.character(), function(snap) {
      logFile <- base::file.path(logDir, run, base::paste(logName, snap, "rds", sep = "."))
      if (base::file.exists(logFile)) {
        base::readRDS(logFile)
      }
    }, simplify = F, USE.NAMES = T), .id = "snapshot")
  }, simplify = F, USE.NAMES = T), .id = "run")
}
