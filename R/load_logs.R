load_logs <- function(logName, logDir, runs, snaps) {
  bind_rows(sapply(runs %>% as.character(), function(run) {
    bind_rows(sapply(snaps %>% as.character(), function(snap) {
      logFile <- file.path(logDir, run, paste(logName, snap, "rds", sep = "."))
      if (file.exists(logFile)) {
        readRDS(logFile)
      }
    }, simplify = F, USE.NAMES = T), .id = "snapshot")
  }, simplify = F, USE.NAMES = T), .id = "run")
}
