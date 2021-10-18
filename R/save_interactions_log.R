save_interactions_log <- function(interactionsLog, extraCols = list(condition = "x"), logDir) {

  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(interactionsLog,
          file = file.path(logDir, paste("intLog", unlist(extraCols), "rds", sep = "."))
  )
}
