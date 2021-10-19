save_interactions_log <- function(interactionsLog, extraCols = list(condition = "x"), logDir) {

  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  base::saveRDS(interactionsLog,
                file = base::file.path(logDir, base::paste("intLog", base::unlist(extraCols), "rds", sep = "."))
  )
}
