save_population <- function(pop, extraCols = list(condition = "x"), logDir) {

  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(convert_pop_list_to_dt(pop, extraCols),
          file = file.path(logDir, paste("pop", unlist(extraCols), "rds", sep = "."))
  )
  saveRDS(rbindlist(lapply(pop, function(agent) {agent$cache}), use.names = TRUE, idcol = "Agent"),
          file = file.path(logDir, paste("cache", unlist(extraCols), "rds", sep = "."))
  )
}
