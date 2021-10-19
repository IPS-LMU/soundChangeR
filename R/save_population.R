save_population <- function(pop, extraCols = list(condition = "x"), logDir) {

  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  base::saveRDS(convert_pop_list_to_dt(pop, extraCols),
                file = base::file.path(logDir, base::paste("pop", base::unlist(extraCols), "rds", sep = "."))
  )
  base::saveRDS(data.table::rbindlist(base::lapply(pop, function(agent) {agent$cache}), use.names = TRUE, idcol = "Agent"),
                file = base::file.path(logDir, base::paste("cache", base::unlist(extraCols), "rds", sep = "."))
  )
}
