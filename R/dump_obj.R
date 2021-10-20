dump_obj <- function(obj, name, agent, params) {
  
  logDir <- get_logDir(agent, params)
  base::saveRDS(obj, base::file.path(logDir, base::paste(name, "rds", sep = ".")))
}
