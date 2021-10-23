dump_obj <- function(obj, name, params) {
  
  logDir <- get_logDir(params)
  base::saveRDS(obj, base::file.path(logDir, base::paste(name, "rds", sep = ".")))
}
