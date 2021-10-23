write_log <- function(msg, params) {
  
  logDir <- get_logDir(params)
  base::write(msg, base::file.path(logDir, "log.txt"), append = TRUE)
}
