write_log <- function(msg, agent, params) {
  
  logDir <- get_logDir(agent, params)
  base::write(msg, base::file.path(logDir, "log.txt"), append = TRUE)
}