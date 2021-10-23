get_logDir <- function(params) {
  
  logDir <- if(base::is.null(params[["logDir"]])) {
    base::file.path(params[["rootLogDir"]], params[["simulationName"]])
  } else {
    params[["logDir"]]
  }
  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  return(logDir)
}
