filter_simulations <- function(rootLogDir, ..., condList = NULL) {

  regFile <- base::file.path(rootLogDir, "simulations_register.rds")
  reg <- rlist::list.load(regFile)
  if (!base::is.null(condList)) {
    matching <- base::sapply(reg, function(r) {
      base::sapply(base::names(condList), function(k) r[[k]] == condList[[k]]) %>% base::all()
    })
  } else {
    matching <- rlist::list.is(reg, ...)
  }
  reg[matching] %>% rlist::list.select(simulationName) %>% base::unlist()
}
