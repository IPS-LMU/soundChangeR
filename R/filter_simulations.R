filter_simulations <- function(rootLogDir, ..., condList = NULL) {

  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  if (!is.null(condList)) {
    matching <- sapply(reg, function(r) {
      sapply(names(condList), function(k) r[[k]] == condList[[k]]) %>% all()
    })
  } else {
    # matching <- list.is(reg, tryEval(..., FALSE))
    matching <- list.is(reg, ...)
  }
  reg[matching] %>% list.select(simulationName) %>% unlist
}
