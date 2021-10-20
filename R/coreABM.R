coreABM <- function(input.df, params, logDir) {
  
  pop <- create_population(input.df = input.df, params = params)
  
  save_population(pop, extraCols = base::list(snapshot = 0), logDir = logDir)
  if (params[["nrOfInteractions"]] > 0) {
    perform_interactions(pop, logDir, params)
  }
}
