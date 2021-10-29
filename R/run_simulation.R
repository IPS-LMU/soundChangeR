#' Start the simulation
#'
#' @param paramsFile path to the params.yaml file
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom data.table %like%
#' @import mclust
run_simulation <- function(paramsFile) {
  
  params <- rlist::list.load(paramsFile)
  
  base::dir.create(params[["rootLogDir"]], showWarnings = FALSE, recursive = TRUE)
  create_simulation_register(params[["rootLogDir"]])
  
  params[["simulationName"]] <- generate_simulation_name()
  logDir <- base::file.path(params[["rootLogDir"]], params[["simulationName"]])
  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  
  input.df <- load_input_data(params)
  base::saveRDS(input.df, base::file.path(logDir, "input.rds"))
  
  check <- validate_params(params, input.df)
  params <- check[["params"]]
  
  register_simulation(params)
  
  if (check[["runSimulation"]]) {
    if (params[["runMode"]] == "single") {
      params[["logDir"]] <- base::file.path(logDir, "1")
      pop <- create_population(input.df = input.df, params = params)
      save_population(pop, extraCols = base::list(snapshot = 0), logDir = params[["logDir"]])
      if (params[["nrOfInteractions"]] > 0) {
        perform_interactions(pop, params[["logDir"]], params)
      }
    } else if (params[["runMode"]] == "multiple") {
      numCores <- parallel::detectCores() - 1
      if (base::Sys.info()[["sysname"]] == "Windows") {
        cl <- parallel::makeCluster(numCores, type = "PSOCK")
        parallel::clusterExport(cl, base::c("input.df", "params", "logDir"))
        # parallel::clusterEvalQ(cl, {
        #   source(file.path("Rcmd", "loadLibraries.R"))
        # })
      } else {
        cl <- parallel::makeCluster(numCores, type = "FORK")
      }
      parallel::clusterSetRNGStream(cl)
      parallel::parLapply(cl, base::seq_len(params[["multipleABMRuns"]]), function(abmName) {
        params[["logDir"]] <- base::file.path(logDir, abmName)
        pop <- create_population(input.df = input.df, params = params)
        save_population(pop, extraCols = base::list(snapshot = 0), logDir = params[["logDir"]])
        if (params[["nrOfInteractions"]] > 0) {
          perform_interactions(pop, params[["logDir"]], params)
        }
      })
      parallel::stopCluster(cl)
    }
    set_completed(params[["simulationName"]], params[["rootLogDir"]])
  }
}
