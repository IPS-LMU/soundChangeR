#' Run simulations, using arguments of function as params
#'
#' @param inputDataFile 
#' @param features 
#' @param group 
#' @param label 
#' @param initial 
#' @param word 
#' @param speaker 
#' @param subsetSpeakers 
#' @param subsetLabels 
#' @param createPopulationMethod 
#' @param bootstrapPopulationSize 
#' @param initialMemoryResampling 
#' @param initialMemoryResamplingFactor 
#' @param removeOriginalExemplarsAfterResampling 
#' @param productionBasis 
#' @param productionResampling 
#' @param productionResamplingFallback 
#' @param productionMinTokens 
#' @param productionSMOTENN 
#' @param perceptionModels 
#' @param memoryIntakeStrategy 
#' @param mahalanobisProbThreshold 
#' @param posteriorProbThr 
#' @param perceptionOOVNN 
#' @param computeGMMsInterval 
#' @param purityRepetitions 
#' @param purityThreshold 
#' @param forgettingRate 
#' @param interactionPartners 
#' @param speakerProb 
#' @param listenerProb 
#' @param runMode 
#' @param multipleABMRuns 
#' @param nrOfSnapshots 
#' @param interactionsPerSnapshot 
#' @param rootLogDir 
#' @param notes 
#'
#' @export
run_simulation_args <- function(inputDataFile = NULL,
                                features = NULL,
                                group = NULL,
                                label = NULL,
                                initial = NULL,
                                word = NULL,
                                speaker = NULL,
                                subsetSpeakers = NULL,
                                subsetLabels = NULL,
                                createPopulationMethod = "speaker_is_agent",
                                bootstrapPopulationSize = 50,
                                initialMemoryResampling = FALSE,
                                initialMemoryResamplingFactor = 2.0,
                                removeOriginalExemplarsAfterResampling = FALSE,
                                productionBasis = "word",
                                productionResampling = "SMOTE",
                                productionResamplingFallback = "label",
                                productionMinTokens = 10,
                                productionSMOTENN = 5,
                                perceptionModels = "singleGaussian",
                                memoryIntakeStrategy = c("mahalanobisDistance", "maxPosteriorProb"),
                                mahalanobisProbThreshold = .95,
                                posteriorProbThr = 1/3,
                                perceptionOOVNN = 5,
                                computeGMMsInterval = 100,
                                purityRepetitions = 5,
                                purityThreshold = 0.75,
                                forgettingRate = 1,
                                interactionPartners = "betweenGroups",
                                speakerProb = NULL,
                                listenerProb = NULL,
                                runMode = "single",
                                multipleABMRuns = 3,
                                nrOfSnapshots = 1,
                                interactionsPerSnapshot = 10,
                                rootLogDir = "./logDir",
                                notes = "") {
  
  params <- base::as.list(base::environment())
  base::dir.create(params[["rootLogDir"]], showWarnings = FALSE, recursive = TRUE)
  create_simulation_register(params[["rootLogDir"]])

  params[["simulationName"]] <- generate_simulation_name()
  logDir <- base::file.path(params[["rootLogDir"]], params[["simulationName"]])
  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)

  if (base::is.null(params$inputDataFile) || !base::file.exists(params$inputDataFile)) {
    stop("Your input data file does not exist. 
         Please make sure you are entering a correct relative or 
         absolute path to an existing data file with extension .csv or .txt.")
  }
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