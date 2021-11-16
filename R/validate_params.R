#' Check parameters for validity. Is automatically run during simulation.
#'
#' @param params list of parameters
#'
#' @return list of checked parameters and boolean on whether or not to run the simulation
#' @export
validate_params <- function(params) {

  runSimulation <- TRUE
  
  if (base::is.null(params[["createPopulationMethod"]])) {
    params[["createBootstrappedPopulation"]] <- FALSE
  }
  if (params[["createBootstrappedPopulation"]] && base::sum(params[["bootstrapPopulationSize"]] <= 0)) {
    write_log("Please set argument bootstrapPopulationSize correctly (number or named vector of numbers higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (base::is.null(params[["expandMemory"]])) {
    params[["expandMemory"]] <- FALSE
  }
  if (params[["expandMemory"]] && (!base::is.numeric(params[["expandMemoryFactor"]]) || params[["expandMemoryFactor"]] <= 0)) {
    write_log("Please set argument expandMemoryFactor correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (base::is.null(params[["removeOriginalExemplars"]])) {
    params[["removeOriginalExemplars"]] <- FALSE
  }

  if (base::is.null(params[["useSMOTE"]])) {
    params[["useSMOTE"]] <- TRUE
  }
  
  if (base::is.null(params[["fallBackOnPhoneme"]])) {
    params[["fallBackOnPhoneme"]] <- TRUE
  }
  
  if (!is.numeric(params[["minTokens"]]) || params[["minTokens"]] <= 0) {
    write_log("Please set argument minTokens correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (!is.numeric(params[["SMOTENN"]]) || params[["SMOTENN"]] <= 0) {
    write_log("Please set argument SMOTENN correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (base::is.null(params[["useFlexiblePhonology"]])) {
    params[["useFlexiblePhonology"]] <- FALSE
  }
  
  if (base::is.null(params[["memoryIntakeStrategy"]])) {
    write_log("Please set argument memoryIntakeStrategy correctly (string or vector with one/several of 
              'maxPosteriorProb', 'mahalanobisDistance', 'posteriorProbThr', 'acceptAll').", params)
    runSimulation <- FALSE
  }
  if (!base::any(base::c("maxPosteriorProb", "posteriorProbThr", "mahalanobisDistance", "acceptAll") %in% params[["memoryIntakeStrategy"]])) {
    write_log("Please set argument memoryIntakeStrategy correctly (string or vector with one/several of 
              'maxPosteriorProb', 'mahalanobisDistance', 'posteriorProbThr', 'acceptAll').", params)
    runSimulation <- FALSE
  }
  if (base::any(base::c("maxPosteriorProb", "posteriorProbThr") %in% params[["memoryIntakeStrategy"]])) {
    if (!params[["useFlexiblePhonology"]]) {
      params[["posteriorProbMethod"]] <- "qda"
    } else {
      params[["posteriorProbMethod"]] <- "GMM"
    }
  }
  
  if ("mahalanobisDistance" %in% params[["memoryIntakeStrategy"]] && 
      (params[["mahalanobisProbThreshold"]] <= 0 || params[["mahalanobisProbThreshold"]] > 1)) {
    write_log("Please set argument mahalanobisProbThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }

  if ("posteriorProbThr" %in% params[["memoryIntakeStrategy"]] && 
      (params[["posteriorProbThreshold"]] <= 0 || params[["posteriorProbThreshold"]] > 1)) {
    write_log("Please set argument posteriorProbThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  
  if (params[["perceptionOOVNN"]] <= 0) {
    params[["perceptionOOVNN"]] <- 5
  }
  if (params[["perceptionOOVNN"]] %% 2 == 0) {
    params[["perceptionOOVNN"]] <- params[["perceptionOOVNN"]] + 1
  }
  
  if (params[["forgettingRate"]] <= 0 || params[["forgettingRate"]] > 1) {
    write_log("Please set argument forgettingRate correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  
  if (params[["computeGMMsInterval"]] <= 0) {
    write_log("Please set argument computeGMMsInterval correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (params[["useFlexiblePhonology"]] && params[["purityRepetitions"]] <= 0) {
    write_log("Please set argument purityRepetitions correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (params[["useFlexiblePhonology"]] && (params[["purityThreshold"]] <= 0 || params[["purityThreshold"]] > 1)) {
    write_log("Please set argument purityThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  
  if (base::is.null(params[["interactionPartners"]])) {
    write_log("Please set argument interactionPartners correctly (either 'betweenGroups' or 'withinGroups' or 'random').", params)
    runSimulation <- FALSE
  } else if (grepl("between[groups]?", params[["interactionPartners"]], ignore.case = T)) {
    params[["interactionPartners"]] <- "betweenGroups"
  } else if (grepl("within[groups]?", params[["interactionPartners"]], ignore.case = T)) {
    params[["interactionPartners"]] <- "withinGroups"
  } else {
    params[["interactionPartners"]] <- "random"
  }
  
  if (base::is.null(params[["runSingleSimulation"]])) {
    params[["runSingleSimulation"]] <- TRUE
  }
  
  if (!params[["runSingleSimulation"]] && params[["multipleABMRuns"]] <= 0) {
    write_log("Please set argument multipleABMRuns correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (params[["nrOfSnapshots"]] <= 0) {
    write_log("Please set argument nrOfSnapshots correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (params[["interactionsPerSnapshot"]] < 0) {
    write_log("Please set argument interactionsPerSnapshot correctly (number equal to or higher than zero).", params)
    runSimulation <- FALSE
  }
  
  params[["featureExtractionMethod"]] <- "identity"
  params[["nrOfInteractions"]] <- params[["nrOfSnapshots"]] * params[["interactionsPerSnapshot"]]
  params[["packageVersion"]] <- base::as.character(utils::packageVersion("soundChangeR"))

  return(base::list(params = params, runSimulation = runSimulation))
}
