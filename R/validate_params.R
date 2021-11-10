#' Check parameters for validity. Is automatically run during simulation.
#'
#' @param params list of parameters
#' @param input.df input data frame
#'
#' @return list of checked parameters and boolean on whether or not to run the simulation
#' @export
validate_params <- function(params, input.df) {

  runSimulation <- TRUE
  
  if (!"createBootstrappedPopulation" %in% base::names(params) || base::is.null(params[["createPopulationMethod"]])) {
    params[["createBootstrappedPopulation"]] <- FALSE
  }
  if (params[["createBootstrappedPopulation"]] && 
      (base::sum(params[["bootstrapPopulationSize"]] <= 0) || !"bootstrapPopulationSize" %in% base::names(params))) {
    write_log("Please specify parameter bootstrapPopulationSize correctly (number or named vector of numbers higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (!"expandMemory" %in% base::names(params) || base::is.null(params[["expandMemory"]])) {
    params[["expandMemory"]] <- FALSE
  }
  
  if (params[["expandMemory"]]) {
    if (!"expandMemoryFactor" %in% base::names(params) ||
        !base::is.numeric(params[["expandMemoryFactor"]]) || 
        params[["expandMemoryFactor"]] <= 0) {
      write_log("Please specify parameter expandMemoryFactor correctly (number higher than zero).", params)
      runSimulation <- FALSE
    }
  }
  
  if (!"removeOriginalExemplars" %in% base::names(params) || 
      base::is.null(params[["removeOriginalExemplars"]])) {
    params[["removeOriginalExemplars"]] <- FALSE
  }

  methodReg <- get_method_register()
  if (!"featureExtractionMethod" %in% base::names(params) || base::is.null(params[["featureExtractionMethod"]])) {
    params[["featureExtractionMethod"]] <- "identity"
  }
  if (!params[["featureExtractionMethod"]] %in% methodReg$method) {
    write_log("Your chosen featureExtractionMethod is currently not implemented. Please use featureExtractionMethod: 'identity'.", params)
    runSimulation <- FALSE
  }
  
  if (!"useSMOTE" %in% base::names(params)) {
    params[["useSMOTE"]] <- TRUE
  }
  
  if (!"fallBackOnPhoneme" %in% base::names(params)) {
    params[["fallBackOnPhoneme"]] <- TRUE
  }
  if (!"minTokens" %in% base::names(params) || params[["minTokens"]] <= 0) {
    write_log("Please specify parameter minTokens correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (!"SMOTENN" %in% base::names(params) || params[["SMOTENN"]] <= 0) {
    write_log("Please specify parameter SMOTENN correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (!"useFlexiblePhonology" %in% base::names(params) || base::is.null(params[["useFlexiblePhonology"]])) {
    params[["useFlexiblePhonology"]] <- FALSE
  }
  
  if (!"memoryIntakeStrategy" %in% base::names(params) || base::is.null(params[["memoryIntakeStrategy"]])) {
    write_log("Please specify parameter memoryIntakeStrategy correctly (string or vector with one/several of 
              'maxPosteriorProb', 'mahalanobisDistance', 'posteriorProbThr', 'acceptAll').", params)
    runSimulation <- FALSE
  }
  if (!base::any(base::c("maxPosteriorProb", "posteriorProbThr", "mahalanobisDistance", "acceptAll") %in% params[["memoryIntakeStrategy"]])) {
    write_log("Please specify parameter memoryIntakeStrategy correctly (string or vector with one/several of 
              'maxPosteriorProb', 'mahalanobisDistance', 'posteriorProbThr', 'acceptAll').", params)
    runSimulation <- FALSE
  }
  if (base::any(base::c("maxPosteriorProb", "posteriorProbThr") %in% params[["memoryIntakeStrategy"]])) {
    if (!params[["useFlexiblePhonology"]]) {
      params[["posteriorProbMethod"]] <- "qda"
    } else if (params[["useFlexiblePhonology"]]) {
      params[["posteriorProbMethod"]] <- "GMM"
    }
  }
  
  if ("mahalanobisDistance" %in% params[["memoryIntakeStrategy"]] && !"mahalanobisProbThreshold" %in% base::names(params)) {
    write_log("Please specify parameter mahalanobisProbThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  if ("mahalanobisDistance" %in% params[["memoryIntakeStrategy"]] && 
      (params[["mahalanobisProbThreshold"]] <= 0 || params[["mahalanobisProbThreshold"]] > 1)) {
    write_log("Please specify parameter mahalanobisProbThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }

  if ("posteriorProbThr" %in% params[["memoryIntakeStrategy"]] && !"posteriorProbThreshold" %in% base::names(params)) {
    write_log("Please specify parameter posteriorProbThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  if ("posteriorProbThr" %in% params[["memoryIntakeStrategy"]] && 
      (params[["posteriorProbThreshold"]] <= 0 || params[["posteriorProbThreshold"]] > 1)) {
    write_log("Please specify parameter posteriorProbThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  
  if (!"perceptionOOVNN" %in% base::names(params) || params[["perceptionOOVNN"]] <= 0) {
    params[["perceptionOOVNN"]] <- 5
  }
  if (params[["perceptionOOVNN"]] %% 2 == 1) {
    params[["perceptionOOVNN"]] <- params[["perceptionOOVNN"]] + 1
  }
  
  if (!"computeGMMsInterval" %in% base::names(params) || params[["computeGMMsInterval"]] <= 0) {
    write_log("Please specify parameter computeGMMsInterval correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (params[["useFlexiblePhonology"]] && !"purityRepetitions" %in% base::names(params)) {
    params[["purityRepetitions"]] <- 5
  }
  if (params[["useFlexiblePhonology"]] && params[["purityRepetitions"]] <= 0) {
    write_log("Please specify parameter purityRepetitions correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (params[["useFlexiblePhonology"]] && !"purityThreshold" %in% base::names(params)) {
    params[["purityThreshold"]] <- 0.75
  }
  if (params[["useFlexiblePhonology"]] && (params[["purityThreshold"]] <= 0 || params[["purityThreshold"]] > 1)) {
    write_log("Please specify parameter purityThreshold correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  
  if (!"forgettingRate" %in% base::names(params)) {
    params[["forgettingRate"]] <- 1
  }
  if (params[["forgettingRate"]] <= 0 || params[["forgettingRate"]] > 1) {
    write_log("Please specify parameter forgettingRate correctly (number between zero and one).", params)
    runSimulation <- FALSE
  }
  
  if (!"interactionPartners" %in% base::names(params)) {
    write_log("Please specify parameter interactionPartners correctly (either
              'betweenGroups' or 'withinGroups' or 'random').", params)
    runSimulation <- FALSE
  }
  if (!base::any(base::c("betweenGroups", "withinGroups", "random") %in% params[["interactionPartners"]])) {
    write_log("Please specify parameter interactionPartners correctly (either
              'betweenGroups' or 'withinGroups' or 'random').", params)
    runSimulation <- FALSE
  }
  
  if (!"speakerProb" %in% base::names(params)) {
    params[["speakerProb"]] <- NULL
  }
  
  if (!"listenerProb" %in% base::names(params)) {
    params[["listenerProb"]] <- NULL
  }
  
  if (!"runSingleSimulation" %in% base::names(params)) {
    write_log("Please specify parameter runSingleSimulation correctly (either TRUE or FALSE).", params)
    runSimulation <- FALSE
  }
  
  if (!params[["runSingleSimulation"]] && (!"multipleABMRuns" %in% base::names(params) || params[["multipleABMRuns"]] <= 0)) {
    params[["multipleABMRuns"]] <- 1
  }
  
  if (!"nrOfSnapshots" %in% base::names(params) || params[["nrOfSnapshots"]] <= 0) {
    write_log("Please specify parameter nrOfSnapshots correctly (number higher than zero).", params)
    runSimulation <- FALSE
  }
  
  if (!"interactionsPerSnapshot" %in% base::names(params) || params[["interactionsPerSnapshot"]] < 0) {
    write_log("Please specify parameter interactionsPerSnapshot correctly (zero or higher than zero).", params)
    runSimulation <- FALSE
  }
  
  params[["nrOfInteractions"]] <- params[["nrOfSnapshots"]] * params[["interactionsPerSnapshot"]]
  params[["packageVersion"]] <- as.character(utils::packageVersion("soundChangeR"))

  return(base::list(params = params, runSimulation = runSimulation))
}
