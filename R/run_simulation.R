#' Run simulation
#'
#' @param inputDataFile string; path to data file
#' @param speaker string; column name for speaker code
#' @param group string; column name for agent group
#' @param word string; column name for word labels
#' @param phoneme string; column name for canonical phonological labels (does not have to be specified if useFlexiblePhonology is TRUE)
#' @param features string or vector of strings; column name(s) for acoustic features
#' @param subsetSpeakers vector of strings; speakers to be included in simulation
#' @param subsetPhonemes string or vector of strings; canonical phonological label(s) to be included in the simulation
#' @param createBootstrappedPopulation boolean; whether to create an agent population using bootstrap or have every speaker represented by one agent
#' @param bootstrapPopulationSize full positive number or named vector of full positive numbers; amount of agents (per group) in bootstrap scenario
#' @param initialMemoryResampling boolean; whether or not to increase number of tokens per word and agent before simulation start
#' @param initialMemoryResamplingFactor full positive number; factor by which to increase the agents' memories before the simulation if initialMemoryResampling is TRUE
#' @param removeOriginalExemplarsAfterResampling boolean; whether or not to remove original exemplars if initialMemoryResampling is TRUE
#' @param productionBasis "word" or "phoneme"; which categories to use as sampling basis in production
#' @param productionResampling boolean; whether or not to use SMOTE in production to make computation of Gaussians more stable
#' @param productionResamplingFallback "phoneme" or NULL;
#' @param productionMinTokens full positive number; minimum of tokens to be used to compute a Gaussian to sample from in production as well as number of tokens per word class and agent not to be undercut through forgetting
#' @param productionSMOTENN full positive number; number of nearest neighbours to use for SMOTE
#' @param memoryIntakeStrategy "acceptAll" or "mahalanobisDistance" and/or "maxPosteriorProb" or "posteriorProbThr"; decision criteria for memorisation
#' @param mahalanobisProbThreshold number between 0 and 1; probability threshold if memoryIntakeStrategy contains "mahalanobisDistance"
#' @param posteriorProbThreshold number between 0 and 1; probability threshold if memoryIntakeStrategy contains "posteriorProbThr"
#' @param perceptionOOVNN full positive number; amount of nearest neighbours to use to assign a phonological label to token of unknown word class
#' @param forgettingRate number between 0 and 1; if a number generated from a uniform distribution is smaller than this threshold, the agent listener removes a token from memory
#' @param useFlexiblePhonology boolean; whether to use GMM and NMF to autonomously compute and update phonological classes or use fixed phonological labels (as given by argument "phoneme")
#' @param computeGMMsInterval full positive number; after how many accepted new tokens to re-compute GMM and NMF if useFlexiblePhonology is TRUE
#' @param purityRepetitions positive full number; how often to recompute the purity calculation to make sure it is robust if useFlexiblePhonology is TRUE
#' @param purityThreshold number between 0 and 1; how pure a phonological class has to be at the least if useFlexiblePhonology is TRUE
#' @param interactionPartners "random" or "betweenGroups" or "withinGroups"; whether interacting agents should come from same or different groups or be randomly chosen
#' @param speakerProb NULL or vector of numbers; one number per agent to indicate how likely the agent is to become agent speaker
#' @param listenerProb NULL or vector of numbers; one number per agent to indicate how likely the agent is to become agent listener
#' @param runSingleSimulation boolean; whether to run a single simulation or multiple runs of the same simulation
#' @param multipleABMRuns full positive number; number of runs if runSingleSimulation is FALSE
#' @param nrOfSnapshots full positive number; number of snapshots, i.e. how often the state of the population shall be saved
#' @param interactionsPerSnapshot full positive number; number of interactions per snapshot
#' @param rootLogDir string; path to logging directory (will be created if it does not exist yet)
#' @param notes string; optional notes on the simulation
#'
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom data.table %like%
#' @import mclust
run_simulation <- function(inputDataFile = NULL,
                           speaker = NULL,
                           group = NULL,
                           word = NULL,
                           phoneme = NULL,
                           features = NULL,
                           subsetSpeakers = NULL,
                           subsetPhonemes = NULL,
                           createBootstrappedPopulation = FALSE,
                           bootstrapPopulationSize = 50,
                           initialMemoryResampling = FALSE,
                           initialMemoryResamplingFactor = 2.0,
                           removeOriginalExemplarsAfterResampling = FALSE,
                           productionBasis = "word",
                           productionResampling = TRUE,
                           productionResamplingFallback = "phoneme",
                           productionMinTokens = 10,
                           productionSMOTENN = 5,
                           memoryIntakeStrategy = c("mahalanobisDistance", "maxPosteriorProb"),
                           mahalanobisProbThreshold = .95,
                           posteriorProbThreshold = 1/3,
                           perceptionOOVNN = 5,
                           forgettingRate = 1,
                           useFlexiblePhonology = FALSE,
                           computeGMMsInterval = 100,
                           purityRepetitions = 5,
                           purityThreshold = 0.75,
                           interactionPartners = "betweenGroups",
                           speakerProb = NULL,
                           listenerProb = NULL,
                           runSingleSimulation = TRUE,
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

  if (base::is.null(params[["inputDataFile"]]) || !base::file.exists(params[["inputDataFile"]])) {
    stop("Your input data file does not exist. Please make sure you are entering a correct relative or absolute path to an existing data file with extension .csv or .txt.")
  }
  input.df <- load_input_data(params)
  base::saveRDS(input.df, base::file.path(logDir, "input.rds"))

  check <- validate_params(params, input.df)
  params <- check[["params"]]

  register_simulation(params)

  if (check[["runSimulation"]]) {
    if (params[["runSingleSimulation"]]) {
      params[["logDir"]] <- base::file.path(logDir, "1")
      pop <- create_population(input.df = input.df, params = params)
      save_population(pop, extraCols = base::list(snapshot = 0), logDir = params[["logDir"]])
      if (params[["nrOfInteractions"]] > 0) {
        perform_interactions(pop, params[["logDir"]], params)
      }
    } else {
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
    set_completed(params[["rootLogDir"]], params[["simulationName"]])
  }
}
