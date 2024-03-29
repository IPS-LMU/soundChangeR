#' Run simulation
#'
#' @param inputDataFile string; absolute or relative path to data file
#' @param speaker string; column name for speaker code
#' @param group string; column name for agent group
#' @param word string; column name for word labels
#' @param phoneme string; column name for canonical phonological labels (does not have to be specified if useFlexiblePhonology is TRUE)
#' @param features string or vector of strings; column name(s) for acoustic features
#' @param subsetSpeakers vector of strings; speakers to be included in simulation
#' @param subsetPhonemes string or vector of strings; canonical phonological label(s) to be included in the simulation (is automatically set to NULL if useFlexiblePhonology is TRUE)
#' @param createBootstrappedPopulation boolean; whether to create an agent population using bootstrap or have every speaker represented by one agent
#' @param bootstrapPopulationSize full positive number or named vector of full positive numbers; amount of agents (per group) in bootstrap scenario
#' @param expandMemory boolean; whether or not to increase number of tokens per word and agent before simulation start
#' @param expandMemoryFactor full positive number; factor by which to increase the agents' memories before the simulation if expandMemory is TRUE
#' @param removeOriginalExemplars boolean; whether or not to remove original exemplars after memory expansion if expandMemory is TRUE
#' @param useSMOTE boolean; whether or not to use SMOTE in production to make computation of Gaussians more stable
#' @param fallBackOnPhoneme boolean; before using SMOTE to increase number of exemplars, test whether the exemplars of the word plus the exemplars associated with the same phoneme reach minTokens; if so, use these additional tokens and do not apply SMOTE
#' @param minTokens full positive number; minimum of tokens to be used to compute a Gaussian to sample from in production as well as number of tokens per word class and agent not to be undercut through forgetting
#' @param SMOTENN full positive number; number of nearest neighbours to use for SMOTE
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
#' @param runs full positive number, 1 or higher; how many (parallel) runs of the simulation to compute
#' @param nrOfSnapshots full positive number, 1 or higher; number of snapshots, i.e. how often the state of the population shall be saved
#' @param interactionsPerSnapshot full positive number, 0 or higher; number of interactions per snapshot
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
                           expandMemory = FALSE,
                           expandMemoryFactor = 2,
                           removeOriginalExemplars = FALSE,
                           useSMOTE = TRUE,
                           fallBackOnPhoneme = TRUE,
                           minTokens = 10,
                           SMOTENN = 5,
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
                           runs = 1,
                           nrOfSnapshots = 10,
                           interactionsPerSnapshot = 100,
                           rootLogDir = "./logDir",
                           notes = "") {
  
  params <- base::as.list(base::environment())
  base::dir.create(params[["rootLogDir"]], showWarnings = FALSE, recursive = TRUE)
  create_simulation_register(params[["rootLogDir"]])

  params[["simulationName"]] <- generate_simulation_name()
  logDir <- base::file.path(params[["rootLogDir"]], params[["simulationName"]])
  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  
  input.df <- load_input_data(params)
  base::saveRDS(input.df, base::file.path(logDir, "input.rds"))
  
  check <- validate_params(params)
  params <- check[["params"]]
  register_simulation(params)

  if (check[["runSimulation"]]) {
    if (params[["runs"]] == 1) {
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
      } else {
        cl <- parallel::makeCluster(numCores, type = "FORK")
      }
      parallel::clusterSetRNGStream(cl)
      parallel::parLapply(cl, base::seq_len(params[["runs"]]), function(abmName) {
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
  } else {
    stop("Oops, something went wrong! Please check the log.txt file in the simulation directory.")
  }
}
