check_params <- function(params, input.df) {

  runSimulation <- TRUE

  if (base::is.null(params[["initialMemoryResampling"]])) {
    params[["initialMemoryResampling"]] <- FALSE
  }
  if (params[["initialMemoryResampling"]]) {
    if (base::is.null(params[["initialMemoryResamplingFactor"]])) {
      params[["initialMemoryResamplingFactor"]] <- 1
    } else if (!base::is.numeric(params[["initialMemoryResamplingFactor"]]) | params[["initialMemoryResamplingFactor"]] <= 0) {
      runSimulation <- FALSE
    }
  }
  if (base::is.null(params[["removeOriginalExemplarsAfterResampling"]])) {
    params[["removeOriginalExemplarsAfterResampling"]] <- FALSE
  }

  if (base::any(base::c("maxPosteriorProb", "posteriorProbThr") %in% params[["memoryIntakeStrategy"]])) {
    if (base::is.null(params[["posteriorProbMethod"]])) {
      if (base::is.null(params[["perceptionModels"]]) || params[["perceptionModels"]] == "singleGaussian") {
        params[["posteriorProbMethod"]] <- "qda"
      } else if (base::grepl("^GMM(s)?", params[["perceptionModels"]])) {
        params[["posteriorProbMethod"]] <- "GMM"
      }
    }
  }

  params[["nrOfInteractions"]] <- params[["nrOfSnapshots"]] * params[["interactionsPerSnapshot"]]

  if (base::is.null(params[["perceptionOVNN"]])) {
    params[["perceptionOVNN"]] <- 5
  }
  if (params[["perceptionOVNN"]] %% 2 == 1) {
    params[["perceptionOVNN"]] <- params[["perceptionOVNN"]] + 1
  }

  methodReg <- get_method_register()
  if (!"featureExtractionMethod" %in% base::names(params)) {
    params[["featureExtractionMethod"]] <- "identity"
  }
  if (!params[["featureExtractionMethod"]] %in% methodReg$method) {
    runSimulation <- FALSE
  }

  if (params[["proportionGroupTokens"]] != 0) {
    groupData <- input.df %>% 
      dplyr::group_by(group) %>% 
      dplyr::summarise(nInputGroupAvailable = dplyr::n())
    df <- input.df %>% 
      dplyr::group_by(speaker, group) %>% 
      dplyr::summarise(nInput = dplyr::n()) %>%
      dplyr::mutate(nInputGroupNeeded = base::ceiling(nInput * params[["proportionGroupTokens"]])) %>%
      dplyr::left_join(groupData, by = "group") %>% 
      dplyr::mutate(nInputGroupAvailable = nInputGroupAvailable - nInput)
    if (base::any(df$nInputGroupNeeded > df$nInputGroupAvailable)) {
      runSimulation <- FALSE
    }
  }

  if (params[["computeGMMsInterval"]] != params[["computeFeaturesInterval"]]) {
    params[["computeGMMsInterval"]] <- params[["computeFeaturesInterval"]]
  }
  
  return(base::list(params = params, runSimulation = runSimulation))
}
