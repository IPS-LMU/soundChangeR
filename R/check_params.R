check_params <- function(params, input.df) {

  runSimulation <- TRUE

  if (params[["splitAndMerge"]]) {
    params[["perceptionModels"]] <- "singleGaussian"
  }

  if (is.null(params[["initialMemoryResampling"]])) {
    params[["initialMemoryResampling"]] <- FALSE
  }
  if (params[["initialMemoryResampling"]]) {
    if (is.null(params[["initialMemoryResamplingFactor"]])) {
      params[["initialMemoryResamplingFactor"]] <- 1
    } else if (!is.numeric(params[["initialMemoryResamplingFactor"]]) |
               params[["initialMemoryResamplingFactor"]] <= 0) {
      runSimulation <- FALSE
    }
  }
  if (is.null(params[["removeOriginalExemplarsAfterResampling"]])) {
    params[["removeOriginalExemplarsAfterResampling"]] <- FALSE
  }

  if (any(c("maxPosteriorProb", "posteriorProbThr") %in% params[["memoryIntakeStrategy"]])) {
    if (is.null(params[["posteriorProbMethod"]])) {
      if (is.null(params[["perceptionModels"]]) || params[["perceptionModels"]] == "singleGaussian") {
        params[["posteriorProbMethod"]] <- "qda"
      } else if (grepl("^GMM(s)?", params[["perceptionModels"]])) {
        params[["posteriorProbMethod"]] <- "GMM"
      }
    }
  }

  params[["nrOfInteractions"]] <- params[["nrOfSnapshots"]] * params[["interactionsPerSnapshot"]]

  if (is.null(params[["perceptionOVNN"]])) {
    params[["perceptionOVNN"]] <- 5
  }
  if (params[["perceptionOVNN"]] %% 2 == 1) {
    params[["perceptionOVNN"]] <- params[["perceptionOVNN"]] + 1
  }

  if (!"featureExtractionMethod" %in% names(params)) {
    params[["featureExtractionMethod"]] <- "identity"
  }
  if (!params[["featureExtractionMethod"]] %in% methodReg$method) {
    runSimulation <- FALSE
  }

  if (params[["splitAndMerge"]] && !any(c("t.test", "bic") %in% params[["splitMergeMethod"]])) {
    params[["splitMergeMethod"]] <- "bic"
  }

  if (params[["proportionGroupTokens"]] != 0) {
    groupData <- input.df %>% group_by(group) %>% dplyr::summarise(nInputGroupAvailable = dplyr::n())
    df <- input.df %>% group_by(speaker, group) %>% dplyr::summarise(nInput = dplyr::n()) %>%
      mutate(nInputGroupNeeded = ceiling(nInput * params[["proportionGroupTokens"]])) %>%
      dplyr::left_join(groupData, by = "group") %>% mutate(nInputGroupAvailable = nInputGroupAvailable - nInput)
    if (any(df$nInputGroupNeeded > df$nInputGroupAvailable)) {
      runSimulation <- FALSE
    }
  }

  if (!params[["splitAndMerge"]] && params[["computeGMMsInterval"]] != params[["computeFeaturesInterval"]]) {
    params[["computeGMMsInterval"]] <- params[["computeFeaturesInterval"]]
  }
  if (params[["splitAndMerge"]] && params[["splitAndMergeInterval"]] != params[["computeFeaturesInterval"]]) {
    params[["splitAndMergeInterval"]] <- params[["computeFeaturesInterval"]]
  }

  if (params[["featureExtractionMethod"]] == "FPCA") {
    if (packageVersion("fda") > "2.4.0") {
      runSimulation <- FALSE
    }
  }

  return(list(params, runSimulation))
}
