create_population <- function(input.df, params) {

  if (!("createPopulationMethod" %in% names(params)) || is.null(params[["createPopulationMethod"]]) ) {
    method <- "speaker_is_agent"
  } else {
    method <- params[["createPopulationMethod"]]
  }

  setDT(input.df)
  sortedSpeakers <- input.df$speaker %>% unique %>% sort

  if (method == "speaker_is_agent") {
    nrOfAgents <- length(sortedSpeakers)
  } else if (method == "bootstrap") {
    if (is.null(names(params[["bootstrapPopulationSize"]]))) {
      nrOfAgents <- params[["bootstrapPopulationSize"]]
    } else {
      nrOfAgents <- sum(params[["bootstrapPopulationSize"]])
      agentGroups <- cut(seq_len(nrOfAgents),
                         breaks = c(0,cumsum(params[["bootstrapPopulationSize"]])),
                         labels = names(params[["bootstrapPopulationSize"]])
      )
      speakerGroups <- input.df[, speaker, by = group] %>% unique
    }
  } else {
    stop(paste("create_population: unrecognised createPopulationMethod:", method))
  }

  initialMemorySize <- input.df[, .N, by = speaker][, max(N)]
  if (params[["initialMemoryResampling"]]) {
    initialMemorySize <- initialMemorySize * params[["initialMemoryResamplingFactor"]]
  }

  memoryBuffer <- ceiling(params[["nrOfInteractions"]]/nrOfAgents + 10 * sqrt(params[["nrOfInteractions"]]/nrOfAgents))
  if(params[["rememberOwnTokens"]]) {
    memoryBuffer <- memoryBuffer * 2
  }

  maxMemorySize <- initialMemorySize + memoryBuffer

  population <- list()

  for (id in seq_len(nrOfAgents)) {
    if (method == "speaker_is_agent") {
      selectedSpeaker <- sortedSpeakers[id]
    } else if (method == "bootstrap") {
      if (is.null(names(params[["bootstrapPopulationSize"]]))) {
        selectedSpeaker <- sample(sortedSpeakers, 1)
      } else {
        selectedSpeaker <- speakerGroups[group == agentGroups[id], sample(speaker, 1)]
      }
    }
    population[[id]] <- create_agent(id, input.df, selectedSpeaker, maxMemorySize, params)
    if (params[["initialMemoryResampling"]]) {
      apply_resampling(population[[id]], initialMemorySize, params)
    }
  }
  return(population)
}
