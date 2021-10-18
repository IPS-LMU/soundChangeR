create_population <- function(input.df, params) {

  if (!("createPopulationMethod" %in% base::names(params)) || base::is.null(params[["createPopulationMethod"]])) {
    method <- "speaker_is_agent"
  } else {
    method <- params[["createPopulationMethod"]]
  }

  data.table::setDT(input.df)
  sortedSpeakers <- input.df$speaker %>% base::unique() %>% base::sort()

  if (method == "speaker_is_agent") {
    nrOfAgents <- base::length(sortedSpeakers)
  } else if (method == "bootstrap") {
    if (base::is.null(base::names(params[["bootstrapPopulationSize"]]))) {
      nrOfAgents <- params[["bootstrapPopulationSize"]]
    } else {
      nrOfAgents <- base::sum(params[["bootstrapPopulationSize"]])
      agentGroups <- base::cut(base::seq_len(nrOfAgents),
                               breaks = base::c(0, base::cumsum(params[["bootstrapPopulationSize"]])),
                               labels = base::names(params[["bootstrapPopulationSize"]])
      )
      speakerGroups <- input.df[, speaker, by = group] %>% base::unique()
    }
  } else {
    stop(paste("create_population: unrecognised createPopulationMethod:", method))
  }

  initialMemorySize <- input.df[, .N, by = speaker][, base::max(N)]
  if (params[["initialMemoryResampling"]]) {
    initialMemorySize <- initialMemorySize * params[["initialMemoryResamplingFactor"]]
  }

  memoryBuffer <- base::ceiling(params[["nrOfInteractions"]]/nrOfAgents + 10 * base::sqrt(params[["nrOfInteractions"]]/nrOfAgents))
  if(params[["rememberOwnTokens"]]) {
    memoryBuffer <- memoryBuffer * 2
  }

  maxMemorySize <- initialMemorySize + memoryBuffer

  population <- base::list()

  for (id in base::seq_len(nrOfAgents)) {
    if (method == "speaker_is_agent") {
      selectedSpeaker <- sortedSpeakers[id]
    } else if (method == "bootstrap") {
      if (base::is.null(base::names(params[["bootstrapPopulationSize"]]))) {
        selectedSpeaker <- base::sample(sortedSpeakers, 1)
      } else {
        selectedSpeaker <- speakerGroups[group == agentGroups[id], base::sample(speaker, 1)]
      }
    }
    population[[id]] <- create_agent(id, input.df, selectedSpeaker, maxMemorySize, params)
    if (params[["initialMemoryResampling"]]) {
      apply_resampling(population[[id]], initialMemorySize, params)
    }
  }
  return(population)
}
