create_agent <- function(id, input.df, selectedSpeaker, maxMemorySize, params) {

  agent <- base::list()

  agent$agentID <- id
  agent$group <- input.df[speaker == selectedSpeaker, group][1]
  agent$speaker <- input.df[speaker == selectedSpeaker, speaker][1]
  agent$initial <- input.df[speaker == selectedSpeaker, .(word, initial)] %>% base::unique()
  
  methodReg <- get_method_register()
  cacheNames <- base::c("nFeatures", "qda", "GMM", "nAccepted", "nForgotten", methodReg[params[["featureExtractionMethod"]], cacheEntries][[1]] %>% .[!base::is.na(.)])
  agent$cache <- data.table::data.table(name = cacheNames, value = base::list(), valid = FALSE)
  set_cache_value(agent, "nAccepted", 0)
  set_cache_value(agent, "nForgotten", 0)

  agent$memory <- data.table::data.table(word = base::character(),
                                         label = base::character(),
                                         valid = base::logical(),
                                         nrOfTimesHeard = base::integer(),
                                         producerID = base::integer(),
                                         timeStamp = base::integer()
  ) %>%
    .[1:maxMemorySize] %>%
    .[, valid := FALSE] %>%
    .[, exemplar := base::list(base::list(FALSE))]

  nInput <- input.df[speaker == selectedSpeaker, .N]
  samples <- input.df[speaker == selectedSpeaker]

  agent$memory %>%
    .[1:nInput, base::c("word", "label", "exemplar") := samples[, .(word, label, exemplar)]] %>%
    .[1:nInput, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)] %>%
    .[1:nInput, timeStamp := base::sample(.N), by = word]

  agent$features <- data.table::data.table(P1 = base::double()) %>% .[1:maxMemorySize]
  update_features(agent, compute_features(agent, params))

  if (params[["useFlexiblePhonology"]]) {
    estimate_GMM(agent, params)
  }
  return(agent)
}
