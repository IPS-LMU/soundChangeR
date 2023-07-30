create_agent <- function(id, input.df, selectedSpeaker, maxMemorySize, params) {

    
  agent <- base::list()

  agent$agentID <- id
  agent$group <- input.df[speaker == selectedSpeaker, group][1]
  agent$speaker <- input.df[speaker == selectedSpeaker, speaker][1]
  
  methodReg <- get_method_register()
  # bis hierhin klappts
  cacheNames <- base::c("nFeatures", "qda", "GMM", "nAccepted", "nForgotten", methodReg[params[["featureExtractionMethod"]], cacheEntries][[1]] %>% .[!base::is.na(.)])
  agent$cache <- data.table::data.table(name = cacheNames, value = base::list(), valid = FALSE)
  set_cache_value(agent, "nAccepted", 0)
  set_cache_value(agent, "nForgotten", 0)

# das folgende if-else-Konstrukt sollte irgendwann noch kompakter geschrieben werden

# Evtl nützlich um Code kompakter zu schreiben:
# das hier sollte helfen (auch schon bei zeile 16 -30)
#X %>% 
#add(1) %>% 
 #{if(Y) add(.,1) else .}
# zu X wird eins hinzugeügt, wenn Y wahr ist, ansonsten passiert nichts
# df %>% methode, die je nach Bedingung ausgeführt wird %>% if und else statement


  if(params[["dim_cuespace"]] == 1){
      agent$memory <- data.table::data.table(word = base::character(),
                                         phoneme_set1 = base::character(),
                                         valid = base::logical(),
                                         nrOfTimesHeard = base::integer(),
                                         producerID = base::integer()
  ) %>%
    .[1:maxMemorySize] %>%
    .[, valid := FALSE] %>%
    .[, exemplar_set1 := base::list(base::list(FALSE))]

  } else{ 
  # momentan entspricht das dim_cuespace == 2
  agent$memory <- data.table::data.table(word = base::character(),
                                         phoneme_set1 = base::character(),
                                         phoneme_set2 = base::character(),
                                         valid = base::logical(),
                                         nrOfTimesHeard = base::integer(),
                                         producerID = base::integer()
  ) %>%
    .[1:maxMemorySize] %>%
    .[, valid := FALSE] %>%
    .[, exemplar_set1 := base::list(base::list(FALSE))] %>%
    .[, exemplar_set2 := base::list(base::list(FALSE))]
  }


  nInput <- input.df[speaker == selectedSpeaker, .N]
  samples <- input.df[speaker == selectedSpeaker]
if(params[["dim_cuespace"]] == 1){
  agent$memory %>%
    .[1:nInput, base::c("word", "phoneme_set1", "exemplar_set1") := samples[, .(word, phoneme_set1, exemplar_set1)]] %>%
    .[1:nInput, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)]
     agent$feature_set1 <- data.table::data.table(P1_1 = base::double()) %>% .[1:maxMemorySize]
}else{
  # momentan entspricht das dim_cuespace == 2
  agent$memory %>%
    .[1:nInput, base::c("word", "phoneme_set1", "phoneme_set2", "exemplar_set1", "exemplar_set2") := samples[, .(word, phoneme_set1, phoneme_set2, exemplar_set1, exemplar_set2)]] %>%
    .[1:nInput, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)]
     agent$feature_set1 <- data.table::data.table(P1_1 = base::double()) %>% .[1:maxMemorySize]
    agent$feature_set2 <- data.table::data.table(P2_1 = base::double()) %>% .[1:maxMemorySize]
}

  update_features(agent, compute_features(agent, params))
  # compute_features gibt eine Liste zurück: [[exemplar_matrix1], [exemplar_matrix2]] bzw. [exemplar_matrix1, [0]]
  #stop(paste(names(agent$memory),collapse = "---"))
  if (params[["useFlexiblePhonology"]]) {
    for (dim in params[["dim_cuespace"]]:1){

      if(params[["dim_cuespace"]] == 1){
       estimate_GMM(agent, params)
      } else{

      # else Tei sollte für zwei *und mehr* Cuespaces funktionieren --> mehr als zwei sind noch nicht getestet
      #dropping columns (feature_setX, phonem_setX and exemplar_setX) that should be used for GMM estimation in different loop
     agent_per_space = agent
      #agent_per_space = crunch::copy(agent, deep = T)
    
      # entfernt das feature_set, das nicht benötigt wird; hier klappt es auch, dass der Agent selber *nicht* modifiziert wird
      agent_per_space[match(paste0("feature_set", as.character(dim)), names(agent_per_space))] = NULL
      
      # entfernt phoneme und exemplarset, das nicht benötigt wird
      
      # phoneme_set und exemplar set werden sowieso bei estimate GMM nicht beachtet, deswegen müssen sie auch nicht enrfernt werden (glaub ich)
      #agent_per_space$memory = agent_per_space$memory[,paste0(c("phoneme_set", "exemplar_set"), as.character(dim)) := NULL]
      
      estimate_GMM(agent_per_space, params)
      
      }
    }
  }
  # bis hierhin klappts sowohl mit einem als auch mit zwei Cuespaces :)
  #stop(paste(names(agent$memory),collapse = "..."))
  return(agent)
}
