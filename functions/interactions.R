################################################################################
#                                                                              #
# This script contains the following functions that perform the interactions:  #
#                                                                              #
# - initialize_memory(df)                                                      #
# - create_population(nrOfAgents, initMemory = NULL)                           #
# - perform_interactions(pop, nrOfInteractions)                                #
# - perform_single_interaction(pop)                                            #
# - produce_token(agent)                                                       #
# - perceive_token(agent, producedToken)                                       #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


initialize_memory <- function(df) {
  # This function initiates the memories of the agents
  # and assigns a random time stamp to each token.
  # Function call in coreABM.R.
  #
  # Args:
  #    - df: input.df, must at least have columns speaker, initial, label, 
  #      word, group, P1, P2, and possibly other columns "P."
  #
  # Returns:
  #    - initMemory: a list of lists for each speaker in input.df
  #      with attributes P, word, age, speaker, initial, group, and nrOfTimesHeard
  #
  
  # initiate variables
  params <- as.matrix(df[, grep("P", names(df))])
  initMemory <- list()
  ageIndex <- 1:nrow(df)
  k <- 1
  
  # loop over the speakers in df
  for (j in unique(df$speaker)[order(unique(df$speaker))]) {
    speakerEntries <- df$speaker == j
    # loop over the words in df
    for (wordClass in unique(df$word)) {
      speakerWordEntries <- speakerEntries & (df$word == wordClass)
      ageIndex[speakerWordEntries] <- sample(1:sum(speakerWordEntries))
    }
    # fill initMemory
    initMemory[[k]] <- list(P = params[speakerEntries, ], 
                            word = as.character(df$word[speakerEntries]), 
                            label = as.character(df$labels[speakerEntries]),
                            age = ageIndex[speakerEntries], 
                            speaker = as.character(df$speaker[speakerEntries]), 
                            initial = as.character(df$initial[speakerEntries]), 
                            group = as.character(df$group[speakerEntries]), 
                            nrOfTimesHeard = rep(1, sum(speakerEntries)))
    if (!is.matrix(initMemory[[k]]$P)) {
      initMemory[[k]]$P <- cbind(initMemory[[k]]$P)
    }
    k <- k + 1
  }
  return(initMemory)
}

create_population <- function(input.df, method = "speaker_is_agent", maxMemorySize ) {
  population <- list()
  Pcols <- grep("^P[[:digit:]]+$", colnames(input.df), value = TRUE)
  if (method == "speaker_is_agent") {
    sortedSpeakers <- input.df$speaker %>% unique %>% sort
    nrOfAgents <- length(sortedSpeakers)
    for (id in seq_len(nrOfAgents)) {
      population[[id]] <- list()
      population[[id]]$agentID <- id
      population[[id]]$labels <- input.df[input.df$speaker == sortedSpeakers[id],
                                               c("word", "labels", "initial", "speaker", "group")] %>%
        setDT %>%
        .[, `:=`(valid = TRUE, nrOfTimesHeard = 1, agentID = id)] %>%
        .[, timeStamp := sample(.N), by = word] %>%
        .[]
      population[[id]]$group <- population[[id]]$labels[, group %>% unique]
      population[[id]]$features <- input.df[, Pcols] %>% as.matrix
      
      bufferRowsCount <- maxMemorySize - nrow(population[[id]]$labels)
      if (bufferRowsCount > 0) {
        population[[id]]$labels <- rbindlist(list(
          population[[id]]$labels,
          matrix(nrow = bufferRowsCount, ncol = ncol(population[[id]]$labels)) %>%
            data.table() %>%
            setnames(colnames(population[[id]]$labels)) %>%
            .[, valid := FALSE]
        ))
        population[[id]]$features <- rbind(population[[id]]$features,
                                           matrix(nrow = bufferRowsCount, ncol = length(Pcols)))
      }
      population[[id]]$labels[, rowIndex := .I]
      population[[id]]$cache <- list(valid = FALSE)
      population[[id]]$received <- expandingList()
    }
  }
  return(population)
}
  #   population$labels <- input.df[, c("word", "labels", "initial", "speaker", "group")] %>%
  #     setDT %>%
  #     .[, valid := TRUE] %>%
  #     # not using a data.table join as it would sort by speaker,
  #     # while original order must be preserved
  #     .[, agent := which(sortedSpeakers == speaker), by = speaker] %>%
  #     .[]
  #   
  #   emptyRowsCounts <- population$labels[, .(Count = maxMemorySize - .N), by = agent]
  #   population$labels <- rbindlist(list(
  #     population$labels,
  #     matrix(nrow = sum(emptyRowsCounts$Count), ncol = ncol(population$labels)) %>%
  #       data.table() %>%
  #       setnames(colnames(population$labels)) %>%
  #       .[, `:=`(agent = rep(emptyRowsCounts$agent, emptyRowsCounts$Count),
  #                valid = FALSE)]
  #   ))
  #   population$labels[, rowIndex := .I]
  #   setnames(population$labels, "labels", "label")
  #   
  #   population$features <- rbind(input.df[, Pcols] %>% as.matrix,
  #                                matrix(nrow = population$labels[valid == FALSE, .N], ncol = length(Pcols))
  #   )
  # }
# }



# create_population <- function(df, method = "speaker_is_agent") {
#   population <- list()
#   population$Pcols <- grep("^P[[:digit:]]+$", colnames(df), value = TRUE)
#   if (method == "speaker_is_agent") {
#     population$info <- df %>%
#       setDT %>%
#       unique(by = c("speaker", "group")) %>%
#       .[order(speaker), .(speaker, group)] %>%
#       .[, agent := .I] %>%
#       .[]
#     
#     cols <- c(population$Pcols, "word", "labels", "initial", "speaker")
#     population$memory <- lapply(population$info$agent, function(a) {
#       mem <- df %>%
#         setDT %>%
#         .[speaker == population$info[agent == a, speaker], .SD, .SDcols=cols] %>%
#         .[population$info[, .(speaker, agent)], on = "speaker", nomatch = 0] %>%
#         .[, speaker := NULL] %>%
#         setnames("labels", "label") %>%
#         .[]
#       if (nrow(mem) == maxMemorySize) {
#         return(mem)
#       } else {
#         return(rbindlist(list(mem, matrix(nrow = maxMemorySize - nrow(mem), ncol = ncol(mem)) %>% data.table)))
#       }
#     })
#   } 
#   return(population)
# }

create_population_ <- function(nrOfAgents, initMemory = NULL) {
  # This functions initiates the memories of the (chosen) agents. 
  # Function call in coreABM.R.
  # 
  # Args:
  #    - nrOfAgents: full positive number, defined in coreABM.R
  #    - initMemory: result of initiateMemory, defined in coreABM.R
  #
  # Returns:
  #    - pop: a list of lists for all speakers in input.df with attributes
  #      agentNr and memory, the latter of which has all seven attributes of
  #      initMemory
  #
  
  # initMemory must exist, else stop
  if (is.null(initMemory) || length(initMemory) != nrOfAgents) {
    stop("The length of initMemory is unequal to nrOfAgents.")
  }
  
  # initiate list called pop
  pop <- list()
  
  # for each agent, an entry in list pop is created
  # with agentNr and all corresponding list items from initMemory
  for (i in 1:nrOfAgents) {
    pop[[i]] <- list(agentNr = i, memory = initMemory[[i]])
    class(pop[[i]]) <- c("agent", class(pop[[1]]))
  }
  return(pop)
}


perform_interactions <- function(pop, nrOfInteractions) {
  # This function repeats perform_single_interaction() (see below)
  # for as many as nrOfInteractions. 
  # Function call in coreABM.R.
  #
  # Args:
  #    - pop: population as defined in coreABM.R by create_population().
  #    - nrOfInteractions: full positive number, defined as parameter 
  #      interactionsPerSimulation in param.R.
  #
  # Returns:
  #    - pop: a list, same as population defined by create_population(),
  #      but obviously with changed feature values because of the interactions.
  #
  
  # perform as many as nrOfInteractions
  for (i in 1:nrOfInteractions) {
    pop <- perform_single_interaction(pop)
  }
  return(pop)
}


perform_single_interaction <- function(pop) {
  # This function performs a single interaction. 
  # Function call in performMultipleInteraction() in this script (see above).
  #
  # Args:
  #    - pop: population as defined in coreABM.R by create_population().
  #
  # Returns:
  #    - pop: a list, same as population defined by create_population(),
  #      but obviously with changed feature values because of the interactions.
  #
  
  # make a list of all agent groups
  groupList <- NULL
  for (i in 1:length(pop)) {
    spk <- pop[[i]]
    if (!unique(spk$memory$group) %in% groupList) {
      groupList <- c(groupList, unique(spk$memory$group))
    }
  }
  
  # sample a speaking and a listening agent (which cannot be the same agent)
  # either randomly, within groups, or between groups
  prodNr <- 1
  percNr <- 1
  while (prodNr == percNr) {
    
    # choose interaction partners without taking their group into account
    if (interactionPartners == "random") {
      prodNr <- sample(1:length(pop), 1, prob = speakerProb)
      percNr <- sample(1:length(pop), 1, prob = listenerProb)
      
      # or choose interaction partners from the same group
    } else if (interactionPartners == "withinGroups") {
      randomGroup <- sample(groupList, 1)
      prodNr <- sample(1:length(pop), 1, prob = speakerProb)
      percNr <- sample(1:length(pop), 1, prob = listenerProb)
      while (unique(pop[[prodNr]]$memory$group) != randomGroup) {
        prodNr <- sample(1:length(pop), 1, prob = speakerProb)
      }
      while (unique(pop[[percNr]]$memory$group) != randomGroup) {
        percNr <- sample(1:length(pop), 1, prob = listenerProb)
      }
      
      # or choose interaction partners from different groups
    } else if (interactionPartners == "betweenGroups") {
      randomPercGroup <- groupList[1]
      randomProdGroup <- groupList[1]
      while (randomPercGroup == randomProdGroup) {
        randomPercGroup <- sample(groupList, 1)
        randomProdGroup <- sample(groupList, 1)
      }
      prodNr <- sample(1:length(pop), 1, prob = speakerProb)
      percNr <- sample(1:length(pop), 1, prob = listenerProb)
      while (unique(pop[[prodNr]]$memory$group) != randomProdGroup) {
        prodNr <- sample(1:length(pop), 1, prob = speakerProb)
      }
      while (unique(pop[[percNr]]$memory$group) != randomPercGroup) {
        percNr <- sample(1:length(pop), 1, prob = listenerProb)
      }
    }
  }
  
  # set producer and perceiver to the chosen agents from pop
  producer <- pop[[prodNr]]
  perceiver <- pop[[percNr]]
  
  # let speaking agent produce a token and listening agent perceive it
  producedToken <- produce_token(producer)
  pop[[percNr]] <- perceive_token(perceiver, producedToken)
  
  return(pop)
}


produce_token <- function(agent) {
  # This function randomly samples a token from one of the
  # feature distributions of one agent (the speaking agent).
  # Function call in perform_single_interaction() in this script (see above).
  #
  # Args:
  #    - agent: one of the agents from population
  #
  # Returns:
  #    - producedToken: a data.frame with columns P..., word, label, age,
  #      initial, speaker, and group
  #

  # create a random index
  randomIndex <- sample(1:length(agent$memory$word), 1)

  # define word and label according to the random index
  # define two logical vectors based on variables word and label
  word <- agent$memory$word[randomIndex]
  label <- agent$memory$label[randomIndex]
  tempWord <- agent$memory$word == word
  tempLabel <- agent$memory$label == label

  # define df as a data.frame with columns P... from the agent's memory
  if (plyr::count(tempWord[tempWord == T])$freq != 1) {
    df <- as.data.frame(agent$memory$P[tempWord, ])
  } else {
    df <- as.data.frame(t(agent$memory$P[tempWord, ]))
  }

  # add rows to df for all words that have the same label
  # in order to amplify the number of values to average over
  for (wordclass in unique(agent$memory$word[tempLabel == T])) {
    if (wordclass != word) {
      values <- as.data.frame(agent$memory$P[agent$memory$word == wordclass, ])
      df <- rbind(df, colMeans(values))
    }
  }

  # sample values from a Gaussian distribution built on df
  mu <- apply(df, 2, mean)
  sigma <- cov(df)
  token <- rmvnorm(1, mu, sigma)

  # create data.frame producedToken
  label <- agent$memory$label[randomIndex]
  age <- agent$memory$age[randomIndex]
  initial <- agent$memory$initial[randomIndex]
  speaker <- agent$memory$speaker[randomIndex]
  group <- agent$memory$group[randomIndex]
  producedToken <- data.frame(P = token, word, label, age, initial, speaker, group, stringsAsFactors = F)
  names(producedToken)[1:length(token)] <- paste("P", 1:length(token), sep = "")
  return(producedToken)
}


perceive_token <- function(agent, producedToken) {
  # This function tests whether the produced token shall be 
  # memorized by the listening agent.
  # Function call in perform_single_interaction() in this script (see above).
  #
  # Args:
  #    - agent: a list that is part of a population
  #    - producedToken: a data.frame, result of produce_token()
  #
  # Returns:
  #    - agent: the same list, but may now have a new token in memory
  #
  
  # check the incoming token
  recognized <- F
  columnNames <- grep("P", names(producedToken), value = T)
  incomingValues <- cbind(producedToken[, columnNames])
  incomingLabel <- unique(agent$memory$label[agent$memory$word == producedToken$word])
  
  # decide on whether or not to memorize the incoming token
  # either with a maximum posterior probability decision or mahalanobis threshold
  if (memoryIntakeStrategy == "maxPosteriorProb") {
    # perform quadratic discriminant analysis on agent's values and labels
    values <- as.matrix(agent$memory$P)
    labels <- agent$memory$label
    qdaResult <- qda(values, grouping = labels)
    
    # calculate posterior prob of the incoming and all other possible labels
    posteriorProbAll <- predict(qdaResult, incomingValues)$posterior
    posteriorProbIncoming <- posteriorProbAll[colnames(posteriorProbAll) == incomingLabel]
    
    # decide if token is to be memorized
    recognized <- colnames(posteriorProbAll)[which.max(posteriorProbAll)] == incomingLabel
    
  } else if (memoryIntakeStrategy == "mahalanobisDistance") {
    # compute mahalanobis distance for the incoming values
    mahalaDistanceLabel <- log(mahalanobis(incomingValues, colMeans(agent$memory$P), cov(agent$memory$P)))
    recognized <- mahalaDistanceLabel < mahalanobisThreshold
  }
  
  # set incomingToken
  incomingToken <- c(producedToken$speaker, producedToken$label, producedToken$age, producedToken$word, 
                     producedToken$initial, producedToken$group)
  
  # process of memorization: look into memory size of agent,
  # check if a token needs to be removed prior to the memorization,
  # and then incorporate token into memory
  if (recognized == T) {
    incomingToken <- c(incomingToken, "y")
    listenersTokens <- length(agent$memory$word)
    
    # in case the agent's memory is full, apply removal strategy
    if (listenersTokens >= maxMemorySize) {
      # remove the oldest token
      if (memoryRemovalStrategy == "timeDecay") {
        ageTemp <- agent$memory$age
        ageTemp[agent$memory$word != producedToken$word] <- NA
        tokenToRemove <- which.min(ageTemp)
        agent$memory$nrOfTimesHeard <- agent$memory$nrOfTimesHeard[-tokenToRemove]
        agent$memory$P <- agent$memory$P[-tokenToRemove, ]
        agent$memory$word <- agent$memory$word[-tokenToRemove]
        agent$memory$label <- agent$memory$label[-tokenToRemove]
        agent$memory$age <- agent$memory$age[-tokenToRemove]
        agent$memory$initial <- agent$memory$initial[-tokenToRemove]
        agent$memory$speaker <- agent$memory$speaker[-tokenToRemove]
        agent$memory$group <- agent$memory$group[-tokenToRemove]
        # or remove the farthest outlier of the correspoding feature distribution
      } else if (memoryRemovalStrategy == "outlierRemoval") {
        temp.mahal <- agent$memory$label == incomingLabel
        tdat.mahal <- train(as.matrix(agent$memory$P[temp.mahal, ]))
        temp.word.mahal <- agent$memory$word == producedToken$word
        nums.mahal <- 1:length(agent$memory$word)
        nums.mahal <- nums.mahal[temp.word.mahal]
        dist.mahal <- distance(agent$memory$P[temp.word.mahal, ], tdat.mahal, metric = "mahal")
        tokenToRemove <- nums.mahal[which.max(dist.mahal)]
        agent$memory$nrOfTimesHeard <- agent$memory$nrOfTimesHeard[-tokenToRemove]
        agent$memory$P <- agent$memory$P[-tokenToRemove, ]
        agent$memory$P <- cbind(agent$memory$P)
        agent$memory$word <- agent$memory$word[-tokenToRemove]
        agent$memory$label <- agent$memory$label[-tokenToRemove]
        agent$memory$age <- agent$memory$age[-tokenToRemove]
        agent$memory$speaker <- agent$memory$speaker[-tokenToRemove]
        agent$memory$group <- agent$memory$group[-tokenToRemove]
        agent$memory$initial <- agent$memory$initial[-tokenToRemove]
      }
    }
    # update the agent's memory
    agent$memory$update <- c(agent$memory$update, "y")
    agent$memory$nrOfTimesHeard <- c(agent$memory$nrOfTimesHeard, 
                                     unique(agent$memory$nrOfTimesHeard[agent$memory$word == producedToken$word]))
    agent$memory$P <- rbind(agent$memory$P, incomingValues)
    agent$memory$label <- c(agent$memory$label, incomingLabel)
    agent$memory$initial <- c(agent$memory$initial, producedToken$initial)
    agent$memory$speaker <- c(agent$memory$speaker, agent$memory$speaker[1])
    agent$memory$group <- c(agent$memory$group, agent$memory$group[1])
    newAgeIndex <- max(agent$memory$age[agent$memory$word == producedToken$word]) + 1
    agent$memory$age <- c(agent$memory$age, newAgeIndex)
    agent$memory$word <- c(agent$memory$word, producedToken$word)
    agent$memory$incoming <- rbind(agent$memory$incoming, incomingToken)
    temp <- agent$memory$word == producedToken$word
    agent$memory$nrOfTimesHeard[temp] <- agent$memory$nrOfTimesHeard[temp] + 1
    # else if the token was not recognized: do not update memory
  } else if (recognized == F) {
    agent$memory$update = c(agent$memory$update, "n")
    incomingToken = c(incomingToken, "n")
    agent$memory$incoming = rbind(agent$memory$incoming, incomingToken)
  }
  
  # apply split&merge if needed
  numReceivedTokens <- length(agent$memory$update)
  if (splitAndMerge == T & numReceivedTokens %% splitAndMergeInterval == 0) {
    agent <- splitandmerge(agent)
  }
  return(agent)
}

