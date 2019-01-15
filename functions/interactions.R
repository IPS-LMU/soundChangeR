################################################################################
#                                                                              #
# This script contains the following functions that perform the interactions:  #
#                                                                              #
# - create_population(input.df, method = "speaker_is_agent", maxMemorySize)    #
# - create_interactions_log(nrOfInteractions)                                  #
# - perform_interactions(pop, nrOfInteractions)                                #
# - perform_single_interaction(pop, interactionsLog, groupsInfo)               #
# - produce_token(agent)                                                       #
# - perceive_token(perceiver, producedToken, interactionsLog                   #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


create_population <- function(input.df, method = "speaker_is_agent") {
  # This function creates the agent population.
  # 
  # Args:
  #    - input.df: the input data.frame
  #    - method: string. Default: "speaker_is_agent"
  #
  # Returns:
  #    - population: a list
  #
  
  setDT(input.df)
  # override maxMemorySize
  maxMemorySize <- round(params[['maxMemoryExpansion']] * input.df[, .N, by = speaker][, max(N)])
  # initiate a list called population and search for P-columns in input.df
  population <- list()
  Pcols <- grep("^P[[:digit:]]+$", colnames(input.df), value = TRUE)
  
  # currently no other possibility than method == "speaker_is_agent"
  if (method == "speaker_is_agent") {
    sortedSpeakers <- input.df$speaker %>% unique %>% sort
    nrOfAgents <- length(sortedSpeakers)
    
    # for every agent in population, add information from input.df
    for (id in seq_len(nrOfAgents)) {
      population[[id]] <- list()
      population[[id]]$agentID <- id
      population[[id]]$labels <- input.df[speaker == sortedSpeakers[id],
                                               .(word, label, initial)] %>%
        .[, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)] %>%
        .[, timeStamp := sample(.N), by = word] %>%
        .[]
      population[[id]]$group <-input.df[speaker == sortedSpeakers[id], group][1] 
      population[[id]]$speaker <- input.df[speaker == sortedSpeakers[id], speaker][1] 
      population[[id]]$features <- input.df[speaker == sortedSpeakers[id], .SD, .SDcols = Pcols]
      
      bufferRowsCount <- maxMemorySize - nrow(population[[id]]$labels)
      if (bufferRowsCount > 0) {
        population[[id]]$labels <- rbindlist(list(
          population[[id]]$labels,
          matrix(nrow = bufferRowsCount, ncol = ncol(population[[id]]$labels)) %>%
            data.table() %>%
            setnames(colnames(population[[id]]$labels)) %>%
            .[, valid := FALSE]
        ))
        population[[id]]$features <- rbindlist(list(population[[id]]$features,
                                           matrix(nrow = bufferRowsCount, ncol = length(Pcols)) %>%
                                             data.table()
                                           ))
      }
      # setnames(population[[id]]$labels, "labels", "label")
      population[[id]]$cache <- data.table(name = "qda", value = list(), valid = FALSE)
    }
  }
  return(population)
}


create_interactions_log <- function(nrOfInteractions) {
  # This function creates a log for every interaction.
  # Function call in perform_interactions().
  # 
  # Args:
  #    - nrOfInteractions: variable created and altered in coreABM.R
  #
  # Returns:
  #    - interactionsLog: a data.table
  #
  interactionsLog <- data.table(word = NA_character_,
                                producerID = NA_integer_, producerLabel = NA_character_,
                                producerNrOfTimesHeard= NA_integer_,
                                perceiverID = NA_integer_, perceiverLabel = NA_character_,
                                perceiverNrOfTimesHeard= NA_integer_,
                                accepted = NA, simulationNr = NA_integer_, valid = NA)[0]

  interactionsLog <- rbindlist(list(
    interactionsLog,
    data.table(matrix(nrow = nrOfInteractions, ncol = ncol(interactionsLog)))
    )) %>% 
    .[, valid := FALSE] %>%
    .[]
}

perform_interactions <- function(pop, logDir) {
  # This function repeats perform_single_interaction() (see below)
  # for as many as nrOfInteractions. 
  # Function call in coreABM.R.
  #
  # Args:
  #    - pop: population as defined in coreABM.R by create_population().
  #    - nrOfInteractions: full positive number, defined as parameter 
  #      params[['interactionsPerSnapshot']] in params.R.
  #
  # Returns:
  #    - interactionsLog: a data.table
  #
  interactionsLog <- create_interactions_log(params[['interactionsPerSnapshot']] * params[['nrOfSnapshots']])
  groupsInfo <- rbindlist(lapply(pop, function(agent) {data.table(agentID = agent$agentID, group = agent$group)}))[order(agentID),]
  for (nrSim in 1:params[['nrOfSnapshots']]) {
    for (i in 1:params[['interactionsPerSnapshot']]) {
      perform_single_interaction(pop, interactionsLog, nrSim, groupsInfo)
    }
    savePopulation(pop, extraCols = list(condition = nrSim), logDir = logDir)
  }
  return(interactionsLog)
}


perform_single_interaction <- function(pop, interactionsLog, nrSim, groupsInfo) {
  # This function performs a single interaction. 
  # Function call in perform_interactions() in this script (see above).
  #
  # Args:
  #    - pop: population as defined in coreABM.R by create_population() or
  #      by perform_interactions(), respectively.
  #    - interactionsLog: a data.table
  #    - groupsInfo: a data.table containing the agents' IDs and groups,
  #      ordered by ID
  #
  # Returns:
  #    - nothing. Simply changes the input.
  #
  
  prodNr <- 1
  percNr <- 1
  while (prodNr == percNr) {
    # choose interaction partners without taking their group into account
    if (params[['interactionPartners']] == "random") {
      prodNr <- sample(groupsInfo$agentID, 1, prob = params[['speakerProb']])
      percNr <- sample(groupsInfo$agentID, 1, prob = params[['listenerProb']])
      
      # or choose interaction partners from the same group
    } else if (params[['interactionPartners']] == "withinGroups") {
      randomGroup <- sample(unique(groupsInfo$group), 1)
      prodNr <- sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1, prob = params[['speakerProb']][groupsInfo$group == randomGroup])
      percNr <- sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1, prob = params[['listenerProb']][groupsInfo$group == randomGroup])
      
      # or choose interaction partners from different groups
    } else if (params[['interactionPartners']] == "betweenGroups") {
      randomGroups <- sample(unique(groupsInfo$group), 2)
      randomPercGroup <- randomGroups[1]
      randomProdGroup <- randomGroups[2]
      prodNr <- sample(groupsInfo$agentID[groupsInfo$group == randomPercGroup], 1, prob = params[['speakerProb']][groupsInfo$group == randomPercGroup])
      percNr <- sample(groupsInfo$agentID[groupsInfo$group == randomProdGroup], 1, prob = params[['listenerProb']][groupsInfo$group == randomProdGroup])
    } else if (params[['interactionPartners']] == "selfTalk") {
      prodNr <- sample(groupsInfo$agentID, 1, prob = params[['speakerProb']])
      percNr <- 0 # temp hack
    }
  }
  
  # temp hack
  if (params[['interactionPartners']] == "selfTalk") {
    percNr <- prodNr
  }
  # set producer and perceiver to the chosen agents from pop
  producer <- pop[[prodNr]]
  perceiver <- pop[[percNr]]
  # let speaking agent produce a token and listening agent perceive it
  pt <- produce_token(producer)
  perceive_token(perceiver, pt, interactionsLog, nrSim)
}



produce_token <- function(agent) {
  # This function simulates the production of a token as realisation 
  # of a randomly selected word.
  # The token is generated by first estimating a Gaussian distribution 
  # for the given word, then sampling that distribution.
  # feature distributions of one agent (the speaking agent).
  # Function call in perform_single_interaction() in this script (see above).
  #
  # Args:
  #    - agent: one of the agents from population
  #
  # Returns:
  #    - producedToken: a list
  #
  
  randomIndex <- sample(which(agent$labels$valid == TRUE), 1)
  producedWord <- agent$labels$word[randomIndex]
  
  # sample a random word and get its corresponding labels
  # producedWord <- agent$labels$word[agent$labels$valid == TRUE] %>% unique() %>% sort %>% sample(1)
  # randomIndex <- which(agent$labels$word == producedWord)[1]
  producedLabel <- agent$labels$label[randomIndex]
  
  nWordTokens <- sum(agent$labels$word == producedWord, na.rm = TRUE)
  nExtraTokens <- 0
  if (params[['productionStrategy']] == "meanWords") {
    # get acoustic values for word tokens and an averaged value
    # per word of the same phoneme category
    otherWords <- unique(agent$labels$word[
      agent$labels$label == producedLabel & agent$labels$word != producedWord & agent$labels$valid == TRUE
      ])
    nExtraTokens <- length(otherWords)
    # print(paste(agent$speaker, producedLabel, producedWord, "nExtraTokens", nExtraTokens))
  } else if (params[['productionStrategy']] == "extraTokens") {
    nExtraTokens <- round(params[['productionExtraTokensRatio']] * nWordTokens)
  } else if (params[['productionStrategy']] == "SMOTE" & nWordTokens < params[['productionMinTokens']]) {
    nExtraTokens <- ceiling((params[['productionMinTokens']] - nWordTokens) / max(nWordTokens, params[['productionSMOTENN']] + 1)) * max(nWordTokens, params[['productionSMOTENN']] + 1)
  }
  wordFeatures <- matrix(nrow = nWordTokens + nExtraTokens, ncol = ncol(as.matrix(agent$features)))
  wordFeatures[1:nWordTokens, ] <- as.matrix(agent$features)[agent$labels$word == producedWord & agent$labels$valid == TRUE, , drop = FALSE]
  if (params[['productionStrategy']] == "meanWords") {
    for (i in 1:nExtraTokens) {
      wordFeatures[nWordTokens + i, ] <- apply(as.matrix(agent$features)[agent$labels$word == otherWords[i] & agent$labels$valid == TRUE, , drop = FALSE],
                                               2,
                                               mean)
    }
  } else if (params[['productionStrategy']] == "extraTokens" & nExtraTokens > 0) {
    wordFeatures[(nWordTokens + 1):(nWordTokens + nExtraTokens), ] <-  as.matrix(agent$features)[
      sample(which(agent$labels$word != producedWord &
                     agent$labels$label == producedLabel &
                     agent$labels$valid == TRUE), nExtraTokens, replace = TRUE), , drop = FALSE]
  } else if (params[['productionStrategy']] == "SMOTE" & nExtraTokens > 0) {
    wordIndicesWithinLabel <- which(agent$labels$word[
      agent$labels$label == producedLabel &
        agent$labels$valid == TRUE] == producedWord)
    # wordIndicesWithinLabel <- agent$labels[label == producedLabel,][word == producedWord, which = TRUE]
    fallbckIndices <- knearest_Fallback(
      as.matrix(agent$features)[agent$labels$label == producedLabel &
                                  agent$labels$valid == TRUE,],
      wordIndicesWithinLabel,
      params[['productionSMOTENN']]
      )
    wordFeatures[(nWordTokens + 1):(nWordTokens + nExtraTokens), ] <- SMOTE(
      agent$features[agent$labels$label == producedLabel &
                                  agent$labels$valid == TRUE, ][fallbckIndices, ],
      rep("a", length(fallbckIndices)),
      params[['productionSMOTENN']],
      nExtraTokens/length(fallbckIndices)
    )$syn_data[,-(ncol(agent$features) + 1)] %>% as.matrix
  }
  
  
  if (params[['productionStrategy']] %in% c("targetWordTokens", "meanWords", "extraTokens", "SMOTE")) {
    tokenGauss <- list(
    mean = apply(wordFeatures, 2, mean),
    cov = cov(wordFeatures)
    )
  } else if (params[['productionStrategy']] == "MAP") {
    tokenGauss <- MAPadaptGaussian(wordFeatures,
                                   as.matrix(agent$features)[agent$labels$label == producedLabel &
                                                               agent$labels$valid == TRUE, ],
                                   params[['productionMAPPriorAdaptRatio']]
                                   )
  }
  epsilon_diag <- 1e-6
  while (!is.positive.definite(tokenGauss$cov)) {
    tokenGauss$cov <- tokenGauss$cov + epsilon_diag * diag(nrow(tokenGauss$cov))
    epsilon_diag <- 2 * epsilon_diag
    print("non-positive def cov in production")
  }
  # generate producedToken as a list
  producedToken <- list(
    features = rmvnorm(1, tokenGauss$mean, tokenGauss$cov),
    labels = agent$labels[randomIndex,
                          .(word, label, nrOfTimesHeard)][
                            , `:=`(producerID = agent$agentID)][
                            ]
  )
  return(producedToken)
}


perceive_token <- function(perceiver, producedToken, interactionsLog, nrSim) {
  # This function tests whether the produced token is to be 
  # memorized by the listening agent.
  # Function call in perform_single_interaction() in this script (see above).
  #
  # Args:
  #    - perceiver: an agent from the population
  #    - producedToken: a data.frame, result of produce_token()
  #    - interactionsLog: a data.table
  #    - nrSim: simulation number
  #
  # Returns:
  #    - nothing. Overwrites one row in the main data.table.
  #
  
  perceiverLabel_ <- unique(perceiver$labels$label[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE])
  # if word is unknown, assign label by looking at perceptionOOVNN nearest neighbours
  if (length(perceiverLabel_) == 0) {
    perceiverLabel_ <- names(which.max(table(perceiver$labels$label[perceiver$labels$valid == TRUE][
      knnx.index(perceiver$features[perceiver$labels$valid == TRUE,], producedToken$features, params[['perceptionOOVNN']])
      ])))
  }
  # find out whether the token is recognized or not
  # ... either by maximum posterior probability decision
  if (params[['memoryIntakeStrategy']] %in% c("maxPosteriorProb", "posteriorProbThr")) {
    if (!{cacheRow <- which(perceiver$cache$name == "qda"); perceiver$cache$valid[cacheRow]}) {
      perceiver$cache[cacheRow,  `:=`(value = list(qda(as.matrix(perceiver$features)[perceiver$labels$valid == TRUE, ],
                             grouping = perceiver$labels$label[perceiver$labels$valid == TRUE])),
                             valid = TRUE)]
    } 
    posteriorProbAll <- predict(perceiver$cache$value[cacheRow][[1]], producedToken$features)$posterior
    if (params[['memoryIntakeStrategy']] == "maxPosteriorProb") {
      recognized <- colnames(posteriorProbAll)[which.max(posteriorProbAll)] == perceiverLabel_
    } else if (params[['memoryIntakeStrategy']] == "posteriorProbThr") {
      recognized <- posteriorProbAll[, perceiverLabel_] >= params[['posteriorProbThr']]
    }
    # or a Mahalanobis distance measurement
  } else if (params[['memoryIntakeStrategy']] == "mahalanobisDistance") {
    mahalaDistanceLabel <- mahalanobis(producedToken$features,
                                           apply(as.matrix(perceiver$features)[perceiver$labels$valid == TRUE & perceiver$labels$label == perceiverLabel_, ], 2, mean),
                                           cov(as.matrix(perceiver$features)[perceiver$labels$valid == TRUE & perceiver$labels$label == perceiverLabel_, ]))
    recognized <- mahalaDistanceLabel <= params[['mahalanobisThreshold']]
  }
  
  # if the token is recognized, test for memory capacity
  if (recognized) {
    # if memory is full, delete one token
    if (all(perceiver$labels$valid)) {
      # ... either the oldest token
      if (params[['memoryRemovalStrategy']] == "timeDecay") {
        rowToWrite <- which(perceiver$labels$word == producedToken$labels$word)[
          which.min(perceiver$labels$timeStamp[perceiver$labels$word == producedToken$labels$word])
          ]
        # ... or the farthest outlier of the token distribution
      } else if (params[['memoryRemovalStrategy']] == "outlierRemoval") {
        tdat.mahal <- train(as.matrix(perceiver$features)[perceiver$labels$label == perceiverLabel_, ])
        rowToWrite <- which(perceiver$labels$word == producedToken$labels$word)[
          which.max(distance(as.matrix(perceiver$features)[perceiver$labels$word == producedToken$labels$word, ], tdat.mahal, metric = "mahal"))
          ]
      } else if (params[['memoryRemovalStrategy']] == "random") {
        rowToWrite <- sample(which(perceiver$labels$word == producedToken$labels$word), 1)
      }
    }
    # if there is still some capacity, use an empty row of the memory
    else {
      rowToWrite <- which(perceiver$labels$valid == FALSE)[1]
    }
    
    # write in memory
    updatedNrOfTimesHeard <- 1 + max(0, perceiver$labels$nrOfTimesHeard[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE][1], na.rm = TRUE)
    receivedTimeStamp <- 1 + max(0, perceiver$labels$timeStamp[perceiver$labels$word == producedToken$labels$word], na.rm = TRUE)
    perceiverInitial <- perceiver$labels$initial[perceiver$labels$label == perceiverLabel_ & perceiver$labels$valid == TRUE][1]
    perceiver$features[rowToWrite, names(perceiver$features) := as.list(producedToken$features)]
    perceiver$labels[rowToWrite, `:=`(
      word = producedToken$labels$word,
      label = perceiverLabel_,
      initial = perceiverInitial,
      valid = TRUE,
      producerID = producedToken$labels$producerID,
      timeStamp = receivedTimeStamp
    )]
    perceiver$labels[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE,
                     nrOfTimesHeard := updatedNrOfTimesHeard]
    if (params[['memoryIntakeStrategy']] %in% c("maxPosteriorProb", "posteriorProbThr")) {
      perceiver$cache[cacheRow, valid := FALSE]
    }
  }
  # write on interactionsLog
  rowToWrite <- which(interactionsLog$valid == FALSE)[1]
  interactionsLog[rowToWrite, `:=`(
    word = producedToken$labels$word,
    producerID = producedToken$labels$producerID,
    producerLabel = producedToken$labels$label,
    producerNrOfTimesHeard = producedToken$labels$nrOfTimesHeard,
    perceiverID = perceiver$agentID,
    perceiverLabel = perceiverLabel_,
    perceiverNrOfTimesHeard = {
      if (recognized) {
        updatedNrOfTimesHeard
      } else {
        as.integer(max(1, perceiver$labels$nrOfTimesHeard[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE][1]))
      }
    },
    accepted = recognized,
    simulationNr = nrSim,
    valid = TRUE
  )]
  # apply split&merge if needed
  numReceivedTokens <- sum(interactionsLog$valid[interactionsLog$perceiverID == perceiver$agentID], na.rm = TRUE)
  if (params[['splitAndMerge']] == T & numReceivedTokens %% params[['splitAndMergeInterval']] == 0) {
    splitandmerge(perceiver, full = FALSE)
    if (params[['memoryIntakeStrategy']] == "maxPosteriorProb") {
      perceiver$cache[cacheRow, valid := FALSE]
    }
  }
}

