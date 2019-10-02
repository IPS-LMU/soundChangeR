################################################################################
#                                                                              #
# This script contains the functions that perform the interactions.            #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

create_population <- function(input.df, params, method = "speaker_is_agent") {
  # This function creates the agent population.
  # Function call in loadLibraries.R, coreABM().
  # 
  # Args:
  #    - input.df: the input data.frame
  #    - params: list of params from params.R
  #    - method: string. Default: "speaker_is_agent"
  #
  # Returns:
  #    - population: a list
  #
  
  setDT(input.df)
  
  # set maxMemorySize
  maxMemorySize <- round(params[['maxMemoryExpansion']] * input.df[, .N, by = speaker][, max(N)])
  
  # initiate a list called population and search for P-columns in input.df
  population <- list()
  Pcols <- grep("^P[[:digit:]]+$", colnames(input.df), value = TRUE)
  
  # currently no other possibility than method == "speaker_is_agent";
  # however, it is planned to implement the possibility that an agent does not only have 
  # tokens from one real speaker, but also some tokens from other speakers of the same group
  if (method == "speaker_is_agent") {
    sortedSpeakers <- input.df$speaker %>% unique %>% sort
    nrOfAgents <- length(sortedSpeakers)
    
    # for every agent in population, create a list and add information from input.df
    for (id in seq_len(nrOfAgents)) {
      population[[id]] <- list()
      population[[id]]$agentID <- id
      population[[id]]$labels <- input.df[speaker == sortedSpeakers[id], .(word, label)] %>%
        .[, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)] %>%
        .[, timeStamp := sample(.N), by = word] %>%
        .[]
      population[[id]]$group <-input.df[speaker == sortedSpeakers[id], group][1]
      population[[id]]$speaker <- input.df[speaker == sortedSpeakers[id], speaker][1]
      population[[id]]$features <- input.df[speaker == sortedSpeakers[id], .SD, .SDcols = Pcols]
      population[[id]]$initial <- input.df[speaker == sortedSpeakers[id], .(word, initial)] %>% unique
      population[[id]]$cache <- data.table(name = "qda", value1 = list(), value2 = list(), valid = FALSE)
      
      bufferRowsCount <- maxMemorySize - nrow(population[[id]]$labels)
      # if an agent's memory is not yet as long as maxMemorySize allocate some more space by adding the
      # appropriate amount of empty rows to the data.table
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
                                                      data.table()))
      }
      # setnames(population[[id]]$labels, "labels", "label")
    }
  }
  return(population)
}

create_interactions_log <- function(nrOfInteractions) {
  # This function creates a log for every interaction.
  # Function call in interactions.R, perform_interactions().
  # 
  # Args:
  #    - nrOfInteractions: numeric; the number of interactions to be performed
  #
  # Returns:
  #    - interactionsLog: a data.table
  #
  
  interactionsLog <- data.table(word = NA_character_, producerID = NA_integer_, producerLabel = NA_character_,
                                producerNrOfTimesHeard = NA_integer_, perceiverID = NA_integer_, 
                                perceiverLabel = NA_character_, perceiverNrOfTimesHeard = NA_integer_,
                                accepted = NA, simulationNr = NA_integer_, valid = NA)[0]

  interactionsLog <- rbindlist(list(
    interactionsLog, data.table(matrix(nrow = nrOfInteractions, ncol = ncol(interactionsLog)))
    )) %>% 
    .[, valid := FALSE] %>%
    .[]
}

perform_interactions <- function(pop, logDir, params) {
  # This function repeats perform_single_interaction() (see below)
  # for as many as nrOfInteractions, generates the interaction log,
  # and saves the current population snapshot.
  # Function call in loadLibraries.R, coreABM().
  #
  # Args:
  #    - pop: population list
  #    - logDir: path to logDir
  #    - params: list of params from params.R
  #
  # Returns:
  #    - interactionsLog: a data.table
  #
  
  # generate interaction log
  interactionsLog <- create_interactions_log(params[['interactionsPerSnapshot']] * params[['nrOfSnapshots']])
  
  # agentIDs and matching groups, ordered by agentID
  groupsInfo <- rbindlist(lapply(pop, function(agent) {data.table(agentID = agent$agentID, group = agent$group)}))[order(agentID),]
  
  # perform the interactions
  for (nrSim in 1:params[['nrOfSnapshots']]) {
    for (i in 1:params[['interactionsPerSnapshot']]) {
      perform_single_interaction(pop, interactionsLog, nrSim, groupsInfo, params)
    }
    save_population(pop, extraCols = list(condition = nrSim), logDir = logDir)
  }
  return(interactionsLog)
}

perform_single_interaction <- function(pop, interactionsLog, nrSim, groupsInfo, params) {
  # This function performs a single interaction. 
  # Function call in interactions.R, perform_interactions().
  #
  # Args:
  #    - pop: population list
  #    - interactionsLog: a data.table
  #    - groupsInfo: a data.table containing the agents' IDs and groups, ordered by ID
  #    - params: list of params from params.R
  #
  # Returns:
  #    - nothing.
  #
  
  prodNr <- 1
  percNr <- 1
  # producer and perceiver need to be different agents
  while (prodNr == percNr) {
    # choose interaction partners without taking their group into account
    if (params[['interactionPartners']] == "random") {
      prodNr <- sample(groupsInfo$agentID, 1, prob = params[['speakerProb']])
      percNr <- sample(groupsInfo$agentID, 1, prob = params[['listenerProb']])
      
      # or choose interaction partners from the same group
    } else if (params[['interactionPartners']] == "withinGroups") {
      randomGroup <- sample(unique(groupsInfo$group), 1)
      prodNr <- sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1, 
                       prob = params[['speakerProb']][groupsInfo$group == randomGroup])
      percNr <- sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1, 
                       prob = params[['listenerProb']][groupsInfo$group == randomGroup])
      
      # or choose interaction partners from different groups
    } else if (params[['interactionPartners']] == "betweenGroups") {
      randomGroups <- sample(unique(groupsInfo$group), 2)
      randomPercGroup <- randomGroups[1]
      randomProdGroup <- randomGroups[2]
      prodNr <- sample(groupsInfo$agentID[groupsInfo$group == randomPercGroup], 1, 
                       prob = params[['speakerProb']][groupsInfo$group == randomPercGroup])
      percNr <- sample(groupsInfo$agentID[groupsInfo$group == randomProdGroup], 1, 
                       prob = params[['listenerProb']][groupsInfo$group == randomProdGroup])
      
      # or let agents talk to themselves (developer option)
    } else if (params[['interactionPartners']] == "selfTalk") {
      prodNr <- sample(groupsInfo$agentID, 1, prob = params[['speakerProb']])
      percNr <- 0 # temp hack
    }
  }
  
  if (params[['interactionPartners']] == "selfTalk") { 
    percNr <- prodNr # temp hack
  }
  
  # set producer and perceiver to be the chosen agents from pop
  producer <- pop[[prodNr]]
  perceiver <- pop[[percNr]]
  
  # let speaking agent produce a token and listening agent perceive it
  pt <- produce_token(producer, params)
  perceive_token(perceiver, pt, interactionsLog, nrSim, params)
}

choose_word <- function(labels, method = "random_index") {
  # This function samples a word label from the available labels.
  # Function call in interactions.R, produce_token().
  #
  # Args:
  #    - labels: data.table from an agent's memory
  #    - method: string. Default: random_index.
  #
  # Returns:
  #    - the chosen word label as a string
  #
  
  if (nrow(labels) == 0) {
    stop("choose_word: Empty labels table (empty agent memory")
  } 
  
  # currently, there is no other method than random_index;
  # however, it is planned to implement lexical frequencies here.
  if (method == "random_index" | is.null(method)) {
    labels$word[sample(which(labels$valid == TRUE), 1)]
  } else {
    stop(paste("choose_word: Unknown method", method))
  }
}

produce_token <- function(agent, params) {
  # This function simulates the production of a token as realisation 
  # of a randomly selected word. The token is generated by first 
  # estimating a Gaussian distribution for the given word, then 
  # sampling that distribution.
  #
  # Function call in interactions.R, perform_single_interaction().
  #
  # Args:
  #    - agent: one of the agents from population
  #    - params: list of params from params.R
  #
  # Returns:
  #    - producedToken: a list
  #
  
  # randomly sample a word
  producedWord <- choose_word(agent$labels)
  producedLabel <- agent$labels$label[agent$labels$word == producedWord][1]
  producedInitial <- agent$initial$initial[agent$initial$word == producedWord]
  nrOfTimesHeard <- agent$labels$nrOfTimesHeard[agent$labels$word == producedWord][1] 
  
  nWordTokens <- sum(agent$labels$word == producedWord, na.rm = TRUE)
  nExtraTokens <- 0
  
  # meanWords: get acoustic values for word tokens and an averaged value per word of the same phoneme category
  if (params[['productionStrategy']] == "meanWords") {
    otherWords <- unique(agent$labels$word[agent$labels$label == producedLabel & agent$labels$word != producedWord & 
                                             agent$labels$valid == TRUE])
    nExtraTokens <- length(otherWords)
    if (nExtraTokens == 0) {
      stop("The current speaker, ", agent$labels$speaker, " knows only one word, so that no further mean tokens can be used
           with production strategy meanWords. Either exclude the speaker or change production strategy.")
    }
    # print(paste(agent$speaker, producedLabel, producedWord, "nExtraTokens", nExtraTokens))
    
    # extraTokens: CURRENTLY UNAVAILABLE as productionExtraTokensRatio is missing in params.R
  } else if (params[['productionStrategy']] == "extraTokens") {
    nExtraTokens <- round(params[['productionExtraTokensRatio']] * nWordTokens)
    
    # SMOTE: Synthetic Minority Over-sampling Technique
  } else if (params[['productionStrategy']] == "SMOTE" & nWordTokens < params[['productionMinTokens']]) {
    nExtraTokens <- ceiling((params[['productionMinTokens']] - nWordTokens) / max(nWordTokens, params[['productionSMOTENN']] + 1)) * 
      max(nWordTokens, params[['productionSMOTENN']] + 1)
  }
  
  # allocate space for wordFeatures and fill with already available acoustic values
  wordFeatures <- matrix(nrow = nWordTokens + nExtraTokens, ncol = ncol(as.matrix(agent$features)))
  wordFeatures[1:nWordTokens, ] <- as.matrix(agent$features)[agent$labels$word == producedWord & agent$labels$valid == TRUE, , drop = FALSE]
  
  # complete wordFeatures depending on productionStrategy
  if (params[['productionStrategy']] == "meanWords") {
    for (i in 1:nExtraTokens) {
      wordFeatures[nWordTokens + i, ] <- apply(as.matrix(agent$features)[agent$labels$word == otherWords[i] & 
                                                                           agent$labels$valid == TRUE, , drop = FALSE], 2, mean)
    }
  } else if (params[['productionStrategy']] == "extraTokens" & nExtraTokens > 0) {
    wordFeatures[(nWordTokens + 1):(nWordTokens + nExtraTokens), ] <-  as.matrix(agent$features)[
      sample(which(agent$labels$word != producedWord &
                     agent$labels$label == producedLabel &
                     agent$labels$valid == TRUE), nExtraTokens, replace = TRUE), , drop = FALSE]
  } else if (params[['productionStrategy']] == "SMOTE" & nExtraTokens > 0) {
    wordIndicesWithinLabel <- which(agent$labels$word[agent$labels$label == producedLabel & agent$labels$valid == TRUE] == producedWord)
    # wordIndicesWithinLabel <- agent$labels[label == producedLabel,][word == producedWord, which = TRUE]
    fallbackIndices <- knearest_fallback(as.matrix(agent$features)[agent$labels$label == producedLabel & 
                                                                     agent$labels$valid == TRUE, , drop = FALSE],
                                         wordIndicesWithinLabel,
                                         params[['productionSMOTENN']])
    wordFeatures[(nWordTokens + 1):(nWordTokens + nExtraTokens), ] <- SMOTE(agent$features[agent$labels$label == producedLabel & 
                                                                                             agent$labels$valid == TRUE, ][fallbackIndices, ],
                                                                            rep("a", length(fallbackIndices)),
                                                                            params[['productionSMOTENN']],
                                                                            nExtraTokens/length(fallbackIndices))$syn_data[,-(ncol(agent$features) + 1)] %>% 
      as.matrix
  }
  
  # estimate Gaussian distributions from wordFeatures
  tokenGauss <- list(
    mean = apply(wordFeatures, 2, mean),
    cov = cov(wordFeatures))
  
  # trick to ensure that covariance of tokenGauss is positive
  epsilon_diag <- 1e-6
  while (!is.positive.definite(tokenGauss$cov)) {
    tokenGauss$cov <- tokenGauss$cov + epsilon_diag * diag(nrow(tokenGauss$cov))
    epsilon_diag <- 2 * epsilon_diag
  }
  
  # generate producedToken as a list
  producedToken <- list(
    features = rmvnorm(1, tokenGauss$mean, tokenGauss$cov),
    labels = data.table(word = producedWord,
                        label = producedLabel,
                        initial = producedInitial,
                        nrOfTimesHeard = nrOfTimesHeard,
                        producerID = agent$agentID)
  )
  return(producedToken)
}

perceive_token <- function(perceiver, producedToken, interactionsLog, nrSim, params) {
  # This function tests whether the produced token is to be memorized by the listening agent.
  # Function call in interactions.R, perform_single_interaction().
  #
  # Args:
  #    - perceiver: an agent from the population
  #    - producedToken: list, result of produce_token()
  #    - interactionsLog: data.table
  #    - nrSim: simulation number
  #    - params: list of params from params.R
  #
  # Returns:
  #    - nothing. Overwrites one row in the main data.table.
  #
  
  # find out which phonological label the perceiver associates with the produced word
  perceiverLabel_ <- unique(perceiver$labels$label[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE])
  
  # if word is unknown, abort communication
  if (length(perceiverLabel_) == 0) {
    return()
  }
  
  # find out whether the token is recognized or not
  # ... either by maximum posterior probability decision
  if (params[['memoryIntakeStrategy']] %in% c("maxPosteriorProb", "posteriorProbThr")) {
    # execute the QDA (Quadrativ Discriminant Analysis)
    if (!{cacheRow <- which(perceiver$cache$name == "qda"); perceiver$cache$valid[cacheRow]}) {
      perceiver$cache[cacheRow,  `:=`(value1 = list(qda(as.matrix(perceiver$features)[perceiver$labels$valid == TRUE, , drop = FALSE],
                             grouping = perceiver$labels$label[perceiver$labels$valid == TRUE])),
                             valid = TRUE)]
    } 
    
    # compute posterior probabilities
    posteriorProbAll <- predict(perceiver$cache$value1[cacheRow][[1]], producedToken$features)$posterior
    
    # decide if token is recognized
    # ... either based on maximum posterior probabilities
    if (params[['memoryIntakeStrategy']] == "maxPosteriorProb") {
      recognized <- colnames(posteriorProbAll)[which.max(posteriorProbAll)] == perceiverLabel_
      # ... or based on posterior probability threshold (parameter is missing from params.R!)
    } else if (params[['memoryIntakeStrategy']] == "posteriorProbThr") {
      recognized <- posteriorProbAll[, perceiverLabel_] >= params[['posteriorProbThr']]
    }
    
  # ... or based on a Mahalanobis distance measurement
  } else if (params[['memoryIntakeStrategy']] == "mahalanobisDistance") {
    mahalaDistanceLabel <- mahalanobis(producedToken$features,
                                           apply(as.matrix(perceiver$features)[perceiver$labels$valid == TRUE & 
                                                                                 perceiver$labels$label == perceiverLabel_, , drop = FALSE], 2, mean),
                                           cov(as.matrix(perceiver$features)[perceiver$labels$valid == TRUE & 
                                                                               perceiver$labels$label == perceiverLabel_, , drop = FALSE]))
    recognized <- mahalaDistanceLabel <= params[['mahalanobisThreshold']]
  } else if (params[['memoryIntakeStrategy']] == "acceptAll") {
    recognized <- TRUE
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
        tdat.mahal <- train(as.matrix(perceiver$features)[perceiver$labels$label == perceiverLabel_, , drop = FALSE])
        rowToWrite <- which(perceiver$labels$word == producedToken$labels$word)[
          which.max(distance(as.matrix(perceiver$features)[perceiver$labels$word == producedToken$labels$word, , drop = FALSE], tdat.mahal, metric = "mahal"))
          ]
      } else if (params[['memoryRemovalStrategy']] == "random") {
        rowToWrite <- sample(which(perceiver$labels$word == producedToken$labels$word), 1)
      }
      
      # if there is still some capacity, use an empty row of the memory
    } else {
      rowToWrite <- which(perceiver$labels$valid == FALSE)[1]
    }
    
    # write in agent's memory
    updatedNrOfTimesHeard <- 1 + max(0, perceiver$labels$nrOfTimesHeard[perceiver$labels$word == producedToken$labels$word & 
                                                                          perceiver$labels$valid == TRUE][1], na.rm = TRUE)
    receivedTimeStamp <- 1 + max(0, perceiver$labels$timeStamp[perceiver$labels$word == producedToken$labels$word], na.rm = TRUE)
    perceiver$features[rowToWrite, names(perceiver$features) := as.list(producedToken$features)]
    perceiver$labels[rowToWrite, `:=`(
      word = producedToken$labels$word,
      label = perceiverLabel_,
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
    splitandmerge(perceiver, params, full = FALSE)
    if (params[['memoryIntakeStrategy']] == "maxPosteriorProb") {
      perceiver$cache[cacheRow, valid := FALSE]
    }
  }
}

  