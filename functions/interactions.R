################################################################################
#                                                                              #
# This script contains the functions that perform the interactions.            #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
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
    nrOfAgents <- params[["bootstrapPopulationSize"]]
  } else {
    stop(paste("create_population: unrecognised createPopulationMethod:", method))
  }
  
  # memory resampling will generate extra artificial tokens (exemplars).
  # Each agent will grow its initial exemplars endowment by a factor of "initialMemoryResamplingFactor"
  # The max number of initial exemplars is taken to compute the final number of tokens, 
  # the same for each agent.
  initialMemorySize <- input.df[, .N, by = speaker][, max(N)]
  if (params[["initialMemoryResampling"]]) {
    initialMemorySize <- initialMemorySize * params[["initialMemoryResamplingFactor"]]
  }
  
  # memoryBuffer defines a buffer of empty memory space
  # so that it is unlikely that an agent will exceed its memory limit during the simulation.
  # This is to decouple the forgetting rate from the memory capacity.
  # maxMemorySize is approx = mean + 10 st. dev. of the expected number of received tokens during the simulation.
  # Num. received tokens is Bin(nrOfInteractions, 1/nrOfAgents) approx = Normal,
  # ignoring the (1-1/nrOfAgents) factor in var and taking worst case of zero forgetting rate.
  memoryBuffer <- ceiling(params[["nrOfInteractions"]]/nrOfAgents + 10 * sqrt(params[["nrOfInteractions"]]/nrOfAgents))
  if(params[["rememberOwnTokens"]]) {
    memoryBuffer <- memoryBuffer * 2
  }
  
  maxMemorySize <- initialMemorySize + memoryBuffer  
  
  # initiate a list called population and search for P-columns in input.df
  population <- list()
  
  Pcols <- grep("^P[[:digit:]]+$", colnames(input.df), value = TRUE)
  if (length(Pcols) == 0) Pcols <- NULL
  
  for (id in seq_len(nrOfAgents)) {
    if (method == "speaker_is_agent") {
      selectedSpeaker <- sortedSpeakers[id]
    } else if (method == "bootstrap") {
      selectedSpeaker <- sample(sortedSpeakers, 1)
    }
    population[[id]] <- create_agent(id, input.df, selectedSpeaker, maxMemorySize, Pcols, exemplarsCol = NULL, params)
    if (params[["initialMemoryResampling"]]) {
      apply_resampling(population[[id]], initialMemorySize, params)
    }
  }
  return(population)
}

create_agent <- function(id, input.df, selectedSpeaker, maxMemorySize, featuresCols = NULL, exemplarsCol = NULL, params) {
  agent <- list()
  # metadata
  agent$agentID <- id
  agent$group <- input.df[speaker == selectedSpeaker, group][1]
  agent$speaker <- input.df[speaker == selectedSpeaker, speaker][1]
  agent$initial <- input.df[speaker == selectedSpeaker, .(word, initial)] %>% unique
  agent$cache <- data.table(name = "qda", value = list(), valid = FALSE)
  # init empty memory of size maxMemorySize
  agent$labels <- data.table(word = character(),
                             label = character(),
                             valid = logical(),
                             nrOfTimesHeard = integer(),
                             producerID = integer(),
                             timeStamp = integer()) %>%
    .[1:maxMemorySize] %>%
    .[, valid := FALSE]
  if (!is.null(featuresCols)) {
    agent$features <- matrix(double(), nrow = maxMemorySize, ncol = length(featuresCols)) %>%
      data.table %>%
      setnames(featuresCols)
  }
  if (!is.null(exemplarsCol)) {
    agent$exemplars <- data.table(exemplars = rep(list(list(FALSE)), maxMemorySize))
  }
  
  # fill memory with content from input.df
  nInput <- input.df[speaker == selectedSpeaker, .N]
  nInputFromGroup <- ceiling(nInput * params[["proportionGroupTokens"]])
  nInputFromOwn <- nInput - nInputFromGroup
  
  groupData <- input.df[group == agent$group & speaker != selectedSpeaker,]
  ownData <- input.df[speaker == selectedSpeaker,]
  samples <- rbind(groupData[sample(nrow(groupData), nInputFromGroup),], ownData[sample(nrow(ownData), nInputFromOwn),]) %>% setDT()
  
  agent$labels %>% 
    .[1:nInput, c("word", "label") := samples[, .(word, label)]] %>%
    .[1:nInput, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)] %>%
    .[1:nInput, timeStamp := sample(.N), by = word]
  
  if (!is.null(featuresCols)) {
    agent$features %>%
      .[1:nInput, (featuresCols) := samples[, .SD, .SDcols = featuresCols]]
  }
  if (!is.null(exemplarsCol)) {
    agent$exemplars %>%
      .[1:nInput, exemplars := samples[, exemplarsCol]]
  }
  
  return(agent)
}

apply_resampling <- function(agent, finalN, params) {
  initialN <- agent$labels[valid == TRUE, .N]
  if (initialN >= finalN)
    return()
  extraN <- min(finalN, nrow(agent$labels)) - initialN
  # a list of produced tokens, all based on the initial memory
  tokens <- replicate(extraN, produce_token(agent, params), simplify = FALSE)
  lapply(seq_along(tokens), function(i) {
    rowToWrite <- row_to_write(agent, tokens[[i]], params)
    update_memory(agent, tokens[[i]], rowToWrite, tokens[[i]]$labels$label) 
  })
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

  rbindlist(list(
    interactionsLog, data.table(matrix(nrow = nrOfInteractions, ncol = ncol(interactionsLog)))
    ), use.names = FALSE) %>% 
    .[, valid := FALSE] %>%
    .[]
}

write_to_log <- function(interactionsLog, producedToken, perceiver, perceiverLabel_, memorise, nrSim) {
  # This function updates the interactionLog.
  # Function call in interactions.R, perceive_token().
  #
  # Args:
  #    - interactionsLog: a data.table that contains information on the interactions
  #    - producedToken: list, result of produce_token()
  #    - perceiver: list, an agent from the population 
  #    - perceiverLabel_: string, label that the perceiver associates with producedToken
  #    - memorise: boolean 
  #    - nrSim: simulation number
  #
  # Returns:
  #    - Nothing. Just updates the log.
  #
  
  rowToWrite <- which(interactionsLog$valid == FALSE)[1]
  interactionsLog[rowToWrite, `:=`(
    word = producedToken$labels$word,
    producerID = producedToken$labels$producerID,
    producerLabel = producedToken$labels$label,
    producerNrOfTimesHeard = producedToken$labels$nrOfTimesHeard,
    perceiverID = perceiver$agentID,
    perceiverLabel = perceiverLabel_,
    perceiverNrOfTimesHeard = {
      if (memorise) {
        perceiver$labels$nrOfTimesHeard[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE][1]
      } else {
        as.integer(max(1, perceiver$labels$nrOfTimesHeard[perceiver$labels$word == producedToken$labels$word & perceiver$labels$valid == TRUE][1]))
      }
    },
    accepted = memorise,
    simulationNr = nrSim,
    valid = TRUE
  )]
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
  interactionsLog <- create_interactions_log(params[["nrOfInteractions"]])
  
  # agentIDs and matching groups, ordered by agentID
  groupsInfo <- rbindlist(lapply(pop, function(agent) {data.table(agentID = agent$agentID, group = agent$group)}))[order(agentID),]
  
  # perform the interactions
  for (nrSim in 1:params[["nrOfSnapshots"]]) {
    for (i in 1:params[["interactionsPerSnapshot"]]) {
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
    if (is.null(params[["interactionPartners"]]) || params[["interactionPartners"]] == "random") {
      prodNr <- sample(groupsInfo$agentID, 1, prob = params[["speakerProb"]])
      percNr <- sample(groupsInfo$agentID, 1, prob = params[["listenerProb"]])
      
      # or choose interaction partners from the same group
    } else if (params[["interactionPartners"]] == "withinGroups") {
      randomGroup <- sample(unique(groupsInfo$group), 1)
      prodNr <- sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1, 
                       prob = params[["speakerProb"]][groupsInfo$group == randomGroup])
      percNr <- sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1, 
                       prob = params[["listenerProb"]][groupsInfo$group == randomGroup])
      
      # or choose interaction partners from different groups
    } else if (params[["interactionPartners"]] == "betweenGroups") {
      randomGroups <- sample(unique(groupsInfo$group), 2)
      randomPercGroup <- randomGroups[1]
      randomProdGroup <- randomGroups[2]
      prodNr <- sample(groupsInfo$agentID[groupsInfo$group == randomPercGroup], 1, 
                       prob = params[["speakerProb"]][groupsInfo$group == randomPercGroup])
      percNr <- sample(groupsInfo$agentID[groupsInfo$group == randomProdGroup], 1, 
                       prob = params[["listenerProb"]][groupsInfo$group == randomProdGroup])
      
      # or let agents talk to themselves (developer option)
    } else if (params[["interactionPartners"]] == "selfTalk") {
      prodNr <- sample(groupsInfo$agentID, 1, prob = params[["speakerProb"]])
      percNr <- 0 # temp hack
    }
  }
  
  if (params[["interactionPartners"]] == "selfTalk") { 
    percNr <- prodNr # temp hack
  }
  
  # set producer and perceiver to be the chosen agents from pop
  producer <- pop[[prodNr]]
  perceiver <- pop[[percNr]]
  
  # let speaking agent produce a token and listening agent perceive it
  pt <- produce_token(producer, params)
  
  perceive_token(perceiver, pt, interactionsLog, nrSim, params, isNotOwnToken = TRUE)
  if(params[["rememberOwnTokens"]]) {
    perceive_token(producer, pt, interactionsLog, nrSim, params, isNotOwnToken = FALSE)
  }
}

choose_word <- function(labels, method = "random_word") {
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
  
  if (!all(c("word", "valid") %in% names(labels))) {
    stop("choose_word: labels data.table should have a 'word' and a 'valid' column")
  }
  
  if (sum(labels$valid == TRUE) == 0) {
    # stop("choose_word: Empty labels table (empty agent memory")
    # print to LOG 
    return (NULL)
  } 
  
  # currently, there is no other method than random_word;
  # however, other methods may be implemented, e.g. lexical frequency-based.
  if (method == "random_word" | is.null(method)) {
    # labels$word[sample(which(labels$valid == TRUE), 1)]
    labels$word[labels$valid == TRUE] %>% unique %>% sample(1)
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
  if (is.null(producedWord)) {
    # print to LOG 
    return (NULL)
  }
  
  producedLabel <- agent$labels$label[agent$labels$word == producedWord & agent$labels$valid == TRUE][1]
  producedInitial <- agent$initial$initial[agent$initial$word == producedWord]
  nrOfTimesHeard <- agent$labels$nrOfTimesHeard[agent$labels$word == producedWord & agent$labels$valid == TRUE][1]
  
  if (grepl("^(target)?[wW]ord$", params[["productionBasis"]])) {
    basisIdx <- which(agent$labels$word == producedWord & agent$labels$valid == TRUE)
  } else if (grepl("^(target)?([lL]abel|[pP]honeme)$", params[["productionBasis"]])) {
    basisIdx <- which(agent$labels$label == producedLabel & agent$labels$valid == TRUE)
  }
  basisTokens <- as.matrix(agent$features)[basisIdx, , drop = FALSE]
  
  if (!is.null(params[["productionResampling"]])) {
    if (grepl("SMOTE", params[["productionResampling"]], ignore.case = TRUE)) {
      nExtraTokens <- params[["productionMinTokens"]] - length(basisIdx)
      if (nExtraTokens > 0) {
        extendedIdx <- NULL
        if (grepl("label|phoneme", params[["productionResamplingFallback"]], ignore.case = TRUE)) {
          extendedIdx <- which(agent$labels$label == producedLabel & agent$labels$valid == TRUE)
        }
        extraTokens <- smote_resampling(agent$features, extendedIdx, basisIdx, params[["productionSMOTENN"]], nExtraTokens)
        basisTokens <- rbind(basisTokens, extraTokens)
      }
    } else {
      stop(paste("produce_token: unrecognised productionResampling method:", params[["productionResampling"]]))
    }
  }
  tokenGauss <- estimate_gaussian(basisTokens)
  
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

estimate_gaussian <- function(features, epsilon_diag = 1e-6) {
  # estimate a Gaussian from data, ensure that it has positive covariance.
  # features: a matrix, rows are data points, columns are dimensions
  # epsilon_diag: starting value for 'water filling' of cov diagonal in case of non positive definiteness
  # returns a list of mean and cov.
  
  tokenGauss <- list(
    mean = apply(features, 2, mean),
    cov = cov(features))
  
  epsilon_diag <- 1e-6
  while (!is.positive.definite(tokenGauss$cov)) {
    tokenGauss$cov <- tokenGauss$cov + epsilon_diag * diag(nrow(tokenGauss$cov))
    epsilon_diag <- 2 * epsilon_diag
  }
  return(tokenGauss)
}

smote_one_class <- function(features, K, N) {
  # A wrapper to smotefamily::SMOTE()
  # All elements (rows) of 'features' belong to the same (minority) class "a".
  # Produce N extra tokens using the SMOTE algorithm, using K nearest neighbors during sampling.
  
  if (K <= 0) {
    stop(paste("smote_one_class: invalid nearest neighbour parameter K =", K))
  }
  if (N <= 0) {
    stop(paste("smote_one_class: invalid number of samples requested N =", N))
  }
  
  if (!is.data.frame(features)) {
    features <- data.frame(features)
  }
  
  # Fringe cases
  if (nrow(features) == 0) {
    # print to LOG 
    return (NULL)
  }
  if (nrow(features) == 1) {
    # print to LOG 
    # return N copies of the only token
    return(matrix(rep(as.numeric(features), N), ncol = ncol(features), byrow = TRUE))
  }
  K <- min(K, nrow(features) - 1)
  
  SMOTE(data.frame(features), rep("a", nrow(features)), K, ceiling(N/nrow(features))) %>%
    .$syn_data %>%
    .[sample(nrow(.), N), -(ncol(features) + 1)] %>% 
    as.matrix
}

smote_resampling <- function(points, extendedIndices = NULL, targetIndices, K, N) {
  fallbackIndices <- knearest_fallback(points, extendedIndices, targetIndices, K)
  smote_one_class(points[fallbackIndices, , drop = FALSE], K, N)
}

row_to_overwrite <- function(perceiver, producedToken, params) {
  # This function is applied when the agent-listener's memory capacity is 
  # full and an old token needs to be overwritten by a new one.
  # Function call in interactions.R, row_to_write().
  #
  # Args:
  #    - perceiver: an agent from the population
  #    - producedToken: list, result of produce_token()
  #    - params: list of params
  #
  # Returns:
  #    - rowToOverwrite: index of row that is to be overwritten
  #
  
  # remove either the oldest token
  if (params[["memoryRemovalStrategy"]] == "timeDecay") {
    rowToOverwrite <- which(perceiver$labels$word == producedToken$labels$word)[
      which.min(perceiver$labels$timeStamp[perceiver$labels$word == producedToken$labels$word])
      ]
    # ... or the farthest outlier of the token distribution
  } else if (params[["memoryRemovalStrategy"]] == "outlierRemoval") {
    tdat.mahal <- train(as.matrix(perceiver$features)[perceiver$labels$label == perceiverLabel_, , drop = FALSE])
    rowToOverwrite <- which(perceiver$labels$word == producedToken$labels$word)[
      which.max(distance(as.matrix(perceiver$features)[perceiver$labels$word == producedToken$labels$word, , drop = FALSE], tdat.mahal, metric = "mahal"))
      ]
    # ... or random token (recommended)
  } else if (params[["memoryRemovalStrategy"]] == "random") {
    rowToOverwrite <- sample(which(perceiver$labels$word == producedToken$labels$word), 1)
  }
  return(rowToOverwrite)
}


row_to_write <- function(agent, producedToken, params) {
  # This function finds the row that the newly produced token will be stored in.
  # Function call in interactions.R, perceive_token().
  #
  # Args:
  #    - agent: an agent from the population
  #    - producedToken: list, result of produce_token()
  #    - params: list of params
  #
  # Returns:
  #    - rowToWrite: index of row that is to be used for the new token
  #
  
  if (all(agent$labels$valid)) {
    print(paste("agent", agent$agentID, "full"))
    rowToWrite <- row_to_overwrite(agent, producedToken, params)
  } else {
    rowToWrite <- which(agent$labels$valid == FALSE)[1]
  }
  return(rowToWrite)
}

update_memory <- function(agent, producedToken, rowToWrite, label_) {
  # This function updates an agent's memory with a new token.
  # Function call in interactions.R, perceive_token().
  #
  # Args:
  #    - agent: an agent from the population
  #    - producedToken: list, result of produce_token()
  #    - rowToWrite: result of row_to_write()
  #    - label_: string, label that the agent associates with producedToken
  #
  # Returns:
  #    - agent: an agent from the population with updated memory
  #
  
  updatedNrOfTimesHeard <- 1 + max(0, agent$labels$nrOfTimesHeard[
    agent$labels$word == producedToken$labels$word & agent$labels$valid == TRUE
    ][1], na.rm = TRUE)
  receivedTimeStamp <- 1 + max(0, agent$labels$timeStamp[agent$labels$word == producedToken$labels$word], na.rm = TRUE)
  agent$features[rowToWrite, names(agent$features) := as.list(producedToken$features)]
  agent$labels[rowToWrite, `:=`(
    word = producedToken$labels$word,
    label = label_,
    valid = TRUE,
    producerID = producedToken$labels$producerID,
    timeStamp = receivedTimeStamp
  )]
  agent$labels[agent$labels$word == producedToken$labels$word & agent$labels$valid == TRUE, 
                   nrOfTimesHeard := updatedNrOfTimesHeard]
}


perceive_token <- function(agent, producedToken, interactionsLog, nrSim, params, isNotOwnToken) {
  # This function tests whether the produced token is to be memorized by the listening agent.
  # Function call in interactions.R, perform_single_interaction().
  #
  # Args:
  #    - agent: an agent from the population
  #    - producedToken: list, result of produce_token()
  #    - interactionsLog: data.table
  #    - nrSim: simulation number
  #    - params: list of params from params.R
  #    - isNotOwnToken: boolean, whether or not the token to be perceived is 
  #      the listener's own token
  #
  # Returns:
  #    - nothing. Overwrites one row in the main data.table.
  #
  
  if (is.null(producedToken)) {
    # print to LOG 
    return()
  }
  
  # find out which phonological label the agent associates with the produced word
  perceiverLabel_ <- unique(agent$labels$label[agent$labels$word == producedToken$labels$word & agent$labels$valid == TRUE])
  
  # if word is unknown, assign label based on majority vote among perceptionNN nearest neighbours
  if (length(perceiverLabel_) == 0) {
    perceiverLabel_ <- names(which.max(table(agent$labels$label[agent$labels$valid == TRUE][
      knnx.index(agent$features[agent$labels$valid == TRUE,], producedToken$features, params[["perceptionNN"]])
      ])))
  }
  
  memorise <- TRUE
  # relative acceptance criterion
  if (memorise && any(c("maxPosteriorProb", "posteriorProbThr") %in% params[["memoryIntakeStrategy"]])) { 
    posteriorProb <- compute_posterior_probabilities(agent, producedToken, params[["posteriorProbMethod"]])
    if ("maxPosteriorProb" %in% params[["memoryIntakeStrategy"]]) {
      memorise %<>% `&`(recognize_posterior_probabilities(posteriorProb, perceiverLabel_, "maxPosteriorProb"))
    } else if ("posteriorProbThr" %in% params[["memoryIntakeStrategy"]]) {
      memorise %<>% `&`(recognize_posterior_probabilities(posteriorProb, perceiverLabel_, "posteriorProbThr", posteriorProbThr = params[["posteriorProbThr"]]))
    }
  }
  # absolute acceptance criterion
  if (memorise && any(c("mahalanobisDistance", "highestDensityRegion") %in% params[["memoryIntakeStrategy"]])) {
    mahalDist <- compute_mahal_distance(agent, producedToken, perceiverLabel_)
    if ("mahalanobisDistance" %in% params[["memoryIntakeStrategy"]]) {
      memorise %<>% `&`(mahalDist <= params[["mahalanobisThreshold"]])
    } else if ("highestDensityRegion" %in% params[["memoryIntakeStrategy"]]) {
      memorise %<>% `&`(runif(1) < pchisq(q = mahalDist, df = ncol(agent$features), lower.tail = FALSE))
    }
  }
  
  # ... or just accept everything
  if ("acceptAll" %in% params[["memoryIntakeStrategy"]]) {
    memorise <- TRUE
  }
  
  # forget
  if (runif(1) < params[["forgettingRate"]]) {
    set(agent$labels, sample(which(agent$labels$valid == TRUE), 1), "valid", FALSE)
  }
  
  if (memorise) {
    # find next free row or row to be overwritten
    rowToWrite <- row_to_write(agent, producedToken, params)
    
    # write in agent's memory
    update_memory(agent, producedToken, rowToWrite, perceiverLabel_)
    
    # empty cache
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }
  
  # write on interactionsLog
  if (isNotOwnToken) {
    write_to_log(interactionsLog, producedToken, agent, perceiverLabel_, memorise, nrSim)
  }
  
  # apply split&merge if needed
  numReceivedTokens <- sum(interactionsLog$valid[interactionsLog$perceiverID == agent$agentID], na.rm = TRUE)
  if (params[["splitAndMerge"]] == T & numReceivedTokens %% params[["splitAndMergeInterval"]] == 0) {
    splitandmerge(agent, params, full = FALSE)
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }
}

  