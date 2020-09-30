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

create_population <- function(input.df, params) {
  # This function creates the agent population.
  # Function call in loadLibraries.R, coreABM().
  # 
  # Args:
  #    - input.df: the input data.frame
  #    - params: list of params from params.R
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
    if (length(params[["bootstrapPopulationSize"]]) == 1) {
      nrOfAgents <- params[["bootstrapPopulationSize"]]
    } else {
      nrOfAgents <- sum(params[["bootstrapPopulationSize"]])
      agentGroups <- cut(seq_len(nrOfAgents),
                         breaks = c(1,cumsum(params[["bootstrapPopulationSize"]])),
                         labels = names(params[["bootstrapPopulationSize"]]),
                         include.lowest = TRUE)
      speakerGroups <- input.df[, speaker, by = group] %>% unique
    }
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
  
  population <- list()
  
  for (id in seq_len(nrOfAgents)) {
    if (method == "speaker_is_agent") {
      selectedSpeaker <- sortedSpeakers[id]
    } else if (method == "bootstrap") {
      if (length(params[["bootstrapPopulationSize"]]) == 1) {
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

create_agent <- function(id, input.df, selectedSpeaker, maxMemorySize, params) {
  agent <- list()
  # metadata
  agent$agentID <- id
  agent$group <- input.df[speaker == selectedSpeaker, group][1]
  agent$speaker <- input.df[speaker == selectedSpeaker, speaker][1]
  agent$initial <- input.df[speaker == selectedSpeaker, .(word, initial)] %>% unique
  cacheNames <- c("nFeatures", "qda", "GMM", "nAccepted", methodReg[params[["featureExtractionMethod"]], cacheEntries][[1]] %>% .[!is.na(.)])
  agent$cache <- data.table(name = cacheNames, value = list(), valid = FALSE)
  set_cache_value(agent, "nAccepted", 0)
  # init empty memory of size maxMemorySize
  agent$memory <- data.table(word = character(),
                             label = character(),
                             valid = logical(),
                             nrOfTimesHeard = integer(),
                             producerID = integer(),
                             timeStamp = integer()
                             ) %>%
    .[1:maxMemorySize] %>%
    .[, valid := FALSE] %>%
    .[, exemplar := list(list(FALSE))]
  
  # fill memory with content from input.df
  nInput <- input.df[speaker == selectedSpeaker, .N]
  nInputFromGroup <- ceiling(nInput * params[["proportionGroupTokens"]])
  nInputFromOwn <- nInput - nInputFromGroup
  
  groupData <- input.df[group == agent$group & speaker != selectedSpeaker,]
  ownData <- input.df[speaker == selectedSpeaker,]
  if (nrow(groupData) < nInputFromGroup) {
    stop("Cannot sample ", nInputFromGroup, " tokens from ", nrow(groupData), " tokens of group ", agent$group, ".\n Please decrease proportionGroupTokens in params.R.")
  }
  samples <- rbindlist(list(
    groupData[sample(.N, nInputFromGroup),],
    ownData[sample(.N, nInputFromOwn),]
    ))
  
  agent$memory %>% 
    .[1:nInput, c("word", "label", "exemplar") := samples[, .(word, label, exemplar)]] %>%
    .[1:nInput, `:=`(valid = TRUE, nrOfTimesHeard = 1, producerID = id)] %>%
    .[1:nInput, timeStamp := sample(.N), by = word]
  
  agent$features <- data.table(P1 = double()) %>% .[1:maxMemorySize]
  update_features(agent, compute_features(agent, params))
  
  if (grepl("^GMM(s)?", params[["perceptionModels"]])) {
    estimate_GMM(agent, params)
  }
  return(agent)
}

apply_resampling <- function(agent, finalN, params) {
  initialN <- agent$memory[valid == TRUE, .N]
  if (initialN >= finalN)
    return()
  extraN <- min(finalN, nrow(agent$memory)) - initialN
  # a list of produced tokens, all based on the initial memory
  tokens <- replicate(extraN, produce_token(agent, params), simplify = FALSE)
  lapply(seq_along(tokens), function(i) {
    rowToWrite <- row_to_write(agent, tokens[[i]], params)
    write_memory(agent, tokens[[i]], rowToWrite, tokens[[i]]$label) # tokens[[i]]$memory$label 
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
                                accepted = NA, rejectionCriterion = NA_character_, simulationNr = NA_integer_, valid = NA)[0]

  rbindlist(list(
    interactionsLog, data.table(matrix(nrow = nrOfInteractions, ncol = ncol(interactionsLog)))
    ), use.names = FALSE) %>% 
    .[, valid := FALSE] %>%
    .[]
}

write_interactions_log <- function(interactionsLog, producedToken, perceiver, perceiverLabel_, memorise, strategy,  nrSim) {
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
    word = producedToken$word,
    producerID = producedToken$producerID,
    producerLabel = producedToken$label,
    producerNrOfTimesHeard = producedToken$nrOfTimesHeard,
    perceiverID = perceiver$agentID,
    perceiverLabel = perceiverLabel_,
    perceiverNrOfTimesHeard = {
      if (memorise) {
        perceiver$memory$nrOfTimesHeard[perceiver$memory$word == producedToken$word & perceiver$memory$valid == TRUE][1]
      } else {
        as.integer(max(1, perceiver$memory$nrOfTimesHeard[perceiver$memory$word == producedToken$word & perceiver$memory$valid == TRUE][1]))
      }
    },
    accepted = memorise,
    rejectionCriterion = ifelse(memorise, NA_character_, strategy),
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
  
  # agentIDs and matching groups, ordered by agentID
  groupsInfo <- rbindlist(lapply(pop, function(agent) {data.table(agentID = agent$agentID, group = agent$group)}))[order(agentID),]
  
  # perform the interactions
  for (snap in 1:params[["nrOfSnapshots"]]) {
    interactionsLog <- create_interactions_log(params[["interactionsPerSnapshot"]])
    for (i in 1:params[["interactionsPerSnapshot"]]) {
      perform_single_interaction(pop, interactionsLog, snap, groupsInfo, params)
    }
    save_population(pop, extraCols = list(snapshot = snap), logDir = logDir)
    save_interactions_log(interactionsLog, extraCols = list(snapshot = snap), logDir = logDir)
  }
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
  producedWord <- choose_word(agent$memory)
  if (is.null(producedWord)) {
    # print to LOG 
    return (NULL)
  }
  
  producedLabel <- agent$memory$label[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]
  producedInitial <- agent$initial$initial[agent$initial$word == producedWord]
  # if initial label is unknown to agent, sample one of the unique initial labels as producedInitial
  if (length(producedInitial) == 0) {
    cat("initial for word", producedWord, "unknown to agent", agent$agentID, agent$speaker, "\n")
    producedInitial <- agent$initial$initial %>% unique %>% sample(1)
  }
  nrOfTimesHeard <- agent$memory$nrOfTimesHeard[agent$memory$word == producedWord & agent$memory$valid == TRUE][1]

  if (grepl("^GMM(s)?", params[["perceptionModels"]])) {
    GMM <- get_cache_value(agent, "GMM")
    # pick a random token of producedWord
    wordIdx <- sample(which(agent$memory$word == producedWord & agent$memory$valid == TRUE), 1)
    features <- as.matrix(agent$features)[wordIdx, , drop = FALSE]
    # identify the closest Gaussian component from GMM of producedLabel
    GIdx <- which.min(compute_mahal_distances_GMM(GMM$models[[producedLabel]], features))
    gaussParams <- get_mean_cov_from_GMM_component(GMM$models[[producedLabel]], GIdx)
    # then extract from that Gaussian
  } else {
    
    if (grepl("^(target)?[wW]ord$", params[["productionBasis"]])) {
      basisIdx <- which(agent$memory$word == producedWord & agent$memory$valid == TRUE)
    } else if (grepl("^(target)?([lL]abel|[pP]honeme)$", params[["productionBasis"]])) {
      basisIdx <- which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
    }
    basisTokens <- as.matrix(agent$features)[basisIdx, , drop = FALSE]
    
    if (!is.null(params[["productionResampling"]])) {
      if (grepl("SMOTE", params[["productionResampling"]], ignore.case = TRUE)) {
        nExtraTokens <- params[["productionMinTokens"]] - length(basisIdx)
        if (nExtraTokens > 0) {
          extendedIdx <- NULL
          if (grepl("label|phoneme", params[["productionResamplingFallback"]], ignore.case = TRUE)) {
            extendedIdx <- which(agent$memory$label == producedLabel & agent$memory$valid == TRUE)
          }
          extraTokens <- smote_resampling(agent$features, extendedIdx, basisIdx, params[["productionSMOTENN"]], nExtraTokens)
          basisTokens <- rbind(basisTokens, extraTokens)
        }
      } else {
        stop(paste("produce_token: unrecognised productionResampling method:", params[["productionResampling"]]))
      }
    }
    gaussParams <- estimate_gaussian(basisTokens)
  }
  # generate producedToken as a list
  features <- rmvnorm(1, gaussParams$mean, gaussParams$cov)
  producedToken <- data.table(word = producedWord,
                              label = producedLabel,
                              initial = producedInitial,
                              exemplar = features2exemplar(features, agent, params),
                              nrOfTimesHeard = nrOfTimesHeard,
                              producerID = agent$agentID)
  return(producedToken)
}

estimate_gaussian <- function(features, epsilon_diag = 1e-6) {
  # estimate a Gaussian from data, ensure that it has positive covariance.
  # features: a matrix, rows are data points, columns are dimensions
  # epsilon_diag: starting value for 'water filling' of cov diagonal in case of non positive definiteness
  # returns a list of mean and cov.
  
  gaussParams <- list(
    mean = apply(features, 2, mean),
    cov = cov(features))
  
  epsilon_diag <- 1e-6
  while (!is.positive.definite(gaussParams$cov)) {
    gaussParams$cov <- gaussParams$cov + epsilon_diag * diag(nrow(gaussParams$cov))
    epsilon_diag <- 2 * epsilon_diag
  }
  gaussParams$invcov <- solve(gaussParams$cov)
  return(gaussParams)
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
  if (ncol(features) == 1) {
    # browser()
  }
  
  smote_res <- SMOTE( # bug in SMOTE
    if (ncol(features) == 1) {
      cbind(as.data.frame(features), XCOL = 0)
    } else {
      as.data.frame(features)
    },
    rep("a", nrow(features)),
    K,
    ceiling(N/nrow(features))
    )
  smote_res %>%
    .$syn_data %>%
    .[sample(nrow(.), N), 1:ncol(features)] %>% 
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
    rowToOverwrite <- which(perceiver$memory$word == producedToken$word)[
      which.min(perceiver$memory$timeStamp[perceiver$memory$word == producedToken$word])
      ]
    # ... or the farthest outlier of the token distribution
  } else if (params[["memoryRemovalStrategy"]] == "outlierRemoval") {
    tdat.mahal <- train(as.matrix(perceiver$features)[perceiver$memory$label == perceiverLabel, , drop = FALSE])
    rowToOverwrite <- which(perceiver$memory$word == producedToken$word)[
      which.max(emuR::distance(as.matrix(perceiver$features)[perceiver$memory$word == producedToken$word, , drop = FALSE], tdat.mahal, metric = "mahal"))
      ]
    # ... or random token (recommended)
  } else if (params[["memoryRemovalStrategy"]] == "random") {
    rowToOverwrite <- sample(which(perceiver$memory$word == producedToken$word), 1)
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
  
  if (all(agent$memory$valid)) {
    print(paste("agent", agent$agentID, "full"))
    rowToWrite <- row_to_overwrite(agent, producedToken, params)
  } else {
    rowToWrite <- which(agent$memory$valid == FALSE)[1]
  }
  return(rowToWrite)
}

write_memory <- function(agent, producedToken, rowToWrite, label_) {
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
  
  updatedNrOfTimesHeard <- 1 + max(0, agent$memory$nrOfTimesHeard[
    agent$memory$word == producedToken$word & agent$memory$valid == TRUE
    ][1], na.rm = TRUE)
  receivedTimeStamp <- 1 + max(0, agent$memory$timeStamp[agent$memory$word == producedToken$word], na.rm = TRUE)
  agent$memory[rowToWrite, `:=`(
    word = producedToken$word,
    exemplar = producedToken$exemplar,
    label = label_,
    valid = TRUE,
    producerID = producedToken$producerID,
    timeStamp = receivedTimeStamp
  )]
  agent$memory[agent$memory$word == producedToken$word & agent$memory$valid == TRUE, 
                   nrOfTimesHeard := updatedNrOfTimesHeard]
  write_features(agent, exemplar2features(producedToken$exemplar, agent, params), rowToWrite)
}

write_features <- function(agent, features, rowToWrite) {
  for (colIdx in 1:ncol(agent$features)) {
    set(agent$features, as.integer(rowToWrite), colIdx, features[1, colIdx])
  }
}

update_features <- function(agent, features) {
  if (nrow(features) != agent$memory[valid == TRUE, .N]) {
    stop(paste("update_features: mismatching number of rows in given features and memory:",
               nrow(features),
               agent$memory[valid == TRUE, .N]))
  }
  nPold <- ncol(agent$features)
  nPnew <- ncol(features)
  if (nPold > nPnew) {
    agent$features[, (nPnew+1):nPold := NULL]
  }
  agent$features[agent$memory$valid, paste0("P", 1:nPnew) := features %>% as.data.frame]
  set_cache_value(agent, "nFeatures", nPnew)
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
  perceiverLabel <- unique(agent$memory$label[agent$memory$word == producedToken$word & agent$memory$valid == TRUE])
  
  # compute features on incoming token
  features <- exemplar2features(producedToken$exemplar, agent, params)
  
  # if word is unknown, assign label based on majority vote among perceptionOOVNN nearest neighbours
  if (length(perceiverLabel) == 0) {
    perceiverLabel <- names(which.max(table(agent$memory$label[agent$memory$valid == TRUE][
      knnx.index(agent$features[agent$memory$valid == TRUE,], features, params[["perceptionOOVNN"]])
      ])))
  }
  
  memorise <- TRUE
  for (strategy in params[["memoryIntakeStrategy"]]) {
    memorise <- memory_intake_strategy(strategy, producedToken$exemplar, features, perceiverLabel, agent, params)
    if (!memorise) break
  }
  
  # forget
  if (runif(1) < params[["forgettingRate"]]) {
    candidateRow <- sample(which(agent$memory$valid == TRUE), 1)
    candidateWord <- agent$memory$word[candidateRow]
    if (sum(agent$memory$word == candidateWord & agent$memory$valid, na.rm = TRUE) >= params[["productionMinTokens"]]) {
      set(agent$memory, candidateRow, "valid", FALSE)
    }
  }
  
  if (memorise) {
    # find next free row or row to be overwritten
    rowToWrite <- row_to_write(agent, producedToken, params)
    
    # write in agent's memory
    write_memory(agent, producedToken, rowToWrite, perceiverLabel)
    write_features(agent, features, rowToWrite)
    
    # update cache
    set_cache_value(agent, "nAccepted", get_cache_value(agent, "nAccepted") + 1)
    
    # empty cache
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }
  
  # write on interactionsLog
  if (isNotOwnToken) {
    write_interactions_log(interactionsLog, producedToken, agent, perceiverLabel, memorise, strategy, nrSim)
  }
  
  # update features if needed
  if (get_cache_value(agent, "nAccepted") %% params[["computeFeaturesInterval"]] == 0) {
    update_features(agent, compute_features(agent, params))
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }
  # re-estimate GMMs if needed
  if (grepl("^GMM(s)?", params[["perceptionModels"]]) && get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    estimate_GMM(agent, params)
  }
  # apply split&merge if needed
  if (params[["splitAndMerge"]] == T && get_cache_value(agent, "nAccepted") %% params[["splitAndMergeInterval"]] == 0) {
    splitandmerge(agent, params, full = FALSE)
    if (any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr")) && isNotOwnToken) {
      invalidate_cache(agent, "qda")
    }
  }
}

getPcols <- function(memory) {
  grep("^P[0-9]+$", colnames(memory), value = TRUE)
}

getNPcols <-  function(memory) {
  # temporary checks, should be moved to a test
  Pcols <- getPcols(memory)
  len <- length(Pcols)
  if (len == 0) return(0)
  if(!all.equal(Pcols, paste0("P", 1:len))) {
    stop(paste("Not contiguous sequence of feature column names:", Pcols))
  }
  return(len)
}

compute_features <- function(agent, params) {
  methodReg[params[["featureExtractionMethod"]], compute_features][[1]](agent$memory[valid == TRUE, exemplar], agent, params)
}

exemplar2features <- function(exemplar, agent, params) {
  methodReg[params[["featureExtractionMethod"]], exemplar2features][[1]](exemplar, agent, params)
}

features2exemplar  <- function(features, agent, params) {
  methodReg[params[["featureExtractionMethod"]], features2exemplar][[1]](features, agent, params)
}

memory_intake_strategy <- function(strategy, exemplar, features, label, agent, params) {
  methodReg[params[["featureExtractionMethod"]], memoryIntakeStrategy][[1]][[strategy]](exemplar, features, label, agent, params)
}
  
