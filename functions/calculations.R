################################################################################
#                                                                              #
# This script contains functions that perform calculations on the data used    #
# or produced in the simulations.                                              #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

convert_pop_list_to_dt <- function(pop, extraCols = list(condition = "x")) {
  # This function converts the population list into a data.table.
  # Function call in simulations.R, save_population().
  #
  # Args:
  #    - pop: population list
  #    - extraCols: a list of columns that will be added to the pop data.table
  #      Default: list(condition = "x")
  #
  # Returns:
  #    - the population as a data.table
  #
  
  # loop over the lists in pop
  rbindlist(lapply(seq_along(pop), function (i) {
    # the next four lines join the 'feature' columns, the 'labels' columns, 
    # and the 'initial' columns, for the current agent (pop[[i]])
    cbind(pop[[i]]$features, pop[[i]]$labels) %>%
      .[valid == TRUE] %>%
      inner_join(pop[[i]]$initial, by = 'word') %>%
      setDT %>%
      # the next four lines set agentID, speaker, group, equivalence for the current agent
      .[, `:=`(agentID = pop[[i]]$agentID,
               speaker = pop[[i]]$speaker,
               group = pop[[i]]$group)] %>%
      .[, equivalence := equal_class(initial, label)]
  })) %>% {
    # add every item from extraCols as a column to new pop data.table
    for (col in names(extraCols)) {
      .[, (col) := extraCols[[col]]]
    }
    .[]
  }
}

# convert_pop_dt_to_list <- function(pop.dt) {
#   # This function ...
#   # Function call in ...
#   #
#   # Args:
#   #    -
#   #
#   # Returns:
#   #    -
#   #
#   
#   population <- list()
#   Pcols <- grep("^P[[:digit:]]+$", colnames(pop.dt), value = TRUE)
#   for (id in pop.dt$agentID %>% unique) {
#     population[[id]] <- list()
#     population[[id]]$agentID <- id
#     population[[id]]$labels <- pop.dt[agentID == id, .(word, label, valid, nrOfTimesHeard, producerID, timeStamp)]
#     population[[id]]$group <- pop.dt[agentID == id, group][1]
#     population[[id]]$speaker <- pop.dt[agentID == id, speaker][1]
#     population[[id]]$features <- pop.dt[agentID == id, .SD, .SDcols = Pcols]
#     population[[id]]$initial <- pop.dt[valid == TRUE, .(word, initial)] %>% unique 
#     population[[id]]$cache <- data.table(name = "qda", value = list(), valid = FALSE)
#   }
#   return(population)
# }

# MAP_adapt_gaussian <- function(adaptData, priorData, priorAdaptRatio) {
#   # This function estimates a Gaussian, taking into account the prior Gaussian across
#   # all word tokens, and then tries to fit that to the few tokens of the target word.
#   # THIS FUNCTION CANNOT BE USED CURRENTLY because params.R misses productionMAPPriorAdaptRatio.
#   # Function call in interactions.R, produce_token().
#   #
#   # Args:
#   #    - adaptData: tokens of the target word
#   #    - priorData: tokens of all words in the same phonological class
#   #    - priorAdaptRatio: weight ratio between adaptData and priorData
#   #
#   # Returns:
#   #    - res: a list of mean and covariance matrix
#   #
#   
#   # k_0 == nu_0 == number of prior samples, this is the 'theoretical one'
#   # k_0 <- nu_0 <- nrow(priorData)
#   n <- nrow(adaptData)
#   k_0 <- nu_0 <- max(round(n * priorAdaptRatio), 1)
#   mu_0 <- apply(priorData, 2, mean)
#   x_hat <- apply(adaptData, 2, mean)
#   Psi <- cov(priorData) * max((nu_0 - 1), 1)
#   C <- 0
#   if (n > 1) {
#     C <- cov(adaptData) * (n - 1)
#   }
#   Psi_posterior <- Psi + C + (k_0 * n)/(k_0 + n) * outer(x_hat - mu_0, x_hat - mu_0)
#   p <- ncol(adaptData)
#   res <- list(
#     mean = (k_0 * mu_0 + n * x_hat) / (k_0 + n),
#     cov = Psi_posterior / (nu_0 + n + p + 1) # Mode, Mean would end in - p - 1
#   )
#   return(res)
# }

# ellipse_confidence <- function(P, alpha) {
#   # This function ...
#   # Function call in ...
#   #
#   # Args:
#   #    - 
#   #
#   # Returns:
#   #    - 
#   #
#   
#   # only 2-dim P
#   eig <- eigen(cov(P), only.values = FALSE)
#   Chisq <- sqrt(qchisq(1 - alpha, 2))
#   return( list(
#     C = colMeans(P),
#     a = Chisq * sqrt(eig$values[1]),
#     b = Chisq * sqrt(eig$values[2]),
#     theta = atan2(eig$vectors[2,1], eig$vectors[1,1])
#   ))
# }

knearest_fallback <- function(cloud, targetIndices, K) {
  # This function is used as an addition to SMOTE; i.e. if additional values 
  # (nExtraTokens > 0; see produce_token()) should be used for SMOTE, they are 
  # sampled in this function by using a K nearest neighbor approach.
  # Function call in interactions.R, produce_token().
  #
  # Args:
  #    - cloud: matrix of feature values
  #    - targetIndices: list of indices
  #    - K: number of nearest neighbors
  #
  # Returns:
  #    - list of targetIndices and fallback:
  #
  
  # stop if there is an empty cloud of feature values, if the words from the
  # targetIndices ar enot part of the cloud, if K <= 0, or if K + 1 is bigger
  # than the number of data points in the cloud
  if (nrow(cloud) < 1) {
    stop("knearest_fallback: Empty input cloud")
  }
  if (!all(targetIndices %in% 1:nrow(cloud))) {
    stop("knearest_fallback: targetIndices out of bound")
  }
  if (K <= 0 | K + 1 > nrow(cloud)) {
    stop(paste("knearest_fallback: invalid number of nearest neighbours requested: K =", K))
  }
  
  # compute number of tokens to be sampled
  nFallback <- K + 1 - length(targetIndices)
  
  # if it turns out that there are enough tokens in targetIndices, return them
  if (nFallback <= 0) {
    return(targetIndices)
  }
  
  # apply a K nearest neighbor algorithm on the cloud,
  # and sample as many as nFallback tokens from the unique neighbors 
  # that are not equal to the targetIndices
  fallback <- knnx.index(cloud, cloud[targetIndices, , drop=FALSE], K + 1) %>%
    as.vector %>%
    .[!. %in% targetIndices] %>%
    unique %>%
    sample(nFallback)
  
  # return a list of targetIndices and fallback
  return(c(targetIndices, fallback))
}

# calc_rejection_ratio <- function(population) {
#   # This function computes the ratio of rejections, 
#   # i.e. how many percent of all produced tokens were 
#   # rejected by the perceiving agent.
#   # Function call in coreABM.R.
#   #
#   # Args:
#   #    - population: result of perform_interactions(), defined in coreABM.R
#   #
#   # Returns:
#   #    - numRejections/numTokens: quotient
#   #
#   
#   numTokens <- 0
#   numRejections <- 0
#   for (i in 1:length(population)) {
#     numTokens <- numTokens + length(population[[i]]$memory$update)
#     numRejections <- numRejections + sum(population[[i]]$memory$update == "n")
#   }
#   return(numRejections/numTokens)
# }

# make_equivalence_labels <- function(labels) {
#   # This function generates equivalence labels from the 
#   # labels of the agents in the population.
#   # Function call in coreABM.R.
#   #
#   # Args:
#   #    - labels: originalPopulation$initial
#   #
#   # Returns:
#   #    - (no variable name): the newly formed labels
#   #
#   
#   ulab <- labels %>% unique %>% sort %>% as.character
#   return(
#     Map(combn,
#         list(ulab),
#         seq_along(ulab),
#         list(function(x) paste0(x, collapse="+"))
#         ) %>% unlist
#     )
# }

equal_class <- function(orig, derived) {
  # This function generates equivalence labels from the initial and 
  # developed labels of the agents in the population.
  # Function call in calculations.R, convert_pop_list_to_dt().
  #
  # Args:
  #    - orig: a vector of characters from pop[[i]]$initial$initial
  #    - derived: a vector of characters from pop[[i]]$labels$label
  #
  # Returns:
  #    - derived: the equivalence label
  #
  
  tab <- t(table(orig, derived))
  namesOfOrig <- colnames(tab)
  # namesOfOrig <- namesOfOrig[order(nchar(namesOfOrig), namesOfOrig)]
  namesOfDerived <- rownames(tab)
  # namesOfDerived <- namesOfDerived[order(nchar(namesOfDerived), namesOfDerived)]
  bintab <- matrix(FALSE, nrow = nrow(tab), ncol = ncol(tab))
  for (j in 1:ncol(tab)) {
    bintab[,j] <- tab[,j] != 0
  }
  for (j in 1:nrow(bintab)) {
    lab.equivalent <- paste(namesOfOrig[bintab[j, ]], collapse="+")
    derived[derived == namesOfDerived[j]] <- lab.equivalent
  }
  return(derived)
}

# get_equivalence_clusters <- function(population, eLabels) {
#   # This function calculates in how many of the agents each
#   # equivalence label occurs.
#   # Currently no function call.
#   #
#   # Args:
#   #    - population: a data.table, as in the format produced by save_population()
#   #    - eLabels: all the equivalence labels to take into account, usually obtained 
#   #     by running make_equivalence_labels()
#   #
#   # Returns:
#   #    - a data.table with columns 'equivalence', the labels, and 'N_Agents', 
#   #     the number of agents where a label occurs
#
#   eq <- data.table(equivalence = eLabels)
#   population[, .N, by = .(agentID, equivalence)][
#     , .SD[eq, .(equivalence, N), on = "equivalence"], by = agentID][
#       , .(N_Agents = sum(!is.na(N))), by = equivalence
#       ]
# }

# inv_dct_from_emuR <- function(X, N = 11) {
#   # This function computes the inverse DCT.
#   # Currently no function call
#   #
#   # Args:
#   #    - 
#   #
#   # Returns:
#   #    - 
#   #
#   
#   0.5 * (sqrt(2) - 1) * X[1] + dtt::dct(c(X, rep(0, N - length(X))), variant = 3)
# }

# empty_rows <- function(df) {
#   # This function finds empty rows in a data.frame or data.table.
#   # Currently no function call.
#   #
#   # Args:
#   #    - df: a data.frame or data.table.
#   #
#   # Returns:
#   #    - (no variable name): the rows which are empty (i.e. filled with NA).
#   #
#   
#   apply(df, 1, function(x) all(is.na(x))) %>% which
# }

# expanding_list <- function(capacity = 10) {
#   # This function changes the capacity of a list.
#   # Currently no function call.
#   # For more information, see:
#   # https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1
#   #
#   # Args:
#   #    - capacity: Numeric. Default: 10
#   #
#   # Returns:
#   #    - methods: a list
#   #
#   
#   buffer <- vector('list', capacity)
#   length <- 0
#   
#   methods <- list()
#   
#   methods$double.size <- function() {
#     buffer <<- c(buffer, vector('list', capacity))
#     capacity <<- capacity * 2
#   }
#   
#   methods$add <- function(val) {
#     if(length == capacity) {
#       methods$double.size()
#     }
#     
#     length <<- length + 1
#     buffer[[length]] <<- val
#   }
#   
#   methods$as.list <- function() {
#     b <- buffer[0:length]
#     return(b)
#   }
#   
#   methods
# }

