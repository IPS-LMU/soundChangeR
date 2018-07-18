################################################################################
#                                                                              #
# This script contains the following functions that perform splits & mergers:  #
#                                                                              #
# - splitandmerge(agent, full = FALSE)                                         #
# - splitandmerge_(agent, full = FALSE)                                        #
# - splitandmergefull(agent)                                                   #
# - splitandmerge_(agent)                                                      #
# - train_gaussian_model(x, lab = rep("x", nrow(x)))                           #
# - phonmerge(agent)                                                           #
# - phonmerge_(Pdata, wordclass, phonclass)                                    #
# - phonmerge_.sub(param, wordlab, phonlab, threshold = 0.05)                  #
# - split_merge_metric(P)                                                      #
# - split_is_justified(P, wordValues, splitValues)                             #
# - phonsplit.sub(P, wordValues, label)                                        #
# - phonsplit(agent)                                                           #
# - phonsplit_(Pdata, wordclass, phonclass)                                    #
# - phonsplit_.sub(param, wordlab, phonlab, threshold = 0.05)                  #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


splitandmerge <- function(agent, full = FALSE) {
  # This function performs splits and mergers on a perceiving agent.
  # Function call in interactions.R.
  #
  # Args:
  #    - agent: a list that is part of the population, as defined in coreABM.R
  #    - full: boolean which tells you whether or not split&merge shall be
  #      performed until there are no further changes or not
  #
  # Returns:
  #    - nothing. Simply changes the input.
  #
  
  phoneFuncList <- list(split = phonsplit,
                        merge = phonmerge)
  for (phoneFunc in c("split", "merge")) {
    didOneSM <- FALSE
    while (phoneFuncList[[phoneFunc]](agent)) {
      if (runMode == "single" && !didOneSM) {
        cat("Agent", agent$agentID, "did", phoneFunc, "\n")
        didOneSM <- TRUE
      }
      if (!full) break
    }
  }
}


splitandmerge_ <- function(agent, full = FALSE) {
  # Another function for split & merge. Currently no function call.
  #
  # Args:
  #    - agent: a list that is part of the population, as defined in coreABM.R
  #    - full: boolean which tells you whether or not split&merge shall be
  #      performed until there are no further changes or not
  #
  # Returns:
  #    - nothing. Simply changes the input.
  #
  
  phoneFuncList <- list(split = phonsplit_,
                        merge = phonmerge_)
  validRows <- which(agent$labels$valid == TRUE)
  for (phoneFunc in c("split", "merge")) {
    while(!identical(lab <- phoneFuncList[[phoneFunc]](as.matrix(agent$features[validRows,]),
                                                       agent$labels[validRows, word],
                                                       agent$labels[validRows, label]),
                     agent$labels[validRows, label])) {
      agent$labels[validRows, label := lab]
      if (runMode == "single") {
        cat("Agent", agent$agentID, "did", phoneFunc, "\n")
      }
      if (!full) break
    }
  }
}


splitandmergefull <- function(agent) {
  # This function performs splits and mergers on an agent until
  # no more changes occur.
  # Function call in coreABM.R.
  #
  # Args:
  #    - agent: a list that is part of the population, as defined in coreABM.R
  #
  # Returns:
  #    - agent: the same except with changed labels, if split&merge occurred
  #
  
  oldsplitMergeAgentLabel <- speaker_1$memory$label
  speaker_1$memory$label <- phonsplit_(speaker_1$memory$P, speaker_1$memory$word, speaker_1$memory$label)
  if (sum(oldsplitMergeAgentLabel != speaker_1$memory$label) > 0 & runMode == "single") {
    cat("Agent", unique(speaker_1$memory$speaker), "did split\n")
  }
  
  splitMergeAgentTemp <- speaker_1$memory$label != oldsplitMergeAgentLabel
  while (sum(splitMergeAgentTemp) != 0) {
    oldsplitMergeAgentLabel <- speaker_1$memory$label
    speaker_1$memory$label <- phonsplit_(speaker_1$memory$P, speaker_1$memory$word, speaker_1$memory$label)
    if (sum(oldsplitMergeAgentLabel != speaker_1$memory$label) > 0 & runMode == "single") {
      cat("Agent", unique(speaker_1$memory$speaker), "did split\n")
    }
    splitMergeAgentTemp <- speaker_1$memory$label != oldsplitMergeAgentLabel
  }
  
  if (length(unique(agent$memory$label)) > 1) {
    oldsplitMergeAgentLabel <- agent$memory$label
    agent$memory$label <- phonmerge_(agent$memory$P, agent$memory$word, agent$memory$label)
    if (sum(oldsplitMergeAgentLabel != agent$memory$label) > 0 & runMode == "single") {
      cat("Agent", unique(agent$memory$speaker), "did merge\n")
    }
    splitMergeAgentTemp <- agent$memory$label != oldsplitMergeAgentLabel
    while (sum(splitMergeAgentTemp) != 0 && length(unique(agent$memory$label)) > 1) {
      oldsplitMergeAgentLabel <- agent$memory$label
      agent$memory$label <- phonmerge_(agent$memory$P, agent$memory$word, agent$memory$label)
      if (sum(oldsplitMergeAgentLabel != agent$memory$label) > 0 & runMode == "single") {
        cat("Agent", unique(agent$memory$speaker), "did merge\n")
      }
      splitMergeAgentTemp <- agent$memory$label != oldsplitMergeAgentLabel
    }
  }
  return(agent)
}


splitandmerge_ <- function(agent) {
  # This function uses the split&merge algorithm on the given agent.
  # Currently no function call.
  #
  # Args:
  #    - agent: a list that is part of a population, as defined in coreABM.R
  #
  # Returns:
  #    - agent: the same list except with changed labels if split&merge occurred
  #
  
  oldLabel <- agent$memory$label
  agent$memory$label <- phonsplit_(agent$memory$P, agent$memory$word, agent$memory$label)
  
  if (sum(oldLabel != agent$memory$label) > 0 & runMode == "single") {
    cat("Agent", unique(agent$memory$speaker), "did split\n")
  }
  if (length(unique(agent$memory$label)) > 1) {
    oldLabel <- agent$memory$label
    agent$memory$label <- phonmerge_(agent$memory$P, agent$memory$word, agent$memory$label)
    if (sum(oldLabel != agent$memory$label) > 0 & runMode == "single") {
      cat("Agent", unique(agent$memory$speaker), "did merge\n")
    }
  }
  return(agent)
}


train_gaussian_model <- function (x, lab = rep("x", nrow(x))) {
  # This function builds a Gaussian distribution on the basis of data in x.
  # Function call in phonmerge_.sub(), phonsplit_.sub(), and 
  # split_merge_metric() in this script.
  #
  # Args:
  #    - x: data to calculate the Gaussian distribution from; see phonmerge_.sub() or
  #      phonsplit_.sub() for the kind of data this function can be used with
  #
  # Returns:
  #    - mat: data object that contains the mean, covariance and inverse covariance values
  #
  
  mat <- NULL
  if (ncol(x) != 1) {
    summeanvals <- NULL
    sumcovvals <- NULL
    sumcovvals.inv <- NULL
    for (j in unique(lab)) {
      temp <- lab == j
      if (sum(temp) == 1) {
        stop("\n\tData passed to function has only one entry for one of the labels.\n\tA gaussian model can't be generated for this data.")
      }
      values <- x[temp, ]
      meanvals <- apply(values, 2, mean)
      covvals <- var(values, values)
      covvals.inv <- solve(covvals)
      summeanvals <- rbind(summeanvals, meanvals)
      sumcovvals <- rbind(sumcovvals, covvals)
      sumcovvals.inv <- rbind(sumcovvals.inv, covvals.inv)
    }
    mat$label <- unique(lab)
    mat$means <- summeanvals
    mat$cov <- sumcovvals
    mat$invcov <- sumcovvals.inv
  } else {
    mat <- NULL
    mat$means <- NULL
    mat$cov <- NULL
    for (j in unique(lab)) {
      temp <- lab == j
      mat$means <- c(mat$means, mean(x[temp]))
      mat$cov <- c(mat$cov, sqrt(var(x[temp])))
    }
    mat$label <- unique(lab)
    mat$invcov <- 1/mat$cov
  }
  return(mat)
}


phonmerge <- function(agent) {
  # This function performs the actual merge on two phoneme classes.
  # Function call in splitandmerge() in this script (see above).
  #
  # Args:
  #    - agent: a list that is part of population
  #
  # Returns:
  #    - didMerge: boolean which stands for whether or not
  #      there has been a merger
  #
  
  # one agent
  # skip entire function if there's only one category
  if (length(unique(agent$labels$word[agent$labels$valid])) <= 1) {
    return(FALSE)
  }
  
  # initiate output variable didMerge with FALSE
  # set didOneMerge to TRUE in order to exec while loop at least once
  didMerge <- FALSE
  didOneMerge <- TRUE
  
  # test wether there are two classes to be merged
  while(didOneMerge) {
    ulab <- unique(agent$labels$label[agent$labels$valid])
    distCentroids <- dist(t(sapply(ulab, function(lab) {
      apply(as.matrix(agent$features)[agent$labels$label == lab & agent$labels$valid, ], 2, mean) 
    })))
    didOneMerge <- FALSE
    for (i in order(distCentroids)) {
      ulab.pair <- labels(distCentroids)[which(lower.tri(distCentroids),arr.ind=TRUE)[i,]]
      ulab.pair.mask <- agent$labels$label %in% ulab.pair
      if (! split_is_justified(as.matrix(agent$features)[ulab.pair.mask,],
                               agent$labels$word[ulab.pair.mask],
                               as.integer(as.factor(agent$labels$label[ulab.pair.mask])) )) {
        while ((mergeLabel <- paste(sample(letters[1:26])[1:6], collapse = "")) %in% ulab) {}
        # stri_rand_strings(1, 6, "[a-z]")) # this is a bit nicer than paste(sample... collapse = ""))
        # but for testing purpose I keep the original one, otherwise we get different random strings with same seed.
        agent$labels[ulab.pair.mask, label := mergeLabel]
        didOneMerge <- TRUE
        break
      }
    }
    
    didMerge <- didMerge || didOneMerge
    if (length(unique(agent$labels$word[agent$labels$valid])) == 1) {
      return (didMerge)
    }
  }
  return (didMerge)
}


phonmerge_ <- function(Pdata, wordclass, phonclass) {
  # This function performs the actual merge on two phoneme classes.
  # Function call in splitandmergefull() in this script (see above).
  #
  # Args:
  #    - Pdata: the P1, P2, etc. features of a specific agent
  #    - wordclass: the corresponding word
  #    - phonclass: the corresponding phoneme label
  #
  # Returns:
  #    - phonclass: the new phoneme label
  #
  
  phonclass <- as.character(phonclass)
  wordclass <- as.character(wordclass)
  flagmult <- T
  if (ncol(Pdata) == 1) {
    flagmult <- F
  }
  if (length(unique(phonclass)) < 2) {
    stop("you must have at least two phonological categories to do the merger")
  }
  euc <- function(a, b) {
    sqrt(sum((a - b)^2))
  }
  flag <- T
  while (flag == T) {
    ulab <- unique(phonclass)
    if (length(ulab) < 2) {
      flag <- F
    } else {
      ulab.comb <- combn(ulab, 2, simplify = T)
      ulab.cen <- NULL
      for (j in 1:ncol(ulab.comb)) {
        temp <- ulab.comb == j
        c1 <- apply(as.matrix(Pdata[phonclass == ulab.comb[1, j], ]), 2, mean)
        c2 <- apply(as.matrix(Pdata[phonclass == ulab.comb[2, j], ]), 2, mean)
        ulab.cen <- c(ulab.cen, euc(c1, c2))
      }
      z <- sort.list(ulab.cen)
      if (length(z) != 1) {
        ulab.comb <- ulab.comb[, z]
      }
      n <- ncol(ulab.comb)
      for (i in 1:n) {
        temp1 <- phonclass == ulab.comb[1, i]
        Pdata1 <- Pdata[temp1, ]
        word1 <- wordclass[temp1]
        phon1 <- rep(ulab.comb[1, i], sum(temp1))
        temp2 <- phonclass == ulab.comb[2, i]
        Pdata2 <- Pdata[temp2, ]
        word2 <- wordclass[temp2]
        phon2 <- rep(ulab.comb[2, i], sum(temp2))
        if (flagmult) {
          d <- rbind(Pdata1, Pdata2)
        } else {
          d <- c(Pdata1, Pdata2)
          d <- cbind(d)
        }
        w <- c(word1, word2)
        p <- c(phon1, phon2)
        if (phonmerge_.sub(d, w, p)) {
          newclasslab <- paste(sample(letters[1:26])[1:6], collapse = "")
          while (sum(newclasslab == phonclass) != 0) {
            newclasslab <- paste(sample(letters[1:26])[1:6], collapse = "")
          }
          phonclass[temp1] <- newclasslab
          phonclass[temp2] <- newclasslab
          break
        }
        if (i == n) {
          flag <- F
        }
      }
    }
  }
  return(phonclass)
}


phonmerge_.sub <- function(param, wordlab, phonlab, threshold = 0.05) {
  # This function is part of the merge algorithm and makes the
  # decision on whether or not the merger shall take place based
  # on statistics.
  # Function call in phonmerge_() in this script (see above).
  #
  # Args:
  #    - param: a matrix of values (one row per observation),
  #      must be a matrix even if only one column!
  #    - wordlab: a parallel set of word labels
  #    - phonlab: a parallel set of phoneme labels consisting of
  #      two and only two phoneme types
  #    - threshold: the probability threshold of the t-statistic
  #      for deciding whether or not merge the two clusters
  #
  # Returns:
  #    - mergecat: logical value, either TRUE or FALSE
  #
  
  flagmult <- T
  if (ncol(param) == 1) {
    flagmult <- F
  }
  
  mergecat <- F
  if (length(unique(phonlab)) != 2) {
    stop("phonlab must have exactly two categories")
  }
  # calculate Bayesian distances to the medoid of the two clusters
  distance.cluster <- rep(0, nrow(param))
  for (j in unique(phonlab)) {
    temp.cluster <- phonlab == j
    tdat.cluster <- train_gaussian_model(cbind(param[temp.cluster,]))
    if (flagmult) {
      distance.cluster[temp.cluster] <- c(distance(param[temp.cluster, ], tdat.cluster, metric = "bayes"))
    } else {
      distance.cluster[temp.cluster] <- log(abs((param[temp.cluster, ] - tdat.cluster$means)/tdat.cluster$cov))
    }
  }
  
  # calculate the distance to the combined data
  tdat.orig <- train_gaussian_model(param)
  if (flagmult) {
    distance.orig <- c(distance(param, tdat.orig, metric = "bayes"))
  } else {
    distance.orig <- c(log(abs((param - tdat.orig$means)/tdat.orig$cov)))
  }
  
  # run t-test aggregated by word to test whether the Bayesian distances to the two clusters 
  # are significantly greater than the Bayesian distances to the combination of the two; 
  # if not, return the same phoneme labels for all observations
  distance.df <- data.frame(d = distance.cluster - distance.orig, W = factor(wordlab))
  dm.df <- aggregate(d ~ W, mean, data = distance.df)
  dm.t <- t.test(dm.df$d)
  if ((dm.t$p.value > threshold) | ( (mean(dm.df$d) < 0))) {
    mergecat <- T
  }
  return(mergecat)
}


split_merge_metric <- function(P) {
  # This function tests for the dimensionality of P and
  # applies the adequate metric.
  # Function call in split_is_justified() in this script (see below).
  #
  # Args:
  #    - P: data.table with the acoustic data of an agent
  #
  # Returns:
  #    - nothing. Simply changes the input
  #
  
  tdat <- train_gaussian_model(P);
  if (ncol(P) == 1) {
    as.numeric(log(abs((P - tdat$means)/tdat$cov)))
  } else {
    as.numeric(distance(P, tdat, metric = "bayes"))
  }
}


split_is_justified <- function(P, wordValues, splitValues) {
  # This function essentially performs the t.test which 
  # decides whether or not two classes should be splitted.
  # Function calls in phonsplit.sub() and phonmerge() in this script.
  #
  # Args:
  #    - P: data.table with the acoustic data of an agent
  #    - wordValues: the corresponding word labels
  #    - splitValues: the corresponding labels
  #
  # Returns:
  #    - a boolean; TRUE means that the split should be performed 
  #      according to the t-test and metric
  #
  
  metric.split <- numeric(nrow(P))
  for (spl in 1:2) { # assume 2 clusters labelled as 1, 2
    metric.split[splitValues == spl] <- split_merge_metric(P[splitValues == spl, ])
  }
  metric.merge <- split_merge_metric(P)
  
  aggrMetric <- data.table(metric = metric.split - metric.merge, word = wordValues)[
    , .(mm = mean(metric)), by = word][
      , mm]
  
  return(t.test(aggrMetric)$p.value < 0.05 & mean(aggrMetric) > 0)
}


phonsplit.sub <- function(P, wordValues, label) {
  # This function returns the new label if a split is performed.
  # Function call in phonsplit() in this script.
  #
  # Args:
  #    - P: a data.frame with the acoustic data of an agent
  #    - wordValues: the corresponding word labels
  #    - label: the corresponding phoneme labels
  #
  # Returns:
  #    - label: the new label after the split
  #
  
  # one label, one agent
  cluster <- pam(P, 2, cluster.only = T)
  for (w in unique(wordValues)) {
    word.mask <- wordValues == w
    cluster[word.mask] <- {
      if (sum(cluster[word.mask] == 1) > sum(word.mask) / 2) 1 else 2
    }
  }
  if (length(unique(cluster)) == 2 & all(sapply(1:2, function(cl) {
    length(unique(wordValues[cluster == cl])) > 1
  }))) {
    if (split_is_justified(P, wordValues, cluster)) {
      label <- paste(label, cluster, sep = ".") 
    }
  }
  return(label)
}


phonsplit <- function(agent) {
  # This function performs the actual split on two phoneme classes.
  # Function call in splitandmerge() in this script (see above).
  # 
  # Args:
  #    - agent: a list that is part of population
  #
  # Returns:
  #    - didSplit: a boolean; TRUE means that a split has taken place.
  #
  
  didSplit <- FALSE
  for (lab in unique(agent$labels$label[agent$labels$valid])) {
     splitLab <- phonsplit.sub(as.matrix(agent$features)[agent$labels$label == lab & agent$labels$valid, ],
                               agent$labels$word[agent$labels$label == lab & agent$labels$valid],
                               lab)
     if (splitLab[1] != lab) {
       agent$labels[label == lab, label := splitLab]
       didSplit <- TRUE
     }
   }
   return(didSplit)
}


phonsplit_ <- function(Pdata, wordclass, phonclass) {
  # This function performs the actual split on two phoneme classes.
  # Function calls in splitandmergefull() and splitandmerge_()
  # in this script (see above).
  #
  # Args:
  #    - Pdata: the P1, P2, etc. features of a specific agent
  #    - wordclass: the corresponding word
  #    - phonclass: the corresponding phoneme label
  #
  # Returns:
  #    - phonclass: a vector of the new phoneme labels
  #
  
  Pdata <- as.matrix(Pdata)
  phonclass <- as.character(phonclass)
  wordclass <- as.character(wordclass)
  for (j in unique(phonclass)) {
    temp <- phonclass == j
    phonclass[temp] <- phonsplit_.sub(Pdata[temp, ], wordclass[temp], phonclass[temp])
  }
  return(phonclass)
}


phonsplit_.sub <- function(param, wordlab, phonlab) {
  # This function is part of the split algorithm and tests whether a
  # given phoneme class should split into two clusters.
  # Function call in phonsplit_() in this script (see above).
  #
  # Args:
  #    - param: a matrix of values (one row per observation),
  #      must be a matrix even if only one column!
  #    - wordlab: a parallel set of word labels
  #    - phonlab: a parallel set of phoneme labels consisting of
  #      two phoneme types
  #
  # Returns:
  #    - phonlab: the new phoneme label
  #
  
  if (!is.matrix(param)) {
    param <- cbind(param)
  }
  
  # split the data into two clusters using (unsupervised) k-means clustering
  param.k <- NULL
  param.k$cluster <- pam(param, 2, cluster.only = T)
  
  # assign each word to one or the other cluster depending on whichever cluster 
  # includes the majority of that word's tokens
  cluster.vec <- rep("", nrow(param))
  for (j in unique(wordlab)) {
    temp.1 <- wordlab == j & param.k$cluster == 1
    temp.2 <- wordlab == j & param.k$cluster == 2
    if (sum(temp.1) > sum(temp.2)) {
      cluster.vec[wordlab == j] <- "1"
    } else {
      cluster.vec[wordlab == j] <- "2"
    }
  }
  
  # do not allow a cluster to consist of just one word - so only apply all of 
  # the rest of the code if there is more than one word in each cluster
  if ((length(unique(wordlab[cluster.vec == "1"])) > 1) & (length(unique(wordlab[cluster.vec == "2"])) > 1)) {
    
    # calculate Bayesian distances to the medoid of cluster 1
    distance.cluster <- rep(0, nrow(param))
    if (any(cluster.vec == "1")) {
      temp.1 <- cluster.vec == "1"
      tdat.1 <- train_gaussian_model(param[temp.1,])
      # tdat.1 <- mclust::mvn("XXX",param[temp.1,])
      # 4.
      flagmult <- T
      if (ncol(param) == 1) {
        flagmult <- F
      }
      if (flagmult) {
        distance.cluster[temp.1] <- distance(param[temp.1,], tdat.1, metric = "bayes")
        # distance.cluster[temp.1] <- dmvnorm(param[temp.1,], tdat.1$parameters$mean, tdat.1$parameters$variance$Sigma, log = TRUE)
      } else {
        distance.cluster[temp.1] <- log(abs((param[temp.1,] - tdat.1$means)/tdat.1$cov))
      }
    }
    
    # calculate Bayesian distances to the medoid of cluster 2
    if(any(cluster.vec == "2")) {
      temp.2 <- cluster.vec == "2"
      tdat.2 <- train_gaussian_model(param[temp.2,])
      # tdat.2 <- mclust::mvn("XXX",param[temp.2,])
      if (flagmult) {
        distance.cluster[temp.2] <- distance(param[temp.2,], tdat.2, metric = "bayes")
        # distance.cluster[temp.2] <- dmvnorm(param[temp.2,], tdat.2$parameters$mean, tdat.2$parameters$variance$Sigma, log = TRUE)
        
      } else {
        distance.cluster[temp.2] <- log(abs((param[temp.2,] - tdat.2$means)/tdat.2$cov))
      }
    }
    
    # calculate Bayesian distances to the medoid of the original data of all tokens
    tdat.orig <- train_gaussian_model(param)
    # tdat.orig <- mclust::mvn("XXX",param)
    if (flagmult) {
      distance.orig <- c(distance(param, tdat.orig, metric = "bayes"))
      # distance.orig <- dmvnorm(param, tdat.orig$parameters$mean, tdat.orig$parameters$variance$Sigma, log = TRUE)
      
    } else {
      distance.orig <- c(log(abs((param - tdat.orig$means)/tdat.orig$cov)))
    }
    
    # run t-test aggregated by word to test whether the Bayesian distances to the two 
    # clusters are significantly greater than the Bayesian distances to the original; 
    # if so, return a vector of phonological labels, split into two categories
    distance.df <- data.frame(d = distance.cluster - distance.orig, W = factor(wordlab))
    dm.df <- aggregate(d ~ W, mean, data = distance.df)
    dm.t <- t.test(dm.df$d)
    if (dm.t$p.value < 0.05 & mean(dm.df$d) > 0) {
      phonlab <- paste(phonlab, cluster.vec, sep = ".")
    }
  }
  return(phonlab)
}

