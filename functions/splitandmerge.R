################################################################################
#                                                                              #
# This script contains the following functions that perform splits & mergers:  #
#                                                                              #
# - splitandmergefull(agent)                                                   #
# - splitandmerge(agent)                                                       #
# - train_gaussian_model(x, lab = rep("x", nrow(x)))                           #
# - phonmerge_(Pdata, wordclass, phonclass)                                     #
# - phonmerge_.sub(param, wordlab, phonlab, threshold = 0.05)                   #
# - phonsplit_(Pdata, wordclass, phonclass)                                     #
# - phonsplit_.sub(param, wordlab, phonlab, threshold = 0.05)                   #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


splitandmerge <- function(agent, full = FALSE) {
  phoneFuncList <- list(split = phonsplit_,
                        merge = phonmerge_)
  validRows <- agent$labels[valid == TRUE, rowIndex]
  for (phoneFunc in c("split", "merge")) {
    while(!identical(lab <- phoneFuncList[[phoneFunc]](agent$features[validRows,],
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
  oldsplitMergeAgentLabel <- agent$memory$label
  agent$memory$label <- phonsplit_(agent$memory$P, agent$memory$word, agent$memory$label)
  if (sum(oldsplitMergeAgentLabel != agent$memory$label) > 0 & runMode == "single") {
    cat("Agent", unique(agent$memory$speaker), "did split\n")
  }
  splitMergeAgentTemp <- agent$memory$label != oldsplitMergeAgentLabel
  while (sum(splitMergeAgentTemp) != 0) {
    oldsplitMergeAgentLabel <- agent$memory$label
    agent$memory$label <- phonsplit_(agent$memory$P, agent$memory$word, agent$memory$label)
    if (sum(oldsplitMergeAgentLabel != agent$memory$label) > 0 & runMode == "single") {
      cat("Agent", unique(agent$memory$speaker), "did split\n")
    }
    splitMergeAgentTemp <- agent$memory$label != oldsplitMergeAgentLabel
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
  # Function call in perceiveToken() in interactions.R
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
  # Function call in phonmerge_.sub() and phonsplit_.sub() below.
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


phonmerge <- function(pop, idx) {
  # one agent
  # skip entire function if there's only one category
  if (pop$labels[idx, label %>% uniqueN] == 1) {
    return (FALSE)
  }
  
  didMerge <- FALSE
  didOneMerge <- TRUE # set TRUE in order to exec while loop at least once
  while(didOneMerge) {
    ulab <- pop$labels[idx, label %>% unique]
    distCentroids <- sapply(ulab, function(lab) {
      pop$features[pop$labels[idx, ][label == lab, rowIndex], ] %>% colMeans
    }) %>% t %>% dist
   
    didOneMerge <- FALSE
    for (i in distCentroids %>% order) {
      ulab.pair <- labels(distCentroids)[which(lower.tri(distCentroids),arr.ind=TRUE)[i,]]
      subIdx <- idx[pop$labels[idx, label] %in% ulab.pair]
      if (! split_is_justified(pop$features[subIdx,],
                             pop$labels[subIdx, word],
                             pop$labels[subIdx, label] %>% as.factor %>% as.integer)) {
        while ((mergeLabel <- stri_rand_strings(1, 6, "[a-z]")) %in% pop$labels[idx, label %>% unique]) {}
        pop$labels[subIdx, label := mergeLabel]
        # print (i)
        didOneMerge <- TRUE
        break
      }
    }
    didMerge <- didMerge || didOneMerge
    if (pop$labels[idx, label %>% uniqueN] == 1) {
      return (didMerge)
    }
  }
  return (didMerge)
}

phonmerge_ <- function(Pdata, wordclass, phonclass) {
  # This function performs the actual merge on two phoneme classes.
  # Function calls in splitandmergefull(), and splitandmerge()
  # in this script (see above).
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
  tdat <- train_gaussian_model(P);
  if (ncol(P) == 1) {
    log(abs((P - tdat$means)/tdat$cov)) %>% as.numeric()
  } else {
    distance(P, tdat, metric = "bayes") %>% as.numeric()
  }
}

split_is_justified <- function(P, wordCol, splitCol) {
  metric.split <- numeric(nrow(P))
  for (spl in 1:2) { # assume 2 clusters labelled as 1, 2
    metric.split[splitCol == spl] <- split_merge_metric(P[splitCol == spl, ])
  }
  metric.merge <- split_merge_metric(P)
  
  aggrMetric <- data.table(metric = metric.split - metric.merge, word = wordCol) %>%
    .[, .(mm = mean(metric)), by = word] %>%
    .[, mm]
  
  return(t.test(aggrMetric)$p.value < 0.05 & mean(aggrMetric) > 0)
}

phonsplit.sub <- function(pop, idx) {
  # one label, one agent
  cluster <- pam(pop$features[idx,], 2, cluster.only = T)
  for (w in pop$labels[idx, word %>% unique]) {
    subIdx <- pop$labels[idx, word == w]
    cluster[subIdx] <- {
      if (sum(cluster[subIdx] == 1) > sum(subIdx) / 2) 1 else 2
    }
  }
  
  # pop$labels[idx,
  #              .(cluster = pam(pop$features[idx,], 2, cluster.only = T),
  #                word)
  #              ][,
  #                cluster := {
  #                  # this line is more general 
  #                  # (does not assume number of clusters == 2 and cluster labels 1, 2)
  #                  # .SD[, .N, by = cluster][which.max(N), cluster]
  #                  # this line is specific for 2 clusters and faster
  #                  if (.SD[cluster == 1, .N] > .N/2) 1L else 2L
  #                },
  #                by = word,
  #                .SDcols = "cluster"
  #                ][] -> clusterByWord
  # 
  # did_split <- FALSE
  
  label <- pop$labels[idx, label]
  if (length(unique(cluster)) == 2 & all(sapply(1:2, function(cl) {
    length(unique(pop$labels[idx, word][cluster == cl])) > 1
  }))) {
    if (split_is_justified(pop$features[idx,], pop$labels[idx, word], cluster)) {
      label <- paste(label, cluster, sep = ".") 
    }
  }
  return(label)
  
  
  
  # if (clusterByWord$cluster %>% uniqueN == 2 &
  #     clusterByWord %>% unique %>% .[, .N, by = cluster] %>% .[, all(N > 1)]) {
  #   if (split_is_justified(pop$features[idx,], clusterByWord$word, clusterByWord$cluster)) {
  #     pop$labels[idx,
  #                  label := paste(label, clusterByWord$cluster, sep = ".")
  #                  ]
  #     did_split <- TRUE
  #   }
  # }
  # return(did_split)
}

 phonsplit <- function(pop, idx) {
  # one agent
  # pop$labels[idx,
  #              .(did_split = phonsplit.sub(pop, rowIndex)),
  #              by = label
  #              ][, Reduce(`|`, did_split)]
   
   labels <- character(length(idx))
   for (lab in pop$labels[idx, label %>% unique]) {
     labels[pop$labels[idx, label == lab]] <- phonsplit.sub(pop, pop$labels[rowIndex %in% idx & label == lab, rowIndex])
   }
   return(labels)
}





phonsplit_ <- function(Pdata, wordclass, phonclass) {
  # This function performs the actual split on two phoneme classes.
  # Function calls in splitandmergefull(), and splitandmerge()
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

