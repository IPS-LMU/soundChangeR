################################################################################
#                                                                              #
# This script contains the functions that perform splits & mergers.            #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

splitandmerge <- function(agent, params, full = FALSE) {
  # This function performs splits and mergers on a perceiving agent.
  # Function call in interactions.R, perceive_token(), and 
  # loadLibraries.R, coreABM().
  #
  # Args:
  #    - agent: a list that is part of the population
  #    - params: list of params from params.R
  #    - full: boolean which tells you whether or not split&merge shall be
  #      performed until there are no further changes or not
  #
  # Returns:
  #    - nothing.
  #
  
  phoneFuncList <- list(split = phonsplit, merge = phonmerge)
  for (phoneFunc in c("split", "merge")) {
    didOneSM <- FALSE
    while (phoneFuncList[[phoneFunc]](agent)) {
      if (params[['runMode']] == "single" && !didOneSM) {
        cat("Agent", agent$agentID, "did", phoneFunc, "\n")
        didOneSM <- TRUE
      }
      if (!full) break
    }
  }
}

train_gaussian_model <- function (x, lab = rep("x", nrow(x))) {
  # This function builds a Gaussian distribution on the basis of data in x.
  # Function call in splitandmerge.R, split_merge_metric().
  #
  # Args:
  #    - x: data to calculate the Gaussian distribution from
  #    - lab: a vector of strings as long as nrow(x)
  #
  # Returns:
  #    - mat: data object that contains the mean, covariance and inverse covariance values
  #
  
  mat <- NULL
  if (!is.null(ncol(x))) {
    summeanvals <- NULL
    sumcovvals <- NULL
    sumcovvals.inv <- NULL
    for (j in unique(lab)) {
      temp <- lab == j
      if (sum(temp) <= 3) {
        stop("In train_gaussian_model: There are only 3 or less data points, so a Gaussian model cannot be generated.")
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
  # Function call in splitandmerge.R, splitandmerge().
  #
  # Args:
  #    - agent: a list that is part of the population
  #
  # Returns:
  #    - didMerge: boolean which stands for whether or not
  #      there has been a merger
  #
  
  # one agent
  # skip entire function if there's only one category
  if (length(unique(agent$labels$label[agent$labels$valid])) <= 1) {
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
      if(!is.null(ncol(as.matrix(agent$features)[agent$labels$label == lab & agent$labels$valid, ]))) {
        apply(as.matrix(agent$features)[agent$labels$label == lab & agent$labels$valid, ], 2, mean) 
      } else {
        mean(as.matrix(agent$features)[agent$labels$label == lab & agent$labels$valid, ])
      }
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
        # but for testing purpose I keep the original one, otherwise we get different random strings with same params[['seed']].
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

split_merge_metric <- function(P) {
  # This function computes a metric on each row of P based on a Gaussian model
  # trained on P itself. In this version the metric is a linear transformation of the log likelihood.
  # This (similarity) metric is used as a criterion to assess wether a split or a merge
  # are justified. 
  # Function call in splitandmerge.R, split_is_justified().
  #
  # Args:
  #    - P: data.table with the acoustic data of an agent
  #
  # Returns:
  #    - a numeric vector of length nrow(P) providing the metric for each row of P.
  #

  if (!is.null(nrow(P))) {
    tdat <- train_gaussian_model(P)
    as.numeric(distance(P, tdat, metric = "bayes"))
  } else {
    tdat <- train_gaussian_model(P, lab = rep("x", length(P)))
    as.numeric(log(abs((P - tdat$means)/tdat$cov)))
  }
}

split_is_justified <- function(P, wordValues, splitValues) {
  # This function essentially performs the t.test which decides whether or not 
  # two classes should be split.
  # Function calls in splitandmerge.R, phonsplit.sub() and phonmerge().
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
  
  # abort split & merge if there are 3 or less tokens in P that represent any of the 2 classes generated by PAM
  if(!is.null(nrow(P))) {
    if (nrow(P[splitValues == 1, ]) <= 3 || nrow(P[splitValues == 2, ]) <= 3) {
      cat("There are 3 or less acoustic tokens for one of the classes proposed by PAM. Aborting split and merge.\n")
      return(FALSE)
    }
  } else {
    if (length(P[splitValues == 1]) <= 3 || length(P[splitValues == 2]) <= 3) {
      cat("There are 3 or less acoustic tokens for one of the classes proposed by PAM. Aborting split and merge.\n")
      return(FALSE)
    }
  }
  
  if(!is.null(nrow(P))) {
    metric.split <- numeric(nrow(P))
  } else {
    metric.split <- numeric(length(P))
  }
  
  for (spl in 1:2) { # assume 2 clusters labelled as 1, 2
    if(!is.null(nrow(P))) {
      metric.split[splitValues == spl] <- split_merge_metric(P[splitValues == spl, ])
    } else {
      metric.split[splitValues == spl] <- split_merge_metric(P[splitValues == spl])
    }
    
  }
  metric.merge <- split_merge_metric(P)
  
  aggrMetric <- data.table(metric = metric.split - metric.merge, word = wordValues)[
    , .(mm = mean(metric)), by = word][
      , mm]
  
  return(t.test(aggrMetric)$p.value < 0.05 & mean(aggrMetric) > 0)
}

phonsplit.sub <- function(P, wordValues, label) {
  # This function returns the new label if a split is performed.
  # Function call in splitandmerge.R, phonsplit().
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
      # this is temp, only to make it possible to use params[['seed']] and compare results
      # break ties with an arbitrary criterion (which.min) indep of pam label, which can flip
      # with an identical P with different order or rows
      if (sum(cluster[word.mask] == 1) == sum(word.mask) / 2) {
        # print (paste("tie", label, w))
        if(!is.null(ncol(P))) {
          cluster[word.mask][which.min(P[word.mask, 1])]
        } else {
          cluster[word.mask][which.min(P[word.mask])]
        }
      }
      else if (sum(cluster[word.mask] == 1) > sum(word.mask) / 2) 1 else 2
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
  # Function call in splitandmerge.R, splitandmerge().
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
