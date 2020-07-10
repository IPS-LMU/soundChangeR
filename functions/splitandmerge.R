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
    while (phoneFuncList[[phoneFunc]](agent, params[['splitMergeMethod']])) {
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


phonmerge <- function(agent, splitMergeMethod) {
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
                               as.integer(as.factor(agent$labels$label[ulab.pair.mask])),
                               splitMergeMethod)) {
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


split_is_justified <- function(P, wordValues, splitValues, method = "t.test") {
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
  if (method == "t.test") {
    metric.split <- numeric(nrow(P))
    for (spl in 1:2) { # assume 2 clusters labelled as 1, 2
      metric.split[splitValues == spl] <- split_merge_metric(P[splitValues == spl, ])
    }
    metric.merge <- split_merge_metric(P)
    
    aggrMetric <- data.table(metric = metric.split - metric.merge, word = wordValues)[
      , .(mm = mean(metric)), by = word][
        , mm]
    
    return(t.test(aggrMetric)$p.value < 0.05 & mean(aggrMetric) > 0)
  } else if (method == "bic") {
    model.merge <- MclustDA(P, "x", G = 1, modelNames = "XXX", verbose = FALSE)
    model.split <- MclustDA(P, splitValues, G = 1, modelNames = "XXX", verbose = FALSE)
    npar <- nVarParams("XXX", d = ncol(P), G = 1) + ncol(P)
    bic.merge <- 2 * model.merge$loglik - npar * log(model.merge$n)
    bic.split <- 2 * (model.split$models$`1`$loglik + model.split$models$`2`$loglik) -
      2 * npar * log(model.merge$n)
    return (bic.split > bic.merge)
  }
}


phonsplit.sub <- function(P, wordValues, label, splitMergeMethod) {
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
    if (split_is_justified(P, wordValues, cluster, splitMergeMethod)) {
      label <- paste(label, cluster, sep = ".") 
    }
  }
  return(label)
}


phonsplit <- function(agent, splitMergeMethod) {
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
                               lab,
                               splitMergeMethod)
     if (splitLab[1] != lab) {
       agent$labels[label == lab & valid == TRUE, label := splitLab]
       didSplit <- TRUE
     }
   }
   return(didSplit)
}

###### GMM-based split&merge

logical_max <- function(x, vec = vector(mode = "logical", length = length(x))) {vec[which.max(x)] <- TRUE; vec}

reduced_word_clusters <- function(fullWordClusters, rank) {
  if (any(fullWordClusters <0)) {stop("reduced_word_clusters: fullWordClusters values cannot be negative")}
  if (rank > ncol(fullWordClusters)) {stop("reduced_word_clusters: rank cannot exceed number of columns of fullWordClusters")}
  zeroCols <- apply(fullWordClusters, 2, sum) == 0
  if (rank > ncol(fullWordClusters) - sum(zeroCols)) {stop("reduced_word_clusters: too many zero columns")}
  if (sum(zeroCols) > 0) {fullWordClusters <- fullWordClusters[, !zeroCols]}
  if (rank == 1) {return(apply(fullWordClusters, 1, sum) %>% as.matrix(ncol = 1))}
  nmfObj <- nmf(x = fullWordClusters, rank = rank, method = "nsNMF")
  reducedClusters <- nmfObj %>% coef() %>%  apply(2, logical_max)
  return(fullWordClusters %*% t(reducedClusters))
}

compute_purity <- function(clustersMat, summaryFunc = min) {
  whichMax <- clustersMat %>% apply(1, which.max)
  sapply(seq_len(ncol(clustersMat)), function(j) {
    sum(clustersMat[whichMax == j, j]) / sum(clustersMat[, j])
  }) %>% summaryFunc
}

compute_purity_matrix <- function(fullWordCluster, reps) {
  purity <- matrix(0, nrow = ncol(fullWordCluster)-1, ncol = reps)
  for (r in 2:ncol(fullWordCluster)) {
    for (rr in 1:reps) {
      purity[r-1, rr] <- reduced_word_cluster(fullWordCluster, r) %>% compute_purity
    }
  }
  return(purity)
}

estimate_number_of_labels_from_purity_matrix <- function(purityMatrix, purityThr) {
  1 + Position(function(x) {!is.na(x) & x > purityThr},
               purityMatrix %>% apply(1, mean),
               right = TRUE,
               nomatch = 0)
}

