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
    while (phoneFuncList[[phoneFunc]](agent, params[["splitMergeMethod"]])) {
      if (params[["runMode"]] == "single" && !didOneSM) {
        cat("Agent", agent$agentID, "did", phoneFunc, "\n")
        didOneSM <- TRUE
      }
      if (!full) break
    }
  }
}

train_gaussian_model <- function(x, lab = rep("x", nrow(x))) {
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
        print("In train_gaussian_model: There are only 3 or less data points, so a Gaussian model cannot be generated.")
        return(NULL)
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
  
  # skip entire function if there's only one category
  if (length(unique(agent$memory$label[agent$memory$valid])) <= 1) {
    return(FALSE)
  }
  
  # initiate output variable didMerge with FALSE
  # set didOneMerge to TRUE in order to exec while loop at least once
  didMerge <- FALSE
  didOneMerge <- TRUE
  
  # test whether there are two classes to be merged
  while(didOneMerge) {
    ulab <- unique(agent$memory$label[agent$memory$valid])
    distCentroids <- dist(t(sapply(ulab, function(lab) {
      if(!is.null(ncol(as.matrix(agent$features)[agent$memory$label == lab & agent$memory$valid, ]))) {
        apply(as.matrix(agent$features)[agent$memory$label == lab & agent$memory$valid, ], 2, mean) 
      } else {
        mean(as.matrix(agent$features)[agent$memory$label == lab & agent$memory$valid, ])
      }
    })))
    didOneMerge <- FALSE
    for (i in order(distCentroids)) {
      ulab.pair <- labels(distCentroids)[which(lower.tri(distCentroids),arr.ind=TRUE)[i,]]
      ulab.pair.mask <- agent$memory$label %in% ulab.pair
      if (! split_is_justified(as.matrix(agent$features)[ulab.pair.mask,],
                               agent$memory$word[ulab.pair.mask],
                               as.integer(as.factor(agent$memory$label[ulab.pair.mask])),
                               splitMergeMethod)) {
        while ((mergeLabel <- paste(sample(letters[1:26])[1:6], collapse = "")) %in% ulab) {}
        # stri_rand_strings(1, 6, "[a-z]")) # this is a bit nicer than paste(sample... collapse = ""))
        # but for testing purpose I keep the original one, otherwise we get different random strings with same params[['seed']].
        agent$memory[ulab.pair.mask, label := mergeLabel]
        didOneMerge <- TRUE
        break
      }
    }
    
    didMerge <- didMerge || didOneMerge
    if (length(unique(agent$memory$word[agent$memory$valid])) == 1) {
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
    tdat <- estimate_gaussian(P)
    tdat$label <- "x" # hack because of emuR::distance
    tdat$means <- matrix(tdat$mean, ncol = ncol(P)) # hack because of emuR::distance
    as.numeric(emuR::distance(P, tdat, metric = "bayes"))
  } else {
    tdat <- train_gaussian_model(P, lab = rep("x", length(P)))
    if(is.null(tdat)) {
      return()
    }
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
  
  P <- as.data.table(P)
  if (nrow(P) <= 3) {
    return(FALSE)
  }
  if (method == "t.test") {
    metric.split <- as.numeric(nrow(P))
    for (spl in 1:2) { # assume 2 clusters labelled as 1, 2
      if (nrow(P[splitValues == spl, ]) <= 3) {
        return(FALSE)
      }
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
  for (lab in unique(agent$memory$label[agent$memory$valid])) {
	 if (ncol(agent$features) == 1 && length(agent$features[agent$memory$label == lab & agent$memory$valid, ][["P1"]]) <= 2 | 
	 ncol(agent$features) > 1 && nrow(as.matrix(agent$features)[agent$memory$label == lab & agent$memory$valid, ]) <= 2) {
	   return(didSplit)
     }
     splitLab <- phonsplit.sub(as.matrix(agent$features)[agent$memory$label == lab & agent$memory$valid, ],
                               agent$memory$word[agent$memory$label == lab & agent$memory$valid],
                               lab,
                               splitMergeMethod)
     if (splitLab[1] != lab) {
       agent$memory[label == lab & valid == TRUE, label := splitLab]
       didSplit <- TRUE
     }
   }
   return(didSplit)
}

###### GMM-based split&merge

logical_max <- function(x, vec = vector(mode = "logical", length = length(x))) {vec[which.max(x)] <- TRUE; vec}

reduced_clusters_incidence_matrix <- function(fullClusters, rank) {
  if (any(fullClusters <0)) {stop("reduced_word_clusters_incidence_matrix: fullClusters values cannot be negative")}
  if (rank > ncol(fullClusters)) {stop("reduced_clusters_incidence_matrix: rank cannot exceed number of columns of fullClusters")}
  zeroCols <- apply(fullClusters, 2, sum) == 0
  if (rank > ncol(fullClusters) - sum(zeroCols)) {stop("reduced_clusters_incidence_matrix: too many zero columns")}
  if (sum(zeroCols) > 0) {fullClusters <- fullClusters[, !zeroCols]}
  if (rank == 1) {
    incidenceMatrix <- matrix(TRUE, nrow = 1, ncol = ncol(fullClusters))
  } else {
    nmfObj <- nmf(x = fullClusters, rank = rank, method = "nsNMF")
    incidenceMatrix <- nmfObj %>% coef() %>%  apply(2, logical_max)
  }
  if (sum(zeroCols) == 0) {
    return(incidenceMatrix)
  } else {
    incidenceMatrixZeroCols <- matrix(FALSE, nrow = rank, ncol = length(zeroCols))
    incidenceMatrixZeroCols[, !zeroCols] <- incidenceMatrix
    return(incidenceMatrixZeroCols)
  }
}

compute_purity <- function(clustersMat, summaryFunc = min) {
  whichMax <- clustersMat %>% apply(1, which.max)
  sapply(seq_len(ncol(clustersMat)), function(j) {
    sum(clustersMat[whichMax == j, j]) / sum(clustersMat[, j])
  }) %>% summaryFunc
}

compute_purity_matrix <- function(fullClusters, reps) {
  # compute reps time each purity for solution with r clusters,
  # from 2 to number of clusters in fullClusters (= ncol)
  # organise it in a reps by ncol(fullClusters)-1 matrix
  purity <- matrix(0, nrow = ncol(fullClusters)-1, ncol = reps)
  for (r in 2:ncol(fullClusters)) {
    for (rr in 1:reps) {
      purity[r-1, rr] <- reduced_clusters_incidence_matrix(fullClusters, r) %>%
        get_reduced_clusters(fullClusters, .) %>%
        compute_purity
    }
  }
  return(purity)
}

estimate_number_of_clusters_from_purity_matrix <- function(purityMatrix, purityThr) {
  1 + Position(function(x) {!is.na(x) & x > purityThr},
               purityMatrix %>% apply(1, mean),
               right = TRUE,
               nomatch = 0)
}

# all_words_to_one_label_ <- function(memory) {
#   data.table(word = memory[valid == TRUE, word] %>% unique,
#              label = factor(1))
# }

collapsed_incidence_matrix_ <- function(fullClusters) {
  matrix(TRUE, nrow = 1, ncol = ncol(fullClusters))
}

estimate_raw_clusters <- function(agent, params) {
  Mclust(as.matrix(agent$features)[agent$memory$valid, , drop = FALSE])
}

get_full_word_clusters <- function(mclust.obj, agent, params) {
  agent$memory[valid == TRUE, .(word, cluster = mclust.obj$classification)] %>% table %>% unclass
}

get_reduced_clusters <- function(fullClusters, incidenceMatrix) {
  fullClusters %*% t(incidenceMatrix)
}

estimate_reduced_clusters_incidence_matrix <- function(fullClusters, purityRepetitions, purityThreshold) {
  if (ncol(fullClusters) == 1) {
    return(collapsed_incidence_matrix_(fullClusters))
  }
  purityMat <- compute_purity_matrix(fullClusters, purityRepetitions)
  nClusters <- estimate_number_of_clusters_from_purity_matrix(purityMat, purityThreshold)
  if (nClusters == 1) {
    return(collapsed_incidence_matrix_(fullClusters))
  }
  while({incidenceMatrix <- reduced_clusters_incidence_matrix(fullClusters, nClusters);
  get_reduced_clusters(fullClusters, incidenceMatrix) %>% compute_purity %>% is.na}) {}
  return(incidenceMatrix)
}

# assign_words_to_labels <- function(agent, params) {
#   if (ncol(agent$features) == 1) {modelNames <- "X"} else {modelNames <- "XXX"}
#   agent$memory[valid == TRUE,
#                cluster := Mclust(as.matrix(agent$features)[agent$memory$valid, , drop = FALSE],
#                                  modelNames = modelNames)$classification]
#   fullWordCluster <- agent$memory[valid == TRUE, .(word, cluster)] %>% table %>% unclass
#   agent$memory[, cluster := NULL]
#   if (ncol(fullWordCluster) == 1) {
#     return(all_words_to_one_label_(agent$memory))
#   }
#   purityMat <- compute_purity_matrix(fullWordCluster, params[['purityRepetitions']])
#   nLabels <- estimate_number_of_labels_from_purity_matrix(purityMat, params[['purityThreshold']])
#   if (nLabels == 1) {
#     return(all_words_to_one_label_(agent$memory))
#   }
#   while({reducedWordCluster <- reduced_word_clusters(fullWordCluster, nLabels);
#   is.na(reducedWordCluster %>% compute_purity)}) {}
#   return(data.table(word = reducedWordCluster %>% rownames,
#                     label = reducedWordCluster %>% apply(1, which.max) %>% factor))
# }

assign_words_to_labels <- function(wordClusters) {
  data.table(word = wordClusters %>% rownames,
             label = wordClusters %>% apply(1, which.max) %>% factor)
}

# build_GMM <- function(rawClusters, incidenceMatrix) {
#   if (ncol(incidenceMatrix) != rawClusters$G) {stop("build_GMM: ncol incidenceMatrix should be == rawClusters$G")}
#   nClasses <- nrow(incidenceMatrix)
#   clusters <- list()
#   clusters$n <- rawClusters$n
#   clusters$d <- rawClusters$d
#   clusters$prop <- (incidenceMatrix %*% rawClusters$parameters$pro) %>% as.vector
#   names(clusters$prop) <- seq_len(nClasses) %>% as.character
#   clusters$models <- list()
#   for (cl in seq_len(nClasses)) {
#     clusters$models[[as.character(cl)]] <- list(
#       n = (rawClusters$classification %in% which(incidenceMatrix[cl,])) %>% sum,
#       d = rawClusters$d,
#       G = incidenceMatrix[cl,] %>% sum,
#       modelName = "VVV",
#       parameters = list(
#         pro = rawClusters$parameters$pro[incidenceMatrix[cl,]] / clusters$prop[cl],
#         mean = rawClusters$parameters$mean[, incidenceMatrix[cl,], drop = FALSE],
#         variance = list(
#           modelName = "VVV",
#           d = rawClusters$d,
#           G = incidenceMatrix[cl,] %>% sum,
#           sigma = rawClusters$parameters$variance$sigma[,,incidenceMatrix[cl,], drop = FALSE]
#         )
#       )
#     )
#   }
#   return(structure(clusters, class = "MclustDA"))
# }

map_classes_to_incidence_matrix <- function(classification, incidenceMatrix) {
  incidenceVector <- apply(incidenceMatrix, 2, which) %>%
    sapply(function(x) {if(length(x) == 0) NA else x})
  aggregatedClasses <- incidenceVector[classification]
  names(aggregatedClasses) <- NULL
  return(aggregatedClasses)
}

reestimate_GMM <- function(rawGMM, incidenceMatrix) {
  aggregatedClasses <- map_classes_to_incidence_matrix(rawGMM$classification, incidenceMatrix)
  G <- apply(incidenceMatrix, 1, sum) %>% sapply(list)
  
  GMM <- MclustDA(data = rawGMM$data[!is.na(aggregatedClasses) ,],
                  class = aggregatedClasses %>% na.exclude,
                  G = G)
  # MclustDA bug: when only one class, G is ignored and set to 1.
  return(GMM)
}

estimate_GMM <- function(agent, params) {
  write_log(agent$speaker, agent, params)
  rawGMM <- estimate_raw_clusters(agent, params)
  fullWordClusters <- get_full_word_clusters(rawGMM, agent, params)
  reducedWordClustersIncidenceMatrix <- estimate_reduced_clusters_incidence_matrix(
    fullWordClusters, params[['purityRepetitions']], params[['purityThreshold']])
  reducedWordClusters <- get_reduced_clusters(fullWordClusters, reducedWordClustersIncidenceMatrix)
  
  # take care of classes with too few tokens
  excludedClassIdx <- reducedWordClusters %>% apply(2, sum) %>% `<`(rawGMM$d) %>% which
  if (length(excludedClassIdx) > 0) {
    write_log(paste("excludedClassIdx", excludedClassIdx),  agent, params)
    reducedWordClustersIncidenceMatrix <- reducedWordClustersIncidenceMatrix[-excludedClassIdx, , drop = FALSE]
    reducedWordClusters <- reducedWordClusters[, -excludedClassIdx, drop = FALSE]
    excludedTokenIdx <- map_classes_to_incidence_matrix(rawGMM$classification, reducedWordClustersIncidenceMatrix) %>%
      is.na %>% which
    agent$memory[which(agent$memory$valid)[excludedTokenIdx], valid := FALSE]
    write_log(paste("excludedTokenIdx", excludedTokenIdx), agent, params)
  }
  tryCatch({
  GMM <- reestimate_GMM(rawGMM, reducedWordClustersIncidenceMatrix)
  }, error = function(c) {
    write_log(paste(conditionMessage(c), conditionCall(c), sep = "\n"), agent, params)
    dump_obj(rawGMM, "rawGMM", agent, params)
    dump_obj(reducedWordClustersIncidenceMatrix, "reducedWordClustersIncidenceMatrix", agent, params)
    stop(c)
  })
  set_cache_value(agent, "GMM", GMM)
  
  wordLabels <- assign_words_to_labels(reducedWordClusters)
  agent$memory[wordLabels, on = "word", label := i.label]
}
  
# estimate_GMM <- function(agent, params) {
#   agent$memory[assign_words_to_labels(agent, params), on = "word", label := i.label]
#   if (ncol(agent$features) == 1) {modelNames <- c("X", "V")} else {modelNames <- c("XXX", "VVV")}
#   GMM <- MclustDA(data = as.matrix(agent$features)[agent$memory$valid, , drop = FALSE],
#                   class = agent$memory$label[agent$memory$valid],
#                   modelNames = modelNames)
#   set_cache_value(agent, "GMM", GMM)
# }
