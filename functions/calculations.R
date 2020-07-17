################################################################################
#                                                                              #
# This script contains functions that perform calculations on the data used    #
# or produced in the simulations.                                              #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

is_cache_valid <- function(agent, cacheName) {
  agent$cache[name == cacheName, valid]
}

update_cache <- function(agent, cacheName, method, ...) {
  agent$cache[name == cacheName, `:=`(value = list(method(agent, ...)), valid = TRUE)]
}

set_cache_value <- function(agent, cacheName, cacheValue) {
  agent$cache[name == cacheName, `:=`(value = list(cacheValue), valid = TRUE)]
}

get_cache_value <- function(agent, cacheName) {
  agent$cache[name == cacheName, value][[1]]
}

invalidate_cache <- function(agent, cacheName) {
  agent$cache[name == cacheName, valid := FALSE]
}

compute_qda <- function(agent) {
  # execute the QDA (Quadrativ Discriminant Analysis)
  # a wrapper for qda()
  qda(as.matrix(agent$features)[agent$memory$valid == TRUE, , drop = FALSE],
      grouping = agent$memory$label[agent$memory$valid == TRUE])
}

compute_posterior_probabilities <- function(agent, features, method) {
  if (method == "qda") {
    if (!is_cache_valid(agent, "qda")) {
      update_cache(agent, "qda", compute_qda)
      # cacheMissCounter <<- cacheMissCounter + 1
    }
    predict(get_cache_value(agent, "qda"), features)$posterior
  } else if (method == "GMM") {
    # not checking validity of cache entry for GMM, I assume it's valid
    predict(get_cache_value(agent, "GMM"), features)$z[1, , drop=FALSE]
  } else {
    NULL
  }
}

recognize_posterior_probabilities <- function(posteriorProb, label, method, ...) {
  if (method == "maxPosteriorProb") {
    colnames(posteriorProb)[which.max(posteriorProb)] == label
  } else if (method == "posteriorProbThr") {
    posteriorProb[, label] >= list(...)[["posteriorProbThr"]]
  }
}

compute_mahal_distance <- function(agent, features, label, method = NULL) {
  if (is.null(method) | method == "singleGaussian") {
    # a wrapper to mahalanobis()
    mahalanobis(features,
                apply(as.matrix(agent$features)[agent$memory$valid == TRUE & 
                                                      agent$memory$label == label, , drop = FALSE], 2, mean),
                cov(as.matrix(agent$features)[agent$memory$valid == TRUE & 
                                                agent$memory$label == label, , drop = FALSE]))
    
  } else if (grepl("^GMM(s)?", method)) {
    GMM <- get_cache_value(agent, "GMM") # check if valid?
    # return the min of Mahal dist to each GMM component
    # in case only one component, return the Mahal dist to that component (handled implicitly)
    if (GMM$d == 1) { # case 1D features
      map2(GMM$models[[label]]$parameters$mean,
           GMM$models[[label]]$parameters$variance$sigmasq, 
           ~ mahalanobis(features, .x, .y)) %>% unlist %>% min
    } else { # case multi-D features
      map2(lapply(1:GMM$models[[label]]$G, function(g) {GMM$models[[label]]$parameters$mean[,g] %>% t}),
           lapply(1:GMM$models[[label]]$G, function(g) {GMM$models[[label]]$parameters$variance$sigma[,,g]}),
           ~ mahalanobis(features, .x, .y)) %>% unlist %>% min
    }  
  }
}

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
    # the next four lines join the 'feature' columns, the 'memory' columns, 
    # and the 'initial' columns, for the current agent (pop[[i]])
    cbind(pop[[i]]$features, pop[[i]]$memory) %>%
      .[valid == TRUE] %>%
      inner_join(pop[[i]]$initial, by = 'word') %>%
      setDT %>%
      # the next four lines set agentID, speaker, group, equivalence for the current agent
      .[, `:=`(agentID = pop[[i]]$agentID,
               speaker = pop[[i]]$speaker,
               group = pop[[i]]$group)] %>%
      .[, equivalence := equal_class(initial, label)]
  }), fill = TRUE) %>% {
    # add every item from extraCols as a column to new pop data.table
    for (col in names(extraCols)) {
      .[, (col) := extraCols[[col]]]
    }
    .[]
  }
}

knearest_fallback <- function(points, extendedIndices, targetIndices, K) {
  # This is an auxiliary function mainly used by SMOTE.
  # SMOTE needs to identify K nearest neighbours each time it produces an extra token. 
  # That means it needs minimum K+1 tokens being available in total, i.e. one target + K neighbours.
  # 'points' is a large set of tokens (rows), typically belonging to one phoneme,
  # from which targetIndices identify the tokens that should be used by SMOTE, typically belonging to a word.
  # If the number of targetIndices < K, additional fallback tokens (rows) are selected from 'points'.
  # These additional tokens are chosen amongst the K + 1 nearest neighbours of points[targetIndices]. 
  
  # Function call in interactions.R, produce_token().
  #
  # Args:
  #    - points: matrix of feature values
  #    - targetIndices: list of indices
  #    - K: number of nearest neighbors
  #
  # Returns:
  #    - list of targetIndices and fallback:
  #
  
  
  
  if (any(c(targetIndices, extendedIndices) < 0)) {
    stop("knearest_fallback: negative index notation not supported for targetIndices and extendedIndices")
  }
  if (nrow(points) == 0) {
    # print to LOG 
    return (NULL)
  }
  if (K <= 0) {
    stop(paste("knearest_fallback: invalid number of nearest neighbours requested: K =", K))
  }
  # Stop if extendedIndices are not in points rows indices
  # note: all(NULL %in% something) == TRUE, hence does not stop when extendedIndices == NULL
  if (!all(extendedIndices %in% seq_len(nrow(points)))) {
   
    stop("knearest_fallback: extendedIndices out of bound")
  }
  if (length(targetIndices) <= 0) {
    stop("knearest_fallback: empty targetIndices")
  }
  # when no extendedIndices provided, just return targetIndices
  if (is.null(extendedIndices) | length(extendedIndices) == 0) {
    return(targetIndices)
  }
  # Stop if targetIndices are not part of (non-empty) extendedIndices 
  if (!all(targetIndices %in% extendedIndices)) {
    stop("knearest_fallback: targetIndices out of bound")
  }
  # Fringe case: return extendedIndices when K + 1 > length(extendedIndices)
  if (K + 1 > length(extendedIndices)) {
    # print to LOG
    return(extendedIndices)
  }
  
  # compute number of tokens to be sampled
  nFallback <- K + 1 - length(targetIndices)
  
  # if it turns out that there are enough tokens in targetIndices, return them
  if (nFallback <= 0) {
    return(targetIndices)
  }
  
  # apply a K nearest neighbor algorithm on the points,
  # and sample as many as nFallback tokens from the unique neighbors 
  # that are not equal to the targetIndices
  fallbackIndices <- knnx.index(points[extendedIndices, , drop=FALSE], points[targetIndices, , drop=FALSE], K + 1) %>%
    as.vector %>%
    extendedIndices[.] %>%
    setdiff(targetIndices) %>%
    sample(nFallback)
    
  return(c(targetIndices, fallbackIndices))
}

equal_class <- function(orig, derived) {
  # This function generates equivalence labels from the initial and 
  # developed labels of the agents in the population.
  # Function call in calculations.R, convert_pop_list_to_dt().
  #
  # Args:
  #    - orig: a vector of characters from pop[[i]]$initial$initial
  #    - derived: a vector of characters from pop[[i]]$memory$label
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

one_obj2exemplar <- function(obj) {
  list(obj)
}

one_exemplar2obj <- function(exemplar) {
  exemplar[[1]]
}


exemplar2matrix <- function(exemplars, ...) {
  do.call(rbind, exemplars) 
}

rowMatrix2exemplar <- function(features, ...) {
  list(as.numeric(features))
}

matrix2exemplar <- function(mat) {
  apply(mat, 1, rowMatrix2exemplar) %>% unlist(recursive = FALSE) 
}

# FPCA
all_fd2exemplar <- function(fdObj) {
  # Do not use to convert the fd of one curve, use one_fd2exemplar instead
  # convert an fd object into a list
  # used to build input.df exemlar column:
  # input.df[, exemplar := all_fd2exemplar(my_fd)]
  nItems <- fdObj$coefs %>% dim %>% .[2]
  sapply(seq_len(nItems), function(i) {fdObj[i]}, simplify = FALSE)
}

one_fd2exemplar <- one_obj2exemplar

all_exemplar2fd <- function(exemplars) {
  # Do not use to convert one single exemplar to fd, use one_exemplar2fd instead
  dim2 <- exemplars[[1]][["coefs"]] %>% dim %>% .[2]
  if (dim2 == 1) {
    coefs <- abind(lapply(exemplars, `[[`, "coefs"), along = 2)
    # coefs <- sapply(exemplars, `[[`, "coefs", simplify = TRUE)
  } else {
    coefs <- abind(lapply(exemplars, `[[`, "coefs"), along = 3) %>% aperm(c(1,3,2)) 
  }
  fd(coef = coefs, basisobj = exemplars[[1]][["basis"]])
}

one_exemplar2fd <- one_exemplar2obj

compute_fpca <- function(exemplars, agent, params) {
  fdObj <- all_exemplar2fd(exemplars)
  nDim <- ifelse(is.matrix(fdObj$coefs), 1, fdObj$coefs %>% dim %>% .[3])
  pcafdPar  <- fdPar(fdObj$basis, 2, params[["lambdaFPCA"]])
  fpcaObj <- pca.fd(fdobj = fdObj, nharm = fdObj$basis$nbasis * nDim, harmfdPar = pcafdPar)
  nPC <- Position(function(x) {x >= params[["varCutoffFPCA"]]}, cumsum(fpcaObj$varprop))
  MSE <- sapply(1:length(exemplars), function(i){
    fdMSE(
      FPCscores2fd(fpcaObj$scores[i, 1:nPC], fpcaObj),
      fdObj[i]
    )
  }, simplify = TRUE)
  set_cache_value(agent, "FPCA", fpcaObj)
  # set_cache_value(agent, "nPC", nPC)
  set_cache_value(agent, "MSE", MSE)
  fpcaObj$scores[, 1:nPC, drop = FALSE]
}

exemplar2FPCscores <- function(exemplar, agent, params) {
  matrix(
    compute_FPCscores(one_exemplar2fd(exemplar),
                      get_cache_value(agent, "FPCA"),
                      get_cache_value(agent, "nFeatures")),
    nrow = 1
  )
}

compute_FPCscores <- function(fdObj, fpcaObj, nPC) {
  if (is.matrix(fpcaObj$meanfd$coefs)) { # 1D curve
    inprod(fdObj - fpcaObj$meanfd, fpcaObj$harmonics[1:nPC]) %>% as.numeric
  } else { # multi-D curve
    nDim <- dim(fpcaObj$meanfd$coefs)[3]
    sapply(1:nPC, function(pc) {
      sapply(1:nDim, function(dimInd) {
        inprod(fd(coef = fdObj$coefs[, dimInd] - fpcaObj$meanfd$coefs[, 1, dimInd], basisobj = fdObj$basis),
               fpcaObj$harmonics[pc,dimInd])
      }) %>% sum
    }, simplify = TRUE)
  }
}

FPCscores2exemplar <- function(features, agent, params) {
  one_fd2exemplar(
    FPCscores2fd(features, get_cache_value(agent, "FPCA"))
  )
}

FPCscores2fd <- function(scores, fpcaObj) {
  if (is.matrix(fpcaObj$meanfd$coefs)) { # 1D curve
    coefs <- fpcaObj$meanfd$coefs + 
      (sapply(seq_along(scores), function(pc) {
        scores[pc] * fpcaObj$harmonics$coefs[, pc]
        }, simplify = TRUE)
       ) %>%
      apply(1, sum)
  } else { # multi-D curve
    nDim <- dim(fpcaObj$meanfd$coefs)[3]
    coefs <- do.call(cbind, lapply(1:nDim, function(dimInd) {
      fpcaObj$meanfd$coefs[,1,dimInd] + 
      (sapply(seq_along(scores), function(pc) {
        scores[pc] * fpcaObj$harmonics$coefs[, pc, dimInd]
      }, simplify = TRUE)
      ) %>%
      apply(1, sum)
    }))
  }
  fd(coefs, fpcaObj$meanfd$basis)
}

fdMSE <- function(fd1, fd2) {
  apply(fd1$coefs - fd2$coefs, -1L, function(x) {
    defint.fd(exponentiate.fd(fd(x, fd1$basis), 2))
  }) %>% sum %>% `/`(diff(fd1$basis$rangeval))
}

# memoryIntakeStrategy

accept_all <- function(exemplar, features, label, agent, params) {
  return(TRUE)
}

mahalanobis_distance <- function(exemplar, features, label, agent, params) {
  mahalDist <- compute_mahal_distance(agent, features, label, params[["perceptionModels"]])
  mahalDist <= qchisq(p = params[["mahalanobisProbThreshold"]], df = get_cache_value(agent, "nFeatures"))
}

max_posterior_prob <- function(exemplar, features, label, agent, params) {
  posteriorProb <- compute_posterior_probabilities(agent, features, params[["posteriorProbMethod"]])
  recognize_posterior_probabilities(posteriorProb, label, "maxPosteriorProb")
}

posterior_prob_thr <- function(exemplar, features, label, agent, params) {
  posteriorProb <- compute_posterior_probabilities(agent, features, params[["posteriorProbMethod"]])
  recognize_posterior_probabilities(posteriorProb, label, "posteriorProbThr", posteriorProbThr = params[["posteriorProbThr"]])
}

MSE_threshold  <- function(exemplar, features, label, agent, params) {
    if (is.numeric(params[["MSEthresholdMaxCoef"]])) {
      return(
        fdMSE(one_exemplar2fd(exemplar), FPCscores2fd(features, get_cache_value(agent, "FPCA"))) <=
          params[["MSEthresholdMaxCoef"]] * max(get_cache_value(agent, "MSE"))
      )
    } else if (is.numeric(params[["MSEthresholdQuantile"]])) {
      return(
        fdMSE(one_exemplar2fd(exemplar), FPCscores2fd(features, get_cache_value(agent, "FPCA"))) <=
          quantile(get_cache_value(agent, "MSE"), probs = params[["MSEthresholdQuantile"]])
      )
    }
  return(TRUE)
}
