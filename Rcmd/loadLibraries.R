################################################################################
#                                                                              #
# In this script, all packages and functions that will be needed throughout    # 
# the ABM are loaded. It is sourced in master.R.                               #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

Sys.setlocale("LC_COLLATE", "C")

library(data.table)
library(plyr)
library(dplyr)
library(MASS)
library(emuR)
library(mvtnorm)
library(ggplot2)
library(RColorBrewer)
library(cluster)
library(matrixcalc)
library(smotefamily)
library(FNN)
library(tools)
library(Hmisc)
library(magrittr)
library(rlist)
library(dtt)
library(mclust)
library(parallel)
library(abind)
library(fda)

source("functions/interactions.R")
source("functions/calculations.R")
source("functions/simulations.R")
source("functions/splitandmerge.R")
source("functions/debugging.R")
source("functions/pca.fd.R")
source("functions/defint.fd.R")


methodReg <- rbindlist(list(
  data.table(
    method = "identity",
    compute_features = exemplar2matrix,
    exemplar2features = exemplar2matrix,
    features2exemplar = rowMatrix2exemplar,
    memoryIntakeStrategy = list(list(
      acceptAll = accept_all,
      mahalanobisDistance = mahalanobis_distance,
      maxPosteriorProb = max_posterior_prob,
      posteriorProbThr = posterior_prob_thr
    )),
    cacheEntries = list(NA_character_)
  ),
  data.table(
    method = "FPCA",
    compute_features = compute_fpca,
    exemplar2features = exemplar2FPCscores,
    features2exemplar = FPCscores2exemplar,
    memoryIntakeStrategy = list(list(
      acceptAll = accept_all,
      mahalanobisDistance = mahalanobis_distance,
      maxPosteriorProb = max_posterior_prob,
      posteriorProbThr = posterior_prob_thr,
      MSEthreshold = MSE_threshold
    )),
    cacheEntries = list(c("FPCA", "MSE"))
  )
)) %>% setkey(method)


coreABM <- function(input.df, params, logDir) {
  # This function runs the main ABM routine. Assumes that params and input.df have been loaded.
  # Function call in ABMmain.R.
  #
  # Args:
  #    - logDir: path to logDir
  #
  # Returns:
  #    - nothing.
  #
  
  pop <- create_population(input.df = input.df, params = params)
  
  if (params[['splitAndMerge']] == TRUE & params[['doSplitAndMergeBeforeABM']] == TRUE) {
    for (j in seq_along(pop)) {
      splitandmerge(pop[[j]], params, full = TRUE)
    }
  }
  save_population(pop,
                 extraCols = list(snapshot = 0),
                 logDir = logDir)
  
  perform_interactions(pop, logDir, params)
}

