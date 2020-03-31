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
require("data.table")
require("plyr")
require("dplyr")
require("MASS")
require("emuR")
require("mvtnorm")
require("ggplot2")
require("RColorBrewer")
require("cluster")
require("matrixcalc")
require("smotefamily")
require("FNN")
require("tools")
library("Hmisc")
library("magrittr")
library("rlist")
library("dtt")
library("mclust")
library("abind")
library("fda")

source("~/Programs/FPCA-phonetics-workshop-master/scripts/header.R")
# source("C:/Users/Michele/Dropbox/scambio_temp/work/FDA/FPCA-phonetics-workshop-master/scripts/header.R")

source(file.path(ABMpath, "functions/interactions.R"))
source(file.path(ABMpath, "functions/calculations.R"))
source(file.path(ABMpath, "functions/simulations.R"))
source(file.path(ABMpath, "functions/splitandmerge.R"))
source(file.path(ABMpath, "functions/debugging.R"))

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
  
  params <- check_params(params)
  
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

