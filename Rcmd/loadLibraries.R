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

source("functions/interactions.R")
source("functions/calculations.R")
source("functions/simulations.R")
source("functions/splitandmerge.R")
source("functions/debugging.R")

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
                 extraCols = list(condition = 0),
                 logDir = logDir)
  
  interactionsLog <- perform_interactions(pop, logDir, params)
  save_interactions_log(interactionsLog, logDir = logDir)
}

