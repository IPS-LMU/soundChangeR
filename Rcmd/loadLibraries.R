################################################################################
#                                                                              #
# In this script, all packages and functions that will be needed throughout    # 
# the ABM are loaded. It is sourced in master.R.                               #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
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


source(file.path(ABMpath, "functions/interactions.R"))
source(file.path(ABMpath, "functions/calculations.R"))
source(file.path(ABMpath, "functions/splitandmerge.R"))


coreABM <- function(logDir) {
  ### main ABM routine
  # Assumes params.R and loadLibraries.R sourced, input.df loaded
  # Only a convenience function to avoid code duplication
  # for single and multiple run modes (parallel) in ABMmain.R
  
  pop <- create_population(input.df = input.df)
  
  if (params[['splitAndMerge']] == TRUE & params[['doSplitAndMergeBeforeABM']] == TRUE) {
    for (j in seq_along(pop)) {
      splitandmerge(pop[[j]], TRUE)
    }
  }
  savePopulation(pop,
                 extraCols = list(condition = 0),
                 logDir = logDir)
  
  interactionsLog <- perform_interactions(pop, logDir)
  saveInteractionsLog(interactionsLog, logDir = logDir)
}

