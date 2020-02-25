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

source(file.path(ABMpath, "functions/interactions.R"))
source(file.path(ABMpath, "functions/calculations.R"))
source(file.path(ABMpath, "functions/simulations.R"))
source(file.path(ABMpath, "functions/splitandmerge.R"))
source(file.path(ABMpath, "functions/debugging.R"))

methodReg <- data.table(
  method = c("identity"),
  compute_features = c(unpack_vector),
  exemplar2features = c(unpack_vector),
  features2exemplar = c(pack_vector),
  cacheEntries = NA_character_
) %>% setkey(method)


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

