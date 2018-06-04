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
require("plyr")
require("dplyr")
require("MASS")
require("emuR")
require("mvtnorm")
require("ggplot2")
require("RColorBrewer")
require("cluster")

source(file.path(path, "functions/interactions.R"))
source(file.path(path, "functions/calculations.R"))
source(file.path(path, "functions/splitandmerge.R"))
# plotting.R is sourced in coreABM.R

