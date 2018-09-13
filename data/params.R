################################################################################
#                                                                              #
# Please set all obligatory parameters below! You will find possible options   #
# for each value in the corresponding commentaries. If you need more detailed  #
# explanations, please refer to param_expl/param_explanations.pdf. If you are  #
# operating on Ubuntu and use okular as a pdf reader, you can open that file   #
# by typing the following command in your R console:                           #
#                                                                              #
# system(paste0('okular param_expl/param_explanations.pdf'))                   #
#                                                                              #
# If you are an advanced user, you can also go through the expert options and  #
# see, if you want to change any of them. Again, a thorough explanation of     #
# each parameter can be found in param_expl/param_explanations.pdf.            #
#                                                                              #
# After having set all necessary parameters, save this script (Strg + s, or    #
# Ctrl + s), and then type the following command in you R console in order     #
# to initiate the ABM:                                                         #
#                                                                              #
# source("Rcmd/master.R")                                                      #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


########################## Obligatory Options ##########################

##### Input dataframe
inputDataFile <- "/vdata/Projects/ABM/data/andalusian/pre-aspiration_clean.df"

##### Strategies

memoryIntakeStrategy <- "maxPosteriorProb"  # ... or "mahalanobisDistance"
memoryRemovalStrategy <- "timeDecay"        # ... or "outlierRemoval"
# maxMemorySize <- 18                        # ... or any full positive number, e.g. 500 (without quotes!).
maxMemoryExpansion <- 1.0

splitAndMerge <- FALSE                       # ... or FALSE
productionStrategy <- "SMOTE"           # "meanWords", "extraTokens", "MAP", "SMOTE"
productionExtraTokensRatio <- 1.0         # used if productionStrategy == "extraTokens"
productionMAPPriorAdaptRatio <- 1.0       # used if productionStrategy == "MAP"
productionMinTokens <- 20                 # used if productionStrategy == "SMOTE"
productionSMOTENN <- 5                    # used if productionStrategy == "SMOTE"

perceptionOOVNN <- 5

##### ABM Options

runMode <- "multiple"                         # ... "single" or "multiple"
nrOfSimulations <- 6                       # ... or any full positive number, e.g. 20
interactionsPerSimulation <- 5000           # ... or any full positive number, e.g. 125

##### Plotting Options

columnsToPlot <- c("P1", "P2")              # ... or any other combination of two P-columns of input.df
plotting <- "points"                        # ... or "tracks" or "formants" or "none"
plotGroupBy <- "speaker"                    # ... or "group"
plotColor <- "equivalence"                  # ... or "initial"
plotBeforeABM <- FALSE                       # or FALSE
plotAfterABM <- FALSE                        # or FALSE


############################ Expert Options ############################

##### General Options

logDir_ <- paste(ABMpath, "logDir", sep = "/")
dateTime <- format(Sys.time(), "%Y%m%d%H%M%S")
logDirDate <- paste(logDir_, dateTime, sep = "/")
img <- paste(logDirDate, "img", sep = "/")

##### ABM Options

splitAndMergeInterval <- 100                # ... or any full positive number, e.g. 75
doSplitAndMergeBeforeABM <- TRUE            # ... or FALSE
mahalanobisThreshold <- 1.5                 # ... or any other full positive number, e.g. 5.0
multipleABMRuns <- 3                      # ... or any full positive number, e.g. 20
subsetSpeakers <- NULL                      # ... or a vector of strings, e.g. c("spk01", "spk02", "spk03")
subsetSegments <- NULL                      # ... or a vector of strings, e.g. c("a", "i", "u", "o")
speakerProb <- NULL                         # ... or a vector of numerics, see param_explanations.pdf
listenerProb <- NULL                        # ... or a vector of numerics, see param_explanations.pdf
interactionPartners <- "betweenGroups"             # ... or "betweenGroups" or "withinGroups"

##### Plotting Options

plotSpeaker <- ""                           # ... or a string, e.g. "spk01"
plotSegment <- ""                           # ... or a string, e.g. "a"

##### Debug options

debugMode <- FALSE
seed <- 398