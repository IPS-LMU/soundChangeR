################################################################################
#                                                                              #
# Please set all obligatory parameters below! You will find possible options   #
# for each value in the corresponding commentaries. If you need more detailed  #
# explanations, please refer to param_expl/param_explanations.pdf. If you are  #
# operating on UNIX and use okular as a pdf reader, you can open that file     #
# by typing the following command in your R console:                           #
#                                                                              #
# system(paste0('okular param_expl/param_explanations.pdf'))                   #
#                                                                              #
# If you are an advanced user, you can also go through the expert options and  #
# see, if you want to change any of them. Again, a thorough explanation of     #
# each parameter can be found in param_expl/param_explanations.pdf.            #
#                                                                              #
# After having set all necessary parameters, save this script (Strg + s, or    #
# Ctrl + s), and then type the following command in your R console in order    #
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

##### Strategies

memoryIntakeStrategy <- "maxPosteriorProb"  # ... or "mahalanobisDistance"
memoryRemovalStrategy <- "timeDecay"        # ... or "outlierRemoval"
memorySizeStrategy <- "maxAgent"            # ... or any full positive number, e.g. 50 (without quotes!).
splitAndMerge <- FALSE                      # ... or TRUE

##### ABM Options

runMode <- "single"                         # ... or "multiple"
nrOfSimulations <- 20                       # ... or any full positive number, e.g. 50
interactionsPerSimulation <- 1000           # ... or any full positive number, e.g. 100

##### Plotting Options

plotting <- "points"                        # ... or "formants" or "tracks" or "none"
columnsToPlot <- c("P1", "P2")              # ... or any other combination of two P-columns of input.df
plotGroupBy <- "group"                      # ... or "speaker"
plotColor <- "initial"                      # ... or "equivalence"
plotBeforeABM <- TRUE                       # ... or FALSE
plotAfterABM <- TRUE                        # ... or FALSE


############################ Expert Options ############################

##### General Options

logDir <- paste(path, "logDir", sep = "/")
dateTime <- format(Sys.time(), "%Y%m%d%H%M%S")
logDirDate <- paste(logDir, dateTime, sep = "/")
img <- paste(logDirDate, "img", sep = "/")

##### ABM Options

splitAndMergeInterval <- 100                # ... or any full positive number, e.g. 75
doSplitAndMergeBeforeABM <- FALSE           # ... or TRUE
mahalanobisThreshold <- 1.5                 # ... or any other full positive number, e.g. 5.0
multipleABMRuns <- 100                      # ... or any full positive number, e.g. 20
subsetSpeakers <- NULL                      # ... or a vector of strings, e.g. c("spk01", "spk02", "spk03")
subsetSegments <- NULL                      # ... or a vector of strings, e.g. c("a", "i", "u", "o")
speakerProb <- NULL                         # ... or a vector of numerics, see param_explanations.pdf
listenerProb <- NULL                        # ... or a vector of numerics, see param_explanations.pdf
interactionPartners <- "random"             # ... or "betweenGroups" or "withinGroups"

##### Plotting Options

plotSpeaker <- ""                           # ... or a string, e.g. "spk01"
plotSegment <- ""                           # ... or a string, e.g. "a"

