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


params = list(
  
  inputDataFile = "data/Antarctica.csv",
  
  ##### Production
  
  productionStrategy = "SMOTE",           # "targetWordTokens", "meanWords", "extraTokens", "MAP", "SMOTE"
  productionMinTokens = 20,                 # used if productionStrategy == "SMOTE"
  productionSMOTENN = 5,                    # used if productionStrategy == "SMOTE"
  
  ##### Perception
  
  memoryIntakeStrategy = "maxPosteriorProb",  # ... or "mahalanobisDistance"
  memoryRemovalStrategy = "outlierRemoval",        # ... or "timeDecay"
  maxMemoryExpansion = 1.0,
  
  splitAndMerge = FALSE, 
  perceptionOOVNN = 5,                      # number of nearest neighbours used 
                                            # to attribute label in case of unknown word
  
  ##### Interaction
  
  interactionPartners = "random",             # ... or "betweenGroups" or "withinGroups"
  speakerProb = NULL,                         # ... or a vector of numerics
  listenerProb = NULL,                        # ... or a vector of numerics
  
  
  ##### Runs
  
  runMode = "multiple",                         # ... "single" or "multiple"
  nrOfSnapshots = 5,                       # ... or any full positive number, e.g. 20
  interactionsPerSnapshot = 100,           # ... or any full positive number, e.g. 125
  multipleABMRuns = 2,                      # ... or any full positive number, e.g. 20
  

  ##### Other options
  
  splitAndMergeInterval = 100,                # ... or any full positive number, e.g. 75
  doSplitAndMergeBeforeABM = FALSE            
  # mahalanobisThreshold = 1.5,                 # ... or any other full positive number, e.g. 5.0
  # subsetSpeakers = NULL,                      # ... or a vector of strings, e.g. c("spk01", "spk02", "spk03")
  # subsetSegments = NULL,                      # ... or a vector of strings, e.g. c("a", "i", "u", "o")
  
)
