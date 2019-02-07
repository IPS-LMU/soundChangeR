################################################################################
#                                                                              #
# Parameter settings                                                           #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


params = list(
  
  inputDataFile = "",
  
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
  interactionsPerSnapshot = 1000,           # ... or any full positive number, e.g. 125
  multipleABMRuns = 100,                      # ... or any full positive number, e.g. 20
  

  ##### Other options
  
  splitAndMergeInterval = 100,                # ... or any full positive number, e.g. 75
  doSplitAndMergeBeforeABM = FALSE            
  # mahalanobisThreshold = 1.5,                 # ... or any other full positive number, e.g. 5.0
  # subsetSpeakers = NULL,                      # ... or a vector of strings, e.g. c("spk01", "spk02", "spk03")
  # subsetSegments = NULL,                      # ... or a vector of strings, e.g. c("a", "i", "u", "o")
  
)
