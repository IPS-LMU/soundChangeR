################################################################################
#                                                                              #
# Parameter settings. Further documentation in GIT REPO                        #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


params = list(
  
  ##### Input data
  
  inputDataFile = "./data/asp.df.txt",        # absolute or relative path to input data
  features = Cs(pre, post),                   # columns in inputDataFile that are used as features
  group = Cs(Age),                            # the column in inputDataFile that defines the agents' groups
  label = quote(P),                           # the column in inputDataFile that stores the phonological labels; these labels will be changed during the interactions
  initial = quote(P),                         # the column in inputDataFile that stores the phonological labels; these labels will remain unchanged
  word = Cs(W),                               # the column in inputDataFile that stores the word labels
  speaker = Cs(Vpn),                          # the column in inputDataFile that stores the speakers' IDs or names
  subsetSpeakers = NULL,                      # NULL or a vector of strings, e.g. c("spk01", "spk02", "spk03")
  subsetLabels = NULL,                        # NULL or a vector of strings, e.g. c("a", "i", "u", "o")
  
  ##### Production
  
  productionStrategy = "meanWords",           # "targetWordTokens" or "meanWords" or "extraTokens" or "MAP" or "SMOTE"
  productionMinTokens = 20,                   # used only if productionStrategy == "SMOTE"
  productionSMOTENN = 5,                      # used only if productionStrategy == "SMOTE"
  
  ##### Perception
  
  memoryIntakeStrategy = "maxPosteriorProb",  # "maxPosteriorProb" or "mahalanobisDistance"
  memoryRemovalStrategy = "outlierRemoval",   # "outlierRemoval" or "timeDecay"
  maxMemoryExpansion = 1.0,                   # any decimal number; conditions the agents' maximum memory sizes
  splitAndMerge = FALSE,                      # apply split & merge algorithm or not
  perceptionOOVNN = 5,                        # number of nearest neighbours used to attribute label in case of unknown word
  
  ##### Interaction
  
  interactionPartners = "random",             # "random" or "betweenGroups" or "withinGroups"
  speakerProb = NULL,                         # NULL or a vector of numerics; whether some agents should speak more often than others
  listenerProb = NULL,                        # NULL or a vector of numerics; whether some agents should listen more often than others
  
  ##### Runs
  
  runMode = "single",                         # "single" or "multiple"
  multipleABMRuns = 100,                      # any full positive number; number of ABM runs if runMode == "multiple"
  nrOfSnapshots = 5,                          # any full positive number; how often the population is archived during the simulation
  interactionsPerSnapshot = 100,              # any full positive number; how many interactions take place per snapshot

  ##### Other options
  
  rootLogDir = "./logs/",                     # absolute or relative path to logging directory
  doSplitAndMergeBeforeABM = FALSE,           # apply split & merge before the first interaction or not
  splitAndMergeInterval = 100,                # any full positive number; after how many interactions an agent applies split & merge
  mahalanobisThreshold = 1.5,                 # any full positive number; threshold if memoryIntakeStrategy == "mahalanobisDistance"
  notes = "Extended andalusian data set"      # optional: Some further notes on the current simulation for better documentation

)

