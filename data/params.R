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
  
  inputDataFile = "/vdata/Projects/ABM/data/u-fronting/u-fronting_NoLob_input_u.csv",    # absolute or relative path to input data
  features = c("P1", "P2", "P3"),                # the column(s) in inputDataFile that is/are used as features
  group = "group",                               # the column in inputDataFile that defines the agents' groups
  label = "label",                               # the column in inputDataFile that stores the phonological labels (can be changed)
  initial = "label",                             # the column in inputDataFile that stores the phonological labels (will not be changed)
  word = "word",                                 # the column in inputDataFile that stores the word labels
  speaker = "speaker",                           # the column in inputDataFile that stores the speakers' IDs or names
  subsetSpeakers = NULL,                         # NULL or a vector of strings, e.g. c("spk01", "spk02", "spk03")
  subsetLabels = NULL,                           # NULL or a vector of strings, e.g. c("a", "i", "u", "o")
  
  ##### Initial setup (before interactions)
  
  createPopulationMethod = "speaker_is_agent",   # "speaker_is_agent" or "bootstrap"
  bootstrapPopulationSize = 50,                  # full positive number; only if createPopulationMethod == "bootstrap"
  initialMemoryResampling = FALSE,
  initialMemoryResamplingFactor = 1.0,           #
  
  ##### Production
  
  productionBasis = "word",                      # "word" or "label"
  productionResampling = "SMOTE",                # NULL or "SMOTE"
  productionResamplingFallback = "label",        # currently only "label"
  productionMinTokens = 20,                      # used if productionResampling == "SMOTE"
  productionSMOTENN = 5,                         # used if productionResampling == "SMOTE"
  
  ##### Perception
  
  # perceptionModels = "singleGaussian",
  memoryIntakeStrategy = "mahalanobisDistance",  # "maxPosteriorProb" and/or "mahalanobisDistance" and/or "posteriorProbThr"
  mahalanobisThreshold = qchisq(.99, df = 3) %>% round(2),   # threshold if memoryIntakeStrategy == "mahalanobisDistance"
  posteriorProbThr = 1/3,                        # only if memoryIntakeStrategy == "posteriorProbThr"
  perceptionOOVNN = 5,                           # number of nearest neighbours used to attribute label in case of unknown word
  
  ##### Forgetting
  forgettingRate = 0,                            # number between 0 and 1
  memoryRemovalStrategy = "random",              # "random" (recommended) or "outlierRemoval" or "timeDecay"
  
  ##### Interactions
  
  interactionPartners = "betweenGroups",         # "random" or "betweenGroups" or "withinGroups"
  speakerProb = NULL,                            # NULL or a vector of numerics; whether some agents should speak more often than others
  listenerProb = NULL,                           # NULL or a vector of numerics; whether some agents should listen more often than others
  
  ##### Split and merge
  splitAndMerge = FALSE,                         # apply split & merge algorithm or not
  doSplitAndMergeBeforeABM = FALSE,              # apply split & merge before the first interaction or not
  splitAndMergeInterval = 100,                   # any full positive number; after how many interactions an agent applies split & merge
  
  ##### Runs
  
  runMode = "single",                            # "single" or "multiple"
  multipleABMRuns = 2,                           # any full positive number; number of ABM runs if runMode == "multiple"
  nrOfSnapshots = 2,                             # any full positive number; how often the population is archived during the simulation
  interactionsPerSnapshot = 1000,                # any full positive number; how many interactions take place per snapshot

  ##### Other options
  
  rootLogDir = "../experiments/removalStrategy/u-fronting/logs",         #./logs      # absolute or relative path to logging directory
  notes = "u-fronting test"                      # some further notes on the current simulation for better documentation

)

