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
  
  ##### Input data
  
  inputDataFile = "/vdata/ABM/data/u-fronting/production_dataframes/u-fronting_NoLob_F2bark.csv",  # absolute or relative path to input data
  inputExemplarsFile = "/vdata/ABM/data/u-fronting/production_dataframes/u-fronting_NoLob_F2bark_tracks_fd.rds",
  # features = c("P1", "P2", "P3"),                   # columns in inputDataFile that are used as features
  group = "age",                            # the column in inputDataFile that defines the agents' groups
  label = "label",                           # the column in inputDataFile that stores the phonological labels; these labels will be changed during the interactions
  initial = "label",                         # the column in inputDataFile that stores the phonological labels; these labels will remain unchanged
  word = "word",                               # the column in inputDataFile that stores the word labels
  speaker = "speaker",                          # the column in inputDataFile that stores the speakers' IDs or names
  subsetSpeakers = NULL,                      # NULL or a vector of strings, e.g. c("spk01", "spk02", "spk03")
  subsetLabels = NULL,                        # NULL or a vector of strings, e.g. c("a", "i", "u", "o")
  
  ##### Initial setup (befor interactions)
  
  initialMemoryResampling = FALSE,
  initialMemoryResamplingFactor = 2,
  proportionGroupTokens = 0.0,                   # between 0.0 and 1.0; proportion of tokens from own speaker group that an agent is initialised with
  rememberOwnTokens = FALSE,                      # whether or not to perceive one's own tokens
  
  ##### feature extraction
  featureExtractionMethod = "FPCA",
  lambdaFPCA = 1e-8,
  varCutoffFPCA = 0.90,
  
  ##### Production
  
  productionBasis = "word",
  productionResampling = "SMOTE",
  productionResamplingFallback = "label",
  productionMinTokens = 20,                 # used if productionStrategy == "SMOTE"
  productionSMOTENN = 5,                    # used if productionStrategy == "SMOTE"
  
  ##### Perception
  
  # perceptionModels = "singleGaussian",
  memoryIntakeStrategy = c("mahalanobisDistance", "MSEthreshold"), # , "maxPosteriorProb"
  MSEthresholdMaxCoef = 2.0,
  memoryRemovalStrategy = "random",        # "outlierRemoval" or "timeDecay"
  maxMemoryExpansion = 1.0,                   # any decimal number; conditions the agents' maximum memory sizes
  splitAndMerge = FALSE,                      # apply split & merge algorithm or not
  splitMergeMethod = "bic",
  perceptionOOVNN = 5,                        # number of nearest neighbours used to attribute label in case of unknown word
  forgettingRate = 0,
  computeFeaturesInterval = 1000,
  
  ##### Interaction
  
  interactionPartners = "betweenGroups",             # ... or "betweenGroups" or "withinGroups"
  speakerProb = NULL,                         # ... or a vector of numerics
  listenerProb = NULL,                        # ... or a vector of numerics
  
  ##### Runs
  
  runMode = "single",                         # "single" or "multiple"
  multipleABMRuns = 8,                       # any full positive number; number of ABM runs if runMode == "multiple"
  nrOfSnapshots = 2,                         # any full positive number; how often the population is archived during the simulation
  interactionsPerSnapshot = 1000,              # any full positive number; how many interactions take place per snapshot
  
  ##### Other options
  
  rootLogDir = "/homes/m.gubian/ABM/ABM/logDir/Wende", #  "/vdata/Projects/ABM/simulations/Michele/u-fronting_Language2017/resanpling_forgetting",
  doSplitAndMergeBeforeABM = TRUE,           # apply split & merge before the first interaction or not
  splitAndMergeInterval = 1000,                # any full positive number; after how many interactions an agent applies split & merge
  mahalanobisThreshold = 10, #qchisq(.99, df = 3) %>% round(2),                 # any full positive number; threshold if memoryIntakeStrategy == "mahalanobisDistance"
  notes = "Wende 1.0 FPCA"         # optional: Some further notes on the current simulation for better documentation
)
