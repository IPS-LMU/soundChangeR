################################################################################
#                                                                              #
# Parameter settings. Further documentation at https://github.com/IPS-LMU/ABM. #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

params = list(
  
  ##### Input data
  
  inputDataFile = "./data/u-fronting.csv",    # absolute or relative path to input data
  features = c("DCT0", "DCT1", "DCT2"),          # the column(s) in inputDataFile that is/are used as features
  group = "age",                                 # the column in inputDataFile that defines the agents' groups
  label = "phoneme",                             # the column in inputDataFile that stores the phonological labels (can be changed)
  initial = "initial",                           # the column in inputDataFile that stores the phonological labels (will not be changed)
  word = "word",                                 # the column in inputDataFile that stores the word labels
  speaker = "spk",                               # the column in inputDataFile that stores the speakers' IDs or names
  subsetSpeakers = NULL,                         # NULL or a vector of strings, e.g. c("spk01", "spk02", "spk03")
  subsetLabels = NULL,                           # NULL or a vector of strings, e.g. c("a", "i", "u", "o")
  
  ##### Initial setup (before interactions)
  
  createPopulationMethod = "speaker_is_agent",   # "speaker_is_agent" or "bootstrap"
  bootstrapPopulationSize = 50,                  # full positive number; only if createPopulationMethod == "bootstrap"
  initialMemoryResampling = FALSE,               # enlarge the agents' memories before the interactions or not
  initialMemoryResamplingFactor = 2.0,           # 1.0 or higher; only if initialMemoryResampling == TRUE
  proportionGroupTokens = 0.0,                   # between 0.0 and 1.0; proportion of tokens from own speaker group that an agent is initialised with
  rememberOwnTokens = FALSE,                     # whether or not to perceive one's own tokens
  
  ##### Feature Extraction
  
  featureExtractionMethod = "identity",          # "FPCA" or "identity"; which method is to be used to extract features from exemplars
  lambdaFPCA = 1e-8,                             # only if featureExtractionMethod == "FPCA"
  varCutoffFPCA = 0.5,                           # must be greater than 0.0 and smaller than 1.0; variance cutoff
  computeFeaturesInterval = 1000,                # interval by which to compute features again
  
  ##### Production
  
  productionBasis = "word",                      # "word" or "label"; estimate Gaussian based on tokens associated with words or labels
  productionResampling = "SMOTE",                # NULL or "SMOTE"; apply SMOTE to make Gaussian more stable or not
  productionResamplingFallback = "label",        # currently only "label"
  productionMinTokens = 20,                      # only if productionResampling == "SMOTE"; minimum number of tokens to be used in building Gaussian
  productionSMOTENN = 5,                         # only if productionResampling == "SMOTE"; number of nearest neighbours used in SMOTE
  
  ##### Perception
  
  perceptionModels = "GMM",                      # "singleGaussian" or "GMM"; whether or not to use GMMs
  memoryIntakeStrategy = "mahalanobisDistance",  # "maxPosteriorProb" and/or "mahalanobisDistance" and/or "posteriorProbThr"
  mahalanobisProbThreshold = .95,                # threshold if memoryIntakeStrategy == "mahalanobisDistance", between 0.0 and 1.0
  MSEthresholdQuantile = 0.999,                  # threshold if memoryIntakeStrategy == "MSEthreshold", between 0.0 and 1.0
  posteriorProbThr = 1/3,                        # only if memoryIntakeStrategy == "posteriorProbThr"
  perceptionOOVNN = 5,                           # uneven full number; assign label to unknown word based on majority vote among perceptionOOVNN nearest neighbours
  computeGMMsInterval = 10,                      # how many tokens an agent must have accepted for it to re-compute GMM
  purityRepetitions = 5,                         # 
  purityThreshold = 0.75,                        # 
  
  ##### Forgetting
  
  forgettingRate = 0,                            # number between 0 and 1
  memoryRemovalStrategy = "random",              # "random" (recommended) or "outlierRemoval" or "timeDecay"
  
  ##### Interactions
  
  interactionPartners = "betweenGroups",         # "random" or "betweenGroups" or "withinGroups"; from which groups the interacting agents must be
  speakerProb = NULL,                            # NULL or a vector of numerics; whether some agents should speak more often than others
  listenerProb = NULL,                           # NULL or a vector of numerics; whether some agents should listen more often than others
  
  ##### Split and merge
  
  splitAndMerge = FALSE,                         # apply split & merge algorithm or not
  doSplitAndMergeBeforeABM = FALSE,              # apply split & merge before the first interaction or not
  splitAndMergeInterval = 100,                   # any full positive number; after how many interactions an agent applies split & merge
  splitMergeMethod = "bic",                      # "bic" or "t.test"; use Bayesion Information Criterion or t-test to decide on split or merge
  
  ##### Runs
  
  runMode = "single",                            # "single" or "multiple"
  multipleABMRuns = 4,                           # any full positive number; number of ABM runs if runMode == "multiple"
  nrOfSnapshots = 20,                            # any full positive number; how often the population is archived during the simulation
  interactionsPerSnapshot = 1000,                # any full positive number; how many interactions take place per snapshot

  ##### Other options
  
  rootLogDir = "./logDir",                       # absolute or relative path to logging directory
  notes = "u-fronting, three phonemes"           # some further notes on the current simulation for better documentation

)

