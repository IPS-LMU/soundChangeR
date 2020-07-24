context("memoryIntakeStrategy")

params = list(
  inputDataFile = "u-fronting_NoLob_F2bark.csv",  # absolute or relative path to input data
  inputExemplarsFile = "u-fronting_NoLob_F2bark_tracks_fd.rds",
  group = "age",                            # the column in inputDataFile that defines the agents' groups
  label = "label",                           # the column in inputDataFile that stores the phonological labels; these labels will be changed during the interactions
  initial = "label",                         # the column in inputDataFile that stores the phonological labels; these labels will remain unchanged
  word = "word",                               # the column in inputDataFile that stores the word labels
  speaker = "speaker",                          # the column in inputDataFile that stores the speakers' IDs or names
  
  initialMemoryResampling = FALSE,
  proportionGroupTokens = 0.0,                   # between 0.0 and 1.0; proportion of tokens from own speaker group that an agent is initialised with
  rememberOwnTokens = FALSE,                      # whether or not to perceive one's own tokens
  
  featureExtractionMethod = "FPCA",
  lambdaFPCA = 1e-8,
  varCutoffFPCA = 0.90,
  posteriorProbMethod = "qda",
  
  memoryIntakeStrategy = NULL,
  mahalanobisProbThreshold = .95,
  MSEthresholdQuantile = .9,
  memoryRemovalStrategy = "random",        # "outlierRemoval" or "timeDecay"
  maxMemoryExpansion = 1.0,                   # any decimal number; conditions the agents' maximum memory sizes
  splitAndMerge = FALSE,                      # apply split & merge algorithm or not
  perceptionNN = 5,                        # number of nearest neighbours used to attribute label in case of unknown word
  forgettingRate = 0,
  computeFeaturesInterval = 10000, # large number, to avoid computation
  splitAndMergeInterval = 10000 # large number, to avoid computation
)

input.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
input.df %>% setnames(c(params[["word"]], params[["speaker"]], params[["group"]]), c("word", "speaker", "group"))
input.df$initial <- input.df[, params[["initial"]], with = FALSE]
input.df$label <- input.df[, params[["label"]], with = FALSE]
fdObj <- readRDS(params[['inputExemplarsFile']])
input.df[, exemplar := all_fd2exemplar(fdObj)]

test_that("Accept/reject decisions are invariant with respect to order of execution of memoryIntakeStrategies", {
  Ntokens <- 100
  input.rows <- sample(input.df %>% nrow, Ntokens)
  memoryIntakeStrategy <- c(
    list(c("MSEthreshold", "mahalanobisDistance", "maxPosteriorProb")),
    list(c("mahalanobisDistance", "MSEthreshold", "maxPosteriorProb")),
    list(c("maxPosteriorProb", "MSEthreshold", "mahalanobisDistance"))
  )
  interactionsLog <- replicate(length(memoryIntakeStrategy),
                               create_interactions_log(Ntokens),
                               simplify = FALSE)
  lapply(seq_along(memoryIntakeStrategy), function(i) {
    agent <- create_agent(1, input.df, "dami", 1000, params)
    params$memoryIntakeStrategy <- memoryIntakeStrategy[i] %>% unlist
    input.df[input.rows, {
      producedToken <- data.table(word = word,
                                  label = label,
                                  initial = initial,
                                  exemplar = exemplar,
                                  nrOfTimesHeard = 1,
                                  producerID = 1
      ) 
      perceive_token(agent, producedToken, interactionsLog[[i]], 1, params, TRUE)
    }, by = sl_rowIdx]
  })
  for (i in 2:length(interactionsLog)) {
    expect_equal(interactionsLog[[1]]$accepted, interactionsLog[[i]]$accepted)
  } 
})
