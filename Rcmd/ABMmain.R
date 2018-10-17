### ABM main routine

# set path (change manually)
ABMpath <- "/homes/m.gubian/ABM/ABM"
setwd(ABMpath)

# load libraries
source(file.path("Rcmd", "loadLibraries.R"))
# log dir
rootLogDir <- "/vdata/Projects/ABM/simulations/Michele/happy_Antarctica"
dir.create(rootLogDir, showWarnings = FALSE, recursive = TRUE)
# create simulations register if it does not exist
createSimulationRegister(rootLogDir)


# source parameter file
source(file.path("data", "u-fronting.params.R"))
params[['prettyName']] <- "happy_Antarctica" # optional
# load input dataframe
input.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
# adapt it, e.g. create groups, subset speakers, etc.
# input.df <- input.df[session == 0 & V %in% c("i:", "ju", "u")] # u-fronting
input.df <- input.df[session == 0 & V %in% c("i:", "I:", "I")] # happy
input.df %>% setnames(Cs(ORT, Vpn, V),
                      Cs(word, speaker, initial))
input.df[, initial := as.character(initial)]
input.df[, labels := initial]
input.df[, group := 'dummy']
# select 22 speakers 
# input.df <- input.df[!speaker %in% c("apwi", "chbr", "olto", "arkn", "gisa")]

# original feature names (columns in input.df)
# params[['features']] <- Cs(kF20, kF21, kF22) # u-fronting
params[['features']] <- Cs(kF10, kF12, kF20) # happy
setFeatureNames(input.df, params[['features']])
params[['notes']] <- "DCT: F1k0,	F1k2,	F2k0	(mean	of	F1,	slope	of	F1,	mean	of	F2), Lobanov norm., data version Oct 2018, session 0"
params[['codeCommit']] <- system("git log -n1 --format=format:\"%H\"", intern = TRUE)

# set up different simulation settings by modifying params

Config <- expand.grid(
  maxMemoryExpansion = c(1,10)
  # memoryRemovalStrategy = c("timeDecay", "outlierRemoval"),
  # memoryIntakeStrategy = c( "posteriorProbThr", "mahalanobisDistance"), 
  # interactionPartners = c("random", "withinGroups")
) %>% setDT


for (Config_i in seq_len(nrow(Config))) {
  for (param in colnames(Config)) {
    params[[param]] <- Config[[param]][Config_i]
  }
  params[['simulationName']] <- generateSimulationName()
  logDir <- file.path(rootLogDir, params[['simulationName']])
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  
  # save input.df
  saveRDS(input.df, file.path(logDir, "input.rds"))
  
  # log simulation in register and save params
  registerSimulation(params, rootLogDir)
  
  # run simulations
  if (params[['runMode']] == "single") {
    coreABM(logDir)
  } else if (params[['runMode']] == "multiple") {
    require(parallel)
    numCores <- detectCores() - 1
    cl <- makeCluster(numCores, type = "FORK")
    clusterSetRNGStream(cl)
    parLapply(cl, seq_len(params[['multipleABMRuns']]), function(abmName) {
      coreABM(file.path(logDir, abmName))
    })
    stopCluster(cl)  
  }
  setCompleted(params[['simulationName']], rootLogDir)
  
}




