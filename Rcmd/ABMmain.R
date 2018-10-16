### ABM main routine

# set path (change manually)
ABMpath <- "/homes/m.gubian/ABM/ABM"
setwd(ABMpath)

# load libraries
source(file.path("Rcmd", "loadLibraries.R"))
# log dir
rootLogDir <- "/vdata/Projects/ABM/simulations/Michele/u-fronting_Language2017"
dir.create(rootLogDir, showWarnings = FALSE, recursive = TRUE)
# create simulations register if it does not exist
createSimulationRegister(rootLogDir)


# source parameter file
source(file.path("data", "u-fronting.params.R"))
params[['prettyName']] <- "u-fronting_non-normalised" # optional
# load input dataframe
input.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
# adapt it, e.g. create groups, subset speakers, etc.
input.df %>% setnames(Cs(W, Vpn, V, Age),
                      Cs(word, speaker, initial, group))
input.df[, initial := as.character(initial)]
input.df[, labels := initial]
# select 22 speakers 
input.df <- input.df[!speaker %in% c("apwi", "chbr", "olto", "arkn", "gisa")]

# original feature names (columns in input.df)
params[['features']] <- Cs(k0, k1, k2)
setFeatureNames(input.df, params[['features']])
params[['notes']] <- "F2 DCT 0,1,2 coefficients, no Lobanov norm."


# set up different simulation settings by modifying params

Config <- expand.grid(
  memoryRemovalStrategy = c("timeDecay", "outlierRemoval"),
  memoryIntakeStrategy = c( "posteriorProbThr", "mahalanobisDistance"), 
  interactionPartners = c("random", "withinGroups")
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
