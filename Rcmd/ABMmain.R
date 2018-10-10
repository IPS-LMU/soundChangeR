### ABM main routine

# set path (change manually)
ABMpath <- "/homes/m.gubian/ABM/ABM"
setwd(ABMpath)

# load libraries
source(file.path("Rcmd", "loadLibraries.R"))
# source parameter file
source(file.path("data", "params.R"))

# log dir
rootLogDir <- "/vdata/Projects/ABM/simulations/Michele/firstExplorations"
rootLogDir <- "/homes/m.gubian/ABM/ABM/tests"
# create simulations register if it does not exist
createSimulationRegister(rootLogDir)

params[['simulationName']] <- generateSimulationName()
logDir <- file.path(rootLogDir, params[['simulationName']])
dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
# load input dataframe (can be specified in params.R)
input.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
# adapt it, e.g. create groups, subset speakers, etc.
input.df <- input.df[session == 0 & V %in% c("i:", "I:", "I")]
input.df %>% setnames(Cs(ORT, Vpn, V),
                      Cs(word, speaker, initial))
input.df[, initial := as.character(initial)]
input.df[, labels := initial]
input.df[, group := "dummy"]

# original feature names (columns in input.df)
params[['features']] <- Cs(kF10, kF12, kF20)
setFeatureNames(input.df, params[['features']])
# save input.df and params
saveRDS(input.df, file.path(logDir, "input.rds"))

# log simulation in register
registerSimulation(params, rootLogDir)

# run simulation
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

