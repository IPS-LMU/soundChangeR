
################################################################################
#                                                                              #
# This script contains the ABM main routine                                    #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################



### ABM main routine

# set path (change manually)
ABMpath <- "/homes/m.gubian/ABM/ABM"
setwd(ABMpath)

# load libraries
source(file.path("Rcmd", "loadLibraries.R"))
# root log dir for this experiment (change manually)
rootLogDir <- "tests/"
dir.create(rootLogDir, showWarnings = FALSE, recursive = TRUE)
# create simulations register if it does not exist
createSimulationRegister(rootLogDir)

# source parameter file
source(file.path("data", "params.R"))
params[['prettyName']] <- "Antarctica" # optional
# load input data.table 
input.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
# only session == 0 used in the simulation
input.df <- input.df[session == 0]

# original feature names (columns in input.df)
params[['features']] <- Cs(kF10, kF11, kF12, kF20, kF21, kF22)
# set their names as P1, P2, ... 
setFeatureNames(input.df, params[['features']])
# optional extra info
params[['notes']] <- "All DCT for F1 and F2 (6 dim), phonemes: i:, I:, ju, u, ou, I."
params[['codeCommit']] <- system("git log -n1 --format=format:\"%H\"", intern = TRUE)

# simulation setup
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

  



