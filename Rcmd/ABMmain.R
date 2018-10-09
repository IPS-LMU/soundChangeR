### ABM main routine

# set path (change manually)
ABMpath <- "/homes/m.gubian/ABM/ABM"
setwd(ABMpath)

# load libraries
source(file.path("Rcmd", "loadLibraries.R"))
# source parameter file
source(file.path("data", "params.R"))

# name of current simulation
simulationName <- paste("pre-aspiration",
                        "age",
                        "maxMemoryExpansion", params[['maxMemoryExpansion']],
                        "productionExtraTokensRatio", params[['productionExtraTokensRatio']],
                        sep = "."
)
# log dir
rootLogDir <- "/vdata/Projects/ABM/simulations/Michele/firstExplorations"
logDir <- file.path(rootLogDir, simulationName)
dir.create(logDir, showWarnings = FALSE, recursive = TRUE)

# and copy it into log dir
file.copy(file.path("data", "params.R"), file.path(logDir, "params.R"))
# load input dataframe (specified in params.R)
input.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
# old/young grouping
input.df[, group := Alter]
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

