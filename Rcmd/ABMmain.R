################################################################################
#                                                                              #
# This script contains the ABM main routine.                                   #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

# load libraries and parameters
source(file.path("Rcmd", "loadLibraries.R"))
source(file.path("data", "params.R"))

# create root logging directory and simulations register if they do not yet exist
dir.create(params[["rootLogDir"]], showWarnings = FALSE, recursive = TRUE)
create_simulation_register(params[["rootLogDir"]])

# create logging directory for this specific simulation
params[["simulationName"]] <- generate_simulation_name()
logDir <- file.path(params[["rootLogDir"]], params[["simulationName"]])
dir.create(logDir, showWarnings = FALSE, recursive = TRUE)

# load input data.table
input.df <- suppressWarnings(fread(params[["inputDataFile"]], stringsAsFactors = F))
input.df %>% setnames(c(params[["word"]], params[["speaker"]], params[["group"]]), c("word", "speaker", "group"))
input.df$initial <- input.df[, params[["initial"]], with = FALSE]
input.df$label <- input.df[, params[["label"]], with = FALSE]
if (!is.null(params[["subsetSpeakers"]])) {
  input.df <- input.df[speaker %in% params[["subsetSpeakers"]]]
}
if (!is.null(params[["subsetLabels"]])) {
  input.df <- input.df[label %in% params[["subsetLabels"]]]
}

if (params[["featureExtractionMethod"]] == "identity") {
  set_feature_names(input.df, params[["features"]])
  input.df[, exemplar := matrix2exemplar(.SD), .SDcols = names(input.df) %like% "^P[[:digit:]]"]
} else if (params[["featureExtractionMethod"]] == "FPCA") {
  fdObj <- readRDS(params[["inputExemplarsFile"]])
  if (!is.null(params[["subsetSpeakers"]])) {
    fdObj[[1]] <- fdObj[[1]][1:10, unique(input.df$sl_rowIdx), 1:2]
  }
  if (!is.null(params[["subsetLabels"]])) {
    fdObj[[1]] <- fdObj[[1]][1:10, unique(input.df$sl_rowIdx), 1:2]
  }
  input.df[, exemplar := all_fd2exemplar(fdObj)]
}

# save input.df
saveRDS(input.df, file.path(logDir, "input.rds"))

# save commit hash so that it is known which version of the ABM was used here
params[["commitHash"]] <- system("git log -n1 --format=format:\"%H\"", intern = TRUE)

# log simulation in register and save params
register_simulation(params)

check <- check_params(params, input.df)
params <- check[[1]]

if (check[[2]]) {
  # run simulations
  if (params[["runMode"]] == "single") {
    params[["logDir"]] <- file.path(logDir, "1")
    coreABM(input.df, params, file.path(logDir, "1"))
  } else if (params[["runMode"]] == "multiple") {
    numCores <- detectCores() - 1
    if (Sys.info()[["sysname"]] == "Windows") {
      cl <- makeCluster(numCores, type = "PSOCK")
      clusterExport(cl, c("input.df", "params", "logDir"))
      clusterEvalQ(cl, {
        source(file.path("Rcmd", "loadLibraries.R"))
      })
    } else {
      cl <- makeCluster(numCores, type = "FORK")
    }
    clusterSetRNGStream(cl)
    parLapply(cl, seq_len(params[["multipleABMRuns"]]), function(abmName) {
      params[["logDir"]] <- file.path(logDir, abmName)
      coreABM(input.df, params, file.path(logDir, abmName))
    })
    stopCluster(cl)
  }
  set_completed(params[["simulationName"]], params[["rootLogDir"]])
}

