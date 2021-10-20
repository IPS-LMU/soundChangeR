ABMmain <- function(paramsFile) {

  source(paramsFile)
  
  base::dir.create(params[["rootLogDir"]], showWarnings = FALSE, recursive = TRUE)
  create_simulation_register(params[["rootLogDir"]])
  
  params[["simulationName"]] <- generate_simulation_name()
  logDir <- base::file.path(params[["rootLogDir"]], params[["simulationName"]])
  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  
  input.df <- base::suppressWarnings(data.table::fread(params[["inputDataFile"]], stringsAsFactors = F))
  input.df %>% data.table::setnames(base::c(params[["word"]], params[["speaker"]], params[["group"]]), 
                                    base::c("word", "speaker", "group"))
  input.df$initial <- input.df[, params[["initial"]], with = FALSE]
  input.df$label <- input.df[, params[["label"]], with = FALSE]
  if (!base::is.null(params[["subsetSpeakers"]])) {
    input.df <- input.df[speaker %in% params[["subsetSpeakers"]]]
  }
  if (!base::is.null(params[["subsetLabels"]])) {
    input.df <- input.df[label %in% params[["subsetLabels"]]]
  }
  
  set_feature_names(input.df, params[["features"]])
  input.df[, exemplar := matrix2exemplar(.SD), .SDcols = base::names(input.df) %like% "^P[[:digit:]]"]
  
  base::saveRDS(input.df, file.path(logDir, "input.rds"))
  
  params[["commitHash"]] <- system("git log -n1 --format=format:\"%H\"", intern = TRUE)
  
  check <- check_params(params, input.df)
  params <- check[[1]]
  
  register_simulation(params)
  
  if (check[[2]]) {
    if (params[["runMode"]] == "single") {
      params[["logDir"]] <- base::file.path(logDir, "1")
      coreABM(input.df, params, base::file.path(logDir, "1"))
    } else if (params[["runMode"]] == "multiple") {
      numCores <- parallel::detectCores() - 1
      if (base::Sys.info()[["sysname"]] == "Windows") {
        cl <- parallel::makeCluster(numCores, type = "PSOCK")
        parallel::clusterExport(cl, base::c("input.df", "params", "logDir"))
        # parallel::clusterEvalQ(cl, {
        #   source(file.path("Rcmd", "loadLibraries.R"))
        # })
      } else {
        cl <- parallel::makeCluster(numCores, type = "FORK")
      }
      parallel::clusterSetRNGStream(cl)
      parallel::parLapply(cl, base::seq_len(params[["multipleABMRuns"]]), function(abmName) {
        params[["logDir"]] <- base::file.path(logDir, abmName)
        coreABM(input.df, params, file.path(logDir, abmName))
      })
      parallel::stopCluster(cl)
    }
    set_completed(params[["simulationName"]], params[["rootLogDir"]])
  }
}
