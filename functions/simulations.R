################################################################################
#                                                                              #
# This script contains functions that organize and register the simulations.   #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

SIM_REG_FILENAME <- "simulations_register.rds"
PARAMS_FILENAME <- "params.yaml"

savePopulation <- function(pop, extraCols = list(condition = "x"), logDir) {
  # This function ...
  # Function call in interactions.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(convert_pop_list_to_dt(pop, extraCols),
          file = file.path(logDir, paste("pop", unlist(extraCols), "rds", sep = "."))
  )
}

saveInteractionsLog <- function(interactionsLog, logDir) {
  # This function ...
  # Function call in loadLibraries.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(interactionsLog,
          file.path(logDir, "intLog.rds")
  )
}

generateSimulationName <- function(prefix = "ABM") {
  # This function ...
  # Function call in ABMmain.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S"))
}

setFeatureNames <- function(input.df, cols) {
  # This function ...
  # Function call in ABMmain.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  stopifnot(all(cols %in% colnames(input.df)))
  input.df %>% setnames(cols, paste0("P", seq_along(cols)))
}

createSimulationRegister <- function(rootLogDir, force = FALSE) {
  # This function ...
  # Function call in ABMmain.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  if (!file.exists(file.path(rootLogDir, SIM_REG_FILENAME)) | force) {
    list.save(list(), file.path(rootLogDir, SIM_REG_FILENAME))
  }
}

registerSimulation <- function(params) {
  # This function ...
  # Function call in ABMmain.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  params[["initial"]] <- as.character(params[["initial"]])
  params[["label"]] <- as.character(params[["label"]])
  list.save(params, file.path(params[["rootLogDir"]], params[['simulationName']], PARAMS_FILENAME))
  params[['completed']] <- FALSE
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  list.load(regFile) %>%
    list.append(params) %>%
    list.save(regFile)
}

setCompleted <- function(simulationName_, rootLogDir) {
  # This function ...
  # Function call in ABMmain.R.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  i <- reg %>% list.findi(simulationName == simulationName_)
  reg[[i]]['completed'] <- TRUE
  list.save(reg, regFile)
}

deleteSimulation <- function(simulationName_, rootLogDir) {
  # This function ...
  # Function call in ...
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  list.load(regFile) %>%
    list.exclude(simulationName == simulationName_) %>%
    list.save(regFile)
}

purgeSimulation <- function(simulationName_, rootLogDir) {
  # This function ...
  # Function call in ...
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  deleteSimulation(simulationName_, rootLogDir)
  system(paste("rm -rf", file.path(rootLogDir, simulationName_)))
}

purgeNotCompleted <- function(rootLogDir) {
  # This function ...
  # Currently no function call. Utility function.
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  lapply(filterSimulations(rootLogDir, completed == FALSE),
         function(simName) {
           purgeSimulation(simName, rootLogDir)
         })
}

filterSimulations <- function(rootLogDir, ..., condList = NULL) {
  # This function ...
  # Function call in ...
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  if (!is.null(condList)) {
    matching <- sapply(reg, function(r) {
      sapply(names(condList), function(k) r[[k]] == condList[[k]]) %>% all()
    })
  } else {
    matching <- list.is(reg, ...)
  }
  reg[matching] %>% list.select(simulationName) %>% unlist
}

getFieldFromSimRegister <- function(rootLogDir, ...) {
  # This function ...
  # Function call in ...
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  list.map(reg, ...)
}

getParams <- function(rootLogDir, simulationName) {
  # This function ...
  # Function call in ...
  #
  # Args:
  #    - 
  #
  # Returns:
  #    - 
  #
  
  list.load(file.path(rootLogDir, simulationName, PARAMS_FILENAME))
}

