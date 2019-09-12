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

save_population <- function(pop, extraCols = list(condition = "x"), logDir) {
  # This function saves the population as an RDS archive.
  # Function call in interactions.R, perform_interactions(); and 
  # loadLibraries.R, coreABM().
  #
  # Args:
  #    - pop: list of the population
  #    - extraCols: a list of columns to be added to the final 
  #      pop data.table. Default: list(condition = "x")
  #    - logDir: path to the logging directory
  #
  # Returns:
  #    - nothing.
  #
  
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(convert_pop_list_to_dt(pop, extraCols),
          file = file.path(logDir, paste("pop", unlist(extraCols), "rds", sep = "."))
  )
}

save_interactions_log <- function(interactionsLog, logDir) {
  # This function saves the interaction log as a RDS archive.
  # Function call in loadLibraries.R, coreABM().
  #
  # Args:
  #    - interactionsLog: a data table that contains information
  #      on the interactions
  #    - logDir: path to the logging directory
  #
  # Returns:
  #    - nothing.
  #
  
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(interactionsLog,
          file.path(logDir, "intLog.rds")
  )
}

generate_simulation_name <- function(prefix = "ABM") {
  # This function generates a name for the simulation out of
  # a given prefix and the system time.
  # Function call in ABMmain.R.
  #
  # Args:
  #    - prefix: string to be added at the beginning of
  #      the simulation name. Default: "ABM"
  #
  # Returns:
  #    - the full simulation name as a string.
  #
  
  paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S"))
}

set_feature_names <- function(input.df, cols) {
  # This function renames the feature columns to P1, P2, P3, etc.
  # Function call in ABMmain.R.
  #
  # Args:
  #    - input.df: the input data.frame
  #    - cols: a list of column names from input.df
  #
  # Returns:
  #    - the input.df with changed column names
  #
  
  stopifnot(all(cols %in% colnames(input.df)))
  input.df %>% setnames(cols, paste0("P", seq_along(cols)))
}

create_simulation_register <- function(rootLogDir, force = FALSE) {
  # This function creates the central simulation register if 
  # it does not yet exist.
  # Function call in ABMmain.R.
  #
  # Args:
  #    - rootLogDir: path to the root logging directory
  #    - force: a boolean that indicates whether or not to force
  #      the creation of the register even if it already exists.
  #      Default: FALSE.
  #
  # Returns:
  #    - nothing.
  #
  
  if (!file.exists(file.path(rootLogDir, SIM_REG_FILENAME)) | force) {
    list.save(list(), file.path(rootLogDir, SIM_REG_FILENAME))
  }
}

register_simulation <- function(params) {
  # This function adds the parameters of current simulation 
  # to the central simulation register.
  # Function call in ABMmain.R.
  #
  # Args:
  #    - params: a list of params from params.R
  #
  # Returns:
  #    - nothing.
  #
  
  params[["initial"]] <- as.character(params[["initial"]])
  params[["label"]] <- as.character(params[["label"]])
  list.save(params, file.path(params[["rootLogDir"]], params[['simulationName']], PARAMS_FILENAME))
  params[['completed']] <- FALSE
  regFile <- file.path(params[["rootLogDir"]], SIM_REG_FILENAME)
  list.load(regFile) %>%
    list.append(params) %>%
    list.save(regFile)
}

set_completed <- function(simulationName_, rootLogDir) {
  # This function registers the current simulation as completed.
  # Function call in ABMmain.R.
  #
  # Args:
  #    - simulationName_: the simulation name as a string
  #    - rootLogDir: the path to the root logging directory
  #
  # Returns:
  #    - nothing.
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  i <- reg %>% list.findi(simulationName == simulationName_)
  reg[[i]]['completed'] <- TRUE
  list.save(reg, regFile)
}

delete_simulation <- function(simulationName_, rootLogDir) {
  # This function deletes a simulation from the central simulation register.
  # Function call in simulations.R, purge_simulation().
  #
  # Args:
  #    - simulationName_: the simulation name as a string
  #    - rootLogDir: the path to the root logging directory
  #
  # Returns:
  #    - nothing.
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  list.load(regFile) %>%
    list.exclude(simulationName == simulationName_) %>%
    list.save(regFile)
}

purge_simulation <- function(simulationName_, rootLogDir) {
  # This function deletes the simulation from the 
  # register and also deletes the simulation results themselves.
  # Function call in simulations.R, purge_uncompleted_simulations().
  #
  # Args:
  #    - simulationName_: the simulation name as a string
  #    - rootLogDir: the path to the root logging directory
  #
  # Returns:
  #    - nothing.
  #
  
  delete_simulation(simulationName_, rootLogDir)
  system(paste("rm -rf", file.path(rootLogDir, simulationName_)))
}

purge_uncompleted_simulations <- function(rootLogDir) {
  # This function purges all simulations that have not been completed.
  # No function call (utility function).
  #
  # Args:
  #    - rootLogDir: the path to the root logging directory
  #
  # Returns:
  #    - nothing.
  #
  
  lapply(filter_simulations(rootLogDir, completed == FALSE),
         function(simName) {
           purge_simulation(simName, rootLogDir)
         })
}

filter_simulations <- function(rootLogDir, ..., condList = NULL) {
  # This function searches for simulations in the central register
  # that match certain params or conditions.
  # Function call in simulations.R, purge_uncompleted_simulations().
  #
  # Args:
  #    - rootLogDir: the path to the root logging directory
  #    - ...: parameters that characterise the searched simulation(s)
  #    - condList: list of parameters that characterise the 
  #      searched simulation(s). Default: NULL.
  #
  # Returns:
  #    - the name of the matching simulation(s).
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  if (!is.null(condList)) {
    matching <- sapply(reg, function(r) {
      sapply(names(condList), function(k) r[[k]] == condList[[k]]) %>% all()
    })
  } else {
    # matching <- list.is(reg, tryEval(..., FALSE))
    matching <- list.is(reg, ...)
  }
  reg[matching] %>% list.select(simulationName) %>% unlist
}

get_field_from_sim_register <- function(rootLogDir, ...) {
  # This function returns the values of any parameter for all simulations 
  # that are stored in the central register.
  # No function call (utility function).
  #
  # Args:
  #    - rootLogDir: the path to the root logging directory
  #    - ...: parameters that characterise the simulation(s)
  #
  # Returns:
  #    - a list of the parameter values for all simulations.
  #
  
  regFile <- file.path(rootLogDir, SIM_REG_FILENAME)
  reg <- list.load(regFile)
  list.map(reg, ...)
}

get_params <- function(rootLogDir, simulationName) {
  # This function loads the list of parameters in params.yaml 
  # for a specific simulation.
  # No function call (utility function).
  #
  # Args:
  #    - rootLogDir: the path to the root logging directory
  #    - simulationName: name of the simulation as a string
  #
  # Returns:
  #    - nothing.
  #
  
  list.load(file.path(rootLogDir, simulationName, PARAMS_FILENAME))
}

