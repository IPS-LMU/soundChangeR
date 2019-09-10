################################################################################
#                                                                              #
# This script contains functions may be helpful for debugging purposes.        #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

convert_pop_dt_to_list <- function(pop.dt) {
  # This function converts the population data.table into a list.
  #
  # Args:
  #    - pop.dt: the population data.table
  #
  # Returns:
  #    - population: a list of lists
  #

  population <- list()
  Pcols <- grep("^P[[:digit:]]+$", colnames(pop.dt), value = TRUE)
  for (id in pop.dt$agentID %>% unique) {
    population[[id]] <- list()
    population[[id]]$agentID <- id
    population[[id]]$labels <- pop.dt[agentID == id, .(word, label, valid, nrOfTimesHeard, producerID, timeStamp)]
    population[[id]]$group <- pop.dt[agentID == id, group][1]
    population[[id]]$speaker <- pop.dt[agentID == id, speaker][1]
    population[[id]]$features <- pop.dt[agentID == id, .SD, .SDcols = Pcols]
    population[[id]]$initial <- pop.dt[valid == TRUE, .(word, initial)] %>% unique
    population[[id]]$cache <- data.table(name = "qda", value = list(), valid = FALSE)
  }
  return(population)
}

make_equivalence_labels <- function(labels) {
  # This function generates all possible combinations of
  # phonological labels that can be used as equivalence labels.
  #
  # Args:
  #    - labels: vector of strings
  #
  # Returns:
  #    - the newly formed labels
  #

  ulab <- labels %>% unique %>% sort %>% as.character
  return(
    Map(combn,
        list(ulab),
        seq_along(ulab),
        list(function(x) paste0(x, collapse="+"))
        ) %>% unlist
    )
}

get_equivalence_clusters <- function(population, eLabels) {
  # This function calculates in how many of the agents each
  # equivalence label occurs.
  #
  # Args:
  #    - population: a data.table
  #    - eLabels: all the equivalence labels to take into account, usually obtained
  #     by running make_equivalence_labels()
  #
  # Returns:
  #    - a data.table with columns 'equivalence', the labels, and 'N_Agents',
  #     the number of agents where a label occurs

  eq <- data.table(equivalence = eLabels)
  population[, .N, by = .(agentID, equivalence)][
    , .SD[eq, .(equivalence, N), on = "equivalence"], by = agentID][
      , .(N_Agents = sum(!is.na(N))), by = equivalence
      ]
}
