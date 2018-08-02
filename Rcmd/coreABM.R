################################################################################
#                                                                              #
# This script contains the ABM run itself, i.e. it creates the data, modifies  #
# it through interactions between agents, applies split&merge, and plots the   #
# data (all according to the given parameters in params.R). The script is      #
# sourced in master.R.                                                         #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


# calculate number of agents, initialize their memories and create the population
nrOfAgents <- as.numeric(length(unique(input.df$speaker)))
initMemory <- initialize_memory(input.df)
population <- create_population_(nrOfAgents, initMemory)

# run a full split&merge on every agent
if (splitAndMerge == TRUE & doSplitAndMergeBeforeABM == TRUE) {
  for (j in 1:nrOfAgents) {
    population[[j]] <- splitandmergefull(population[[j]])
  }
}

# convert the population (list of lists) into a data.frame
originalPopulation <- convert_list_to_df(population, condition = "original")

# create equivalence labels
# equivalenceLabels <- make_equivalence_labels_(originalPopulation)
equivalenceLabels <- make_equivalence_labels(originalPopulation$initial)

# iterator of the ABM run
nrSim <- 0

# calculate equivalence labels
eclassLabels <- as.character(originalPopulation$initial)
for (speakers in unique(originalPopulation$speaker)) {
  origSpeakers <- originalPopulation$speaker == speakers
  eclassLabels[origSpeakers] <- equal_class(as.character(originalPopulation$initial[origSpeakers]),
                                            as.character(originalPopulation$label[origSpeakers]))
}

# add column for equivalence labels to originalPopulation
originalPopulation$equivalence <- eclassLabels

# calculate in how many agents each equivalence label occurs
originalEquivalenceClusters <- get_equivalence_clusters_(originalPopulation, equivalenceLabels, 
                                                        abmName, nrSim)
if (runMode == "single") {
  cat("\n"); print(dplyr::select_if(originalEquivalenceClusters,
                                    colnames(originalEquivalenceClusters) %in% equivalenceLabels &
                                      originalEquivalenceClusters[1, ] != 0 |
                                      colnames(originalEquivalenceClusters) %in% c("ABM", "simulation")));
  cat("\n")
}

# now that some variables have been set, source the plotting functions
source(file.path(ABMpath, "functions/plotting.R"))

# make plots before the ABM run, if wanted
plotPath <- file.path(img, paste("plotBeforeABM_", abmName, ".svg", sep = ""))
if (plotting != "none" & plotBeforeABM == TRUE) {
  svg(filename = plotPath, width = 10, height = 10, onefile = TRUE)
  originalPlot <- plot_before(originalPopulation)
  print(originalPlot)
  dev.off()
  if (runMode == "single") {
    print(originalPlot)
  }
}

# create data.frame which will be expanded during the ABM run
abm.df <- cbind(originalPopulation, ABM = rep(abmName, times = nrow(originalPopulation)))

# ABM run
for (nrSim in 1:nrOfSimulations) {
  
  # perform interactions
  population <- perform_interactions(pop = population, nrOfInteractions = interactionsPerSimulation)
  
  # convert population (list of lists) to data.frame
  modifiedPopulation <- convert_list_to_df(population, as.character(nrSim))
  
  # calculate equivalence labels
  eclassLabels <- as.character(modifiedPopulation$initial)
  for (speakers in unique(modifiedPopulation$speaker)) {
   modSpeakers <- modifiedPopulation$speaker == speakers
   eclassLabels[modSpeakers] <- equal_class(as.character(modifiedPopulation$initial[modSpeakers]), 
                                            as.character(modifiedPopulation$label[modSpeakers]))
  }
  
  # add column for equivalence labels to modifiedPopulation
  modifiedPopulation$equivalence <- as.factor(eclassLabels)
  
  # calculate in how many agents each equivalence label occurs
  newEquivalenceClusters <- get_equivalence_clusters_(modifiedPopulation, equivalenceLabels, 
                                                     abmName, nrSim)
  if (runMode == "single") {
    cat("\n"); print(dplyr::select_if(newEquivalenceClusters,
                                      colnames(newEquivalenceClusters) %in% equivalenceLabels &
                                        newEquivalenceClusters[1, ] != 0 |
                                        colnames(newEquivalenceClusters) %in% c("ABM", "simulation")));
    cat("\n")
  }
  
  # expand abm.df
  abm.df <- rbind(abm.df, cbind(modifiedPopulation, 
                                ABM = rep(abmName, times = nrow(modifiedPopulation))))
  
  # create the plots during the ABM run
  # plotPath <- file.path(img, paste("plotDuringABM_", abmName, "_", nrSim * interactionsPerSimulation, ".svg", sep = ""))
  # if (plotting != "none" & plotBeforeABM == TRUE) {
  #   svg(filename = plotPath, width = 10, height = 10, onefile = FALSE)
  #   modifiedPlot <- plot_during(modifiedPopulation)
  #   print(modifiedPlot); dev.off()
  #   if (runMode == "single") {
  #     print(modifiedPlot)
  #   }
  # }
  
  # calculate rejection ratio and derived labels per agent
  if (runMode == "single") {
    rejections <- 100 * calc_rejection_ratio(population)
    averageDerivedLabels <- sum(table(modifiedPopulation$speaker, modifiedPopulation$label) != 0) / length(unique(modifiedPopulation$speaker))
    cat("Average rejections:", rejections, "%.\nAverage derived clusters per agent:", 
        averageDerivedLabels, "\n\n")
  }
}
# the ABM run ends here

# make plots after the last interaction in order to see what has changed during the ABM
if (plotting != "none" & plotAfterABM == TRUE) {
  plotPath <- file.path(img, paste("plotAfterABM_", abmName, "_comparison.svg", sep = ""))
  svg(filename = plotPath, width = 10, height = 10, onefile = TRUE)
  plotComparison <- plot_after(abm.df)
  print(plotComparison); dev.off()
  if (runMode == "single") {
    print(plotComparison)
  }
  
  if (splitAndMerge == TRUE) {
    plotPath <- file.path(img, paste("plotAfterABM_", abmName, "_development.svg", sep = ""))
    svg(filename = plotPath, width = 10, height = 10, onefile = TRUE)
    plotEquivalenceLabels <- plot_development(abm.df)
    print(plotEquivalenceLabels); dev.off()
    if (runMode == "single") {
      print(plotEquivalenceLabels)
    }
  }
}

