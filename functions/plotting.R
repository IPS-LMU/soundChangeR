################################################################################
#                                                                              #
# This script contains the following functions that help to plot the data:     #
#                                                                              #
# - calc_axis_limits(firstParam, secondParam)                                  #
# - plot_before(originalPopulation)                                            #
# - plot_during(modifiedPopulation)                                            #
# - plot_after(abm.df)                                                         #
# - plot_development(abm.df)                                                   #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

# set legend title for the plots (except for plots_after())
if (plotColor == "initial") {
  legendTitle <- "Initial Labels"
} else if (plotColor == "equivalence") {
  legendTitle <- "Equivalence Labels"
}

# define plot scale so that colors are pre-defined for all plots
if (length(equivalenceLabels) <= 9) {
  suppressWarnings(plotColors <- brewer.pal(length(equivalenceLabels), "Set1"))
} else {
  plotColors <- brewer.pal(9, "Set1")
  plotColors <- colorRampPalette(plotColors)(length(equivalenceLabels))
}
names(plotColors) <- equivalenceLabels
colScale <- scale_colour_manual(name = legendTitle, values = plotColors, breaks = equivalenceLabels)

# set another scale for plot_after()
if (length(equivalenceLabels) <= 9) {
  suppressWarnings(plotColorsLight <- brewer.pal(length(equivalenceLabels), "Pastel1"))
} else {
  plotColorsLight <- brewer.pal(9, "Pastel1")
  plotColorsLight <- colorRampPalette(plotColorsLight)(length(equivalenceLabels))
}
names(plotColorsLight) <- equivalenceLabels
colScaleAll <- scale_colour_manual(name = legendTitle, values = c(plotColors, plotColorsLight), breaks = equivalenceLabels)


calc_axis_limits <- function(firstParam, secondParam) {
  # This function computes the axisLimits (xlim, ylim) which are needed for the plots.
  # Function calls in plot_before(), plot_during(), and plot_after() (all in this script).
  #
  # Args:
  #    - firstParam: column of plotPopulation
  #    - secondParam: column of plotPopulation
  #
  # Returns:
  #    - axisLimits: a list of 2 vectors, one representing the limits of the x-axis, 
  #      the other representing the limits of the y-axis
  #
  
  # compute minima, maxima, and sd of the two parameters
  maxFirstParam <- max(firstParam)
  minFirstParam <- min(firstParam)
  maxSecondParam <- max(secondParam)
  minSecondParam <- min(secondParam)
  sdFirstParam <- sd(firstParam)
  sdSecondParam <- sd(secondParam)
  
  xlim <- c((floor(minFirstParam) - 2 * sdFirstParam), (ceiling(maxFirstParam) + 2 * sdFirstParam))
  ylim <- c((floor(minSecondParam) - 2 * sdSecondParam), (ceiling(maxSecondParam) + 2 * sdSecondParam))
  
  # set and return axisLimits
  axisLimits <- c(xlim, ylim)
  return(axisLimits)
}


# set params to be plotted and calculate axis limits
firstParam <- columnsToPlot[1]
secondParam <- columnsToPlot[2]
axisLimits <- calc_axis_limits(originalPopulation[, firstParam], originalPopulation[, secondParam])


plot_before <- function(originalPopulation) {
  # This function generates a plot of the original data distribution.
  # Function call in coreABM.R.
  #
  # Args:
  #    - originalPopulation: data.frame generated from population (see coreABM.R)
  #
  # Returns:
  #    - result: a ggplot
  #
  plotPopulation <- originalPopulation
  
  # subset plotPopulation to plotSpeaker
  if (plotSpeaker != "") {
    plotPopulation <- plotPopulation[plotPopulation$speaker == plotSpeaker, ]
  }
  
  # subset plotPopulation to plotSegment
  if (plotSegment != "") {
    plotPopulation <- plotPopulation[plotPopulation$initial == plotSegment, ]
  }
  
  # either plot points
  if (plotting == "points") {
    result <- ggplot(plotPopulation, aes_string(x = firstParam, y = secondParam, color = plotColor)) + 
      geom_point() + facet_wrap(c(plotGroupBy)) + colScale + stat_ellipse() +  
      xlim(axisLimits[1], axisLimits[2]) + ylim(axisLimits[3], axisLimits[4]) + 
      theme(legend.position = "top") + 
      ggtitle("Number of Interactions: 0")
    
    # or formants
  } else if (plotting == "formants") {
    result <- ggplot(plotPopulation, aes_string(x = firstParam, y = secondParam, color = plotColor)) + 
      geom_point() + facet_wrap(c(plotGroupBy)) + stat_ellipse() + colScale + 
      scale_x_reverse(limits = c(axisLimits[2], axisLimits[1])) + 
      scale_y_reverse(limits = c(axisLimits[4], axisLimits[3])) + 
      theme(legend.position = "top") + 
      ggtitle("Number of Interactions: 0")
    
    # or tracks
  } else if(plotting == "tracks") {
    tracks <- reconstruct_tracks(plotPopulation)
    result <- ggplot(tracks[[1]], aes(x = time, y = track, color = initial)) + 
      geom_line(lwd = 1.3) + theme(legend.position = "top") + 
      ggtitle("Number of Interactions: 0") + 
      geom_line(data = tracks[[2]], aes(x = time, y = upper, color = initial), linetype = 2) + 
      geom_line(data = tracks[[2]], aes(x = time, y = lower, color = initial), linetype = 2)
  }
  return(result)
}


plot_during <- function(modifiedPopulation) {
  # This function generates a plot of the modified data distribution.
  # Function call in coreABM.R.
  #
  # Args:
  #    - modifiedPopulation: data.frame generated from population (see coreABM.R)
  #
  # Returns:
  #    - result: a ggplot
  #
  plotPopulation <- modifiedPopulation
  plotTitle <- paste("Number of Interactions:", as.character(nrSim * interactionsPerSimulation), sep = " ")
  
  # subset plotPopulation to plotSpeaker
  if (plotSpeaker != "") {
    plotPopulation <- plotPopulation[plotPopulation$speaker == plotSpeaker, ]
  }
  
  # subset plotPopulation to plotSegment
  if (plotSegment != "") {
    plotPopulation <- plotPopulation[plotPopulation$initial == plotSegment, ]
  }
  
  # either plot points
  if (plotting == "points") {
    result <- ggplot(plotPopulation, aes_string(x = firstParam, y = secondParam, color = plotColor)) + 
      geom_point() + facet_wrap(c(plotGroupBy)) + colScale + stat_ellipse() + 
      xlim(axisLimits[1], axisLimits[2]) + ylim(axisLimits[3], axisLimits[4]) + 
      theme(legend.position = "top") + 
      ggtitle(plotTitle)
    
    # or formants
  } else if (plotting == "formants") {
    result <- ggplot(plotPopulation, aes_string(x = firstParam, y = secondParam, color = plotColor)) + 
      geom_point() + facet_wrap(c(plotGroupBy)) + stat_ellipse() + colScale + 
      scale_x_reverse(limits = c(axisLimits[2], axisLimits[1])) + 
      scale_y_reverse(limits = c(axisLimits[4], axisLimits[3])) + 
      theme(legend.position = "top") + 
      ggtitle(plotTitle)
    
    # or tracks
  } else if(plotting == "tracks") {
    tracks <- reconstruct_tracks(plotPopulation)
    result <- ggplot(tracks[[1]], aes(x = time, y = track, color = initial)) + 
      geom_line(lwd = 1.3) + theme(legend.position = "top") + 
      ggtitle(plotTitle) + 
      geom_line(data = tracks[[2]], aes(x = time, y = upper, color = initial), linetype = 2) + 
      geom_line(data = tracks[[2]], aes(x = time, y = lower, color = initial), linetype = 2)
  }
  return(result)
}


plot_after <- function(abm.df) {
  # This function generates a plot of both the original
  # and the modified data distribution in order to compare them.
  # Function call in coreABM.R.
  #
  # Args:
  #    - abm.df: data.frame generated before and expanded during the ABM run (in coreABM.R)
  #
  # Returns:
  #    - result: a ggplot
  #
  plotPopulation <- abm.df
  plotTitle <- "Comparison of data before and after the ABM run"
  
  # subset plotPopulation to plotSpeaker
  if (plotSpeaker != "") {
    plotPopulation <- plotPopulation[plotPopulation$speaker == plotSpeaker, ]
  }
  
  # subset plotPopulation to plotSegment
  if (plotSegment != "") {
    plotPopulation <- plotPopulation[plotPopulation$initial == plotSegment, ]
  }
  
  # subset plotPopulation to initial and final state of the data
  plotPopulation$condition <- as.character(plotPopulation$condition)
  plotPopulation <- plotPopulation[plotPopulation$condition == "original" | plotPopulation$condition == nrOfSimulations, ]
  plotPopulation$condition <- as.factor(plotPopulation$condition)
  levels(plotPopulation$condition)[levels(plotPopulation$condition) == nrOfSimulations] <- "final"
  
  # use plotColor to generate new column colorBy
  if (plotColor == "equivalence") {
    plotPopulation$colorBy <- as.factor(paste(plotPopulation$equivalence, plotPopulation$condition, sep = "_"))
  } else {
    plotPopulation$colorBy <- as.factor(paste(plotPopulation$initial, plotPopulation$condition, sep = "_"))
  }
  
  # set scale for plotting points and formants
  plotColorsPoints <- suppressWarnings(brewer.pal(length(unique(plotPopulation[grepl("original", plotPopulation$colorBy) == T, ]$colorBy)), "Set1"))
  names(plotColorsPoints) <- unique(plotPopulation[grepl("original", plotPopulation$colorBy) == T, ]$colorBy)[order(unique(plotPopulation[grepl("original", plotPopulation$colorBy) == T, ]$colorBy))]
  plotColorsPointsLight <- suppressWarnings(brewer.pal(length(unique(plotPopulation[grepl("final", plotPopulation$colorBy) == T, ]$colorBy)), "Pastel1"))
  names(plotColorsPointsLight) <- unique(plotPopulation[grepl("final", plotPopulation$colorBy) == T, ]$colorBy)[order(unique(plotPopulation[grepl("final", plotPopulation$colorBy) == T, ]$colorBy))]
  colScalePoints <- scale_colour_manual(name = legendTitle, values = c(plotColorsPoints, plotColorsPointsLight), breaks = plotPopulation$colorBy)
  
  # either plot points
  if (plotting == "points") {
    result <- ggplot(plotPopulation, aes_string(x = firstParam, y = secondParam, color = "colorBy")) + 
      geom_point() + facet_wrap(c(plotGroupBy)) + colScalePoints + stat_ellipse() +  
      xlim(axisLimits[1], axisLimits[2]) + ylim(axisLimits[3], axisLimits[4]) + 
      theme(legend.position = "top") + 
      ggtitle(plotTitle)
    
    # or formants
  } else if (plotting == "formants") {
    result <- ggplot(plotPopulation, aes_string(x = firstParam, y = secondParam, color = "colorBy")) + 
      geom_point() + facet_wrap(c(plotGroupBy)) + stat_ellipse() + colScalePoints + 
      scale_x_reverse(limits = c(axisLimits[2], axisLimits[1])) + 
      scale_y_reverse(limits = c(axisLimits[4], axisLimits[3])) + 
      theme(legend.position = "top") + 
      ggtitle(plotTitle)
    
    # or tracks
  } else if(plotting == "tracks") {
    origTracks <- reconstruct_tracks(plotPopulation[plotPopulation$condition == "original", ])
    origTracks <- origTracks[[1]]
    origTracks$state <- rep("before interactions", nrow(origTracks))
    
    newTracks <- reconstruct_tracks(plotPopulation[plotPopulation$condition == "final", ])
    newTracks <- newTracks[[1]]
    newTracks$state <- rep("after interactions", nrow(newTracks))
    
    tracks <- rbind(origTracks, newTracks)

    result <- ggplot(tracks, aes(x = time, y = track, color = initial, linetype = state)) + 
      geom_line(lwd = 1.3) + theme(legend.position = "top") + ggtitle(plotTitle)
  }
  return(result)
}


plot_development <- function(abm.df) {
  # This function generates a plot of the development of the equivalence
  # labels over the course of the interactions.
  # Function call in coreABM.R.
  #
  # Args:
  #    - abm.df: data.frame generated before and expanded during the ABM run (in coreABM.R)
  #
  # Returns:
  #    - result: a ggplot
  #
  plotPopulation <- abm.df
  
  # subset plotPopulation to plotSpeaker
  if (plotSpeaker != "") {
    plotPopulation <- plotPopulation[plotPopulation$speaker == plotSpeaker, ]
  }
  
  # subset plotPopulation to plotSegment
  if (plotSegment != "") {
    plotPopulation <- plotPopulation[plotPopulation$initial == plotSegment, ]
  }
  
  # pre-define a data.frame called eclass.df
  eclass.df <- setNames(data.frame(matrix(ncol = 3, nrow = length(unique(plotPopulation$condition)) * length(equivalenceLabels))), c("interactions", "equivalence", "frequency"))
  eclass.df$interactions <- rep(unique(plotPopulation$condition), times = length(equivalenceLabels))
  eclass.df$equivalence <- rep(equivalenceLabels, each = length(unique(plotPopulation$condition)))
  
  # complete the data.frame eclass.df
  for (i in 1:nrow(eclass.df)) {
    subsetted <- plotPopulation[plotPopulation$condition == eclass.df[i,1], ]
    eclassPerAgent <- as.data.frame(table(subsetted$equivalence, subsetted$speaker))
    tempCol <- eclassPerAgent[eclassPerAgent$Var1 == eclass.df[i,2], ]
    frequency <- sum(tempCol$Freq != 0)
    eclass.df[i,3] <- frequency
  }
  
  # substitute condition "original" with 0 in order to be able to plot lines
  eclass.df$interactions <- as.character(eclass.df$interactions)
  eclass.df[eclass.df == "original"] <- 0
  eclass.df$interactions <- as.numeric(eclass.df$interactions)
  
  # generate the plot
  result <- ggplot(eclass.df, aes(x = interactions, y = frequency, color = equivalence)) + 
    geom_line(lwd = 1.3) + colScale
  
  return(result)
}

