################################################################################
#                                                                              #
# This script contains all checks that are carried out before the ABM starts.  #
# It is sourced in master.R.                                                   #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg                                                #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


########################## Basic Parameter Checks ##########################

# check if all params of class character are set
if (memoryIntakeStrategy != "maxPosteriorProb" & memoryIntakeStrategy != "mahalanobisDistance" |
    memoryRemovalStrategy != "timeDecay" & memoryRemovalStrategy != "outlierRemoval" |
    runMode != "single" & runMode != "multiple" |
    plotting != "points" & plotting != "formants" & plotting != "tracks" & plotting != "none" |
    plotGroupBy != "group" & plotGroupBy != "speaker" |
    plotColor != "equivalence" & plotColor != "initial" |
    class(logDir) != "character" | logDir == "" |
    class(dateTime) != "character" | dateTime == "" |
    class(logDirDate) != "character" | logDirDate == "" |
    class(img) != "character" | img == "" |
    interactionPartners != "random" & interactionPartners != "betweenGroups" & interactionPartners != "withinGroups" |
    class(plotSpeaker) != "character" | class(plotSegment) != "character") {
  stop("One of the following parameters has not been set to an adequate value, 
  or there is a typo, or the param is not of class character:
  memoryIntakeStrategy, memoryRemovalStrategy, runMode, plotting, plotGroupBy, 
  plotColor, logDir, dateTime, logDirDate, img, interactionPartners, plotSpeaker.
  Please check params.R.")
}

# check if all params of class numeric are set to adequate values
if (class(nrOfSnapshots) != "numeric" | 
    class(nrOfSnapshots) == "numeric" & nrOfSnapshots < 1 | 
    class(interactionsPerSnapshot) != "numeric" | 
    class(interactionsPerSnapshot) == "numeric" & interactionsPerSnapshot < 1 | 
    class(splitAndMergeInterval) != "numeric" | 
    class(splitAndMergeInterval) == "numeric" & splitAndMergeInterval < 1 | 
    class(mahalanobisThreshold) != "numeric" | 
    class(multipleABMRuns) != "numeric" | 
    class(multipleABMRuns) == "numeric" & multipleABMRuns < 0) {
  stop("One of the following parameters has not been set to an adequate value 
  or is not of class numeric:
  nrOfSnapshots, interactionsPerSnapshot, splitAndMergeInterval, mahalanobisThreshold, multipleABMRuns.
  Please check params.R.")
}

# check if params of class numeric are set to full numbers
if (nrOfSnapshots %% 1 != 0 | interactionsPerSnapshot %% 1 != 0 | 
    splitAndMergeInterval %% 1 != 0 | multipleABMRuns %% 1 != 0) {
  stop("One of the following parameters has not been set to a full number:
  nrOfSnapshots, interactionsPerSnapshot, splitAndMergeInterval, multipleABMRuns.
  Please check params.R.")
}

# check if all params of class logical are set
if (class(splitAndMerge) != "logical" |
    class(plotBeforeABM) != "logical" |
    class(plotAfterABM) != "logical" |
    class(doSplitAndMergeBeforeABM) != "logical" |
    class(splitAndMerge) != "logical") {
  stop("One of the following parameters has not been set to a logical value (FALSE or TRUE):
  splitAndMerge, plotBeforeABM, plotAfterABM, doSplitAndMergeBeforeABM.
  Please check params.R.")
}


########################## input.df Checks ##########################


# check if all necessary columns exist in input.df
necessaryCols <- c("speaker", "initial", "labels", "word", "group")
colList <- NULL
for (col in necessaryCols) {
  if (!col %in% colnames(input.df)) {
    colList <- c(colList, col)
  }
}
if (length(colList) == 1) {
  print(colList)
  stop("The column printed above is missing from input.df.
       Please make sure that all necessary columns are present in input.df.
       If in doubt, refer to param_expl/param_explanations.pdf.")
} else if (length(colList) > 1) {
  print(colList)
  stop("The columns printed above are missing from input.df.
       Please make sure that all necessary columns are present in input.df.
       If in doubt, refer to param_expl/param_explanations.pdf.")
}

# check if all columns are of the correct class, else convert them
if (class(input.df$speaker) != "character") {
  input.df$speaker <- as.character(input.df$speaker)
}
if (class(input.df$initial) != "character") {
  input.df$initial <- as.character(input.df$initial)
}
if (class(input.df$label) != "character") {
  input.df$label <- as.character(input.df$label)
}
if (class(input.df$word) != "character") {
  input.df$word <- as.character(input.df$word)
}
if (class(input.df$group) != "character") {
  input.df$group <- as.character(input.df$group)
}
columnsNamedP <- grep("P", names(input.df), value = T)
for (column in columnsNamedP) {
  if (class(input.df[colnames(input.df) == column]) != "numeric") {
    input.df[, column] <- as.numeric(input.df[, column])
  }
}


########################## Subset input.df ##########################


# check if subsetSpeakers are all in input.df
if(! is.null(subsetSpeakers)) {
  if (class(subsetSpeakers) != "character") {
    stop("The parameter subsetSpeakers is neither set to NULL
         nor to a vector of characters. Please adapt this parameter in params.R.")
  }
  speakersNotInDf <- NULL
  for (i in 1:length(subsetSpeakers)) {
    if (!subsetSpeakers[i] %in% input.df$speaker) {
      speakersNotInDf <- c(speakersNotInDf, subsetSpeakers[i])
    }
  }
  if (length(speakersNotInDf) == 1) {
    print(speakersNotInDf)
    stop("The speaker printed above is not part of the input.df.
         Please adapt parameter subsetSpeakers in params.R")
  } else if (length(speakersNotInDf) > 1) {
    print(speakersNotInDf)
    stop("The speakers printed above are not part of the input.df.
         Please adapt parameter subsetSpeakers in params.R")
  }
}

# check if subsetSegments are all in input.df
if(! is.null(subsetSegments)) {
  if (class(subsetSegments) != "character") {
    stop("The parameter subsetSegments is neither set to NULL
  nor to a vector of characters. Please adapt this parameter in params.R.")
  }
  segmentsNotInDf <- NULL
  for (i in 1:length(subsetSegments)) {
    if (!subsetSegments[i] %in% input.df$initial) {
      segmentsNotInDf <- c(segmentsNotInDf, subsetSegments[i])
    }
  }
  if (length(segmentsNotInDf) == 1) {
    print(segmentsNotInDf)
    stop("The segment printed above is not part of the input.df.
  Please adapt parameter subsetSegments in params.R")
  } else if (length(segmentsNotInDf) > 1) {
    print(segmentsNotInDf)
    stop("The segments printed above are not part of the input.df.
  Please adapt parameter subsetSegments in params.R")
  }
}

# subset input.df to subsetSpeakers
if (! is.null(subsetSpeakers)) {
  input.df <- input.df[input.df$speaker %in% subsetSpeakers, ]
}

# subset input.df to subsetSegments
if (! is.null(subsetSegments)) {
  input.df <- input.df[input.df$initial %in% subsetSegments, ]
}


########################## More Parameter Checks ##########################


# check memorySizeStrategy: either "maxAgent" or numeric (also see below)
if (memorySizeStrategy != "maxAgent" & class(memorySizeStrategy) != "numeric") {
  stop("The parameter memorySizeStrategy is neither set to 'maxAgent' nor to a 
  full positive number. Please choose a valid option for this parameter in params.R.")
}

# check memorySizeStrategy: maxMemorySize must be higher than the largest available memory
if (memorySizeStrategy == "maxAgent") {
  maxMemorySize <- as.numeric(max(plyr::count(input.df$speaker)$freq))
} else if (class(memorySizeStrategy) == "numeric" & memorySizeStrategy %% 1 == 0) {
  maxMemorySize <- memorySizeStrategy
  maxAgentMemory <- as.numeric(max(plyr::count(input.df$speaker)$freq))
  if (maxMemorySize < maxAgentMemory) {
    print(maxAgentMemory)
    stop("You chose a memory size that is lower than that of the largest memory 
  available in one of your agents (see maximal memory size printed above).
  Please adapt parameter memorySizeStrategy in param.R to be equal to or exceed the largest memory.")
  }
} else {
  stop("Please set parameter memorySizeStrategy in params.R to a valid value
  (either 'maxAgent' or a full positive number.")
}

# check if param columnsToPlot contains columns that are in input.df
columnsToPlotNotInDf <- NULL
for (i in 1:length(columnsToPlot)) {
  if (!columnsToPlot[i] %in% columnsNamedP) {
    columnsToPlotNotInDf <- c(columnsToPlotNotInDf, columnsToPlot[i])
  }
}
if (length(columnsToPlotNotInDf) == 1) {
  print(columnsToPlotNotInDf)
  stop("The column printed above is not present in input.df.
  Please adapt parameter columnsToPlot in params.R.")
} else if (length(columnsToPlotNotInDf) > 1) {
  print(columnsToPlotNotInDf)
  stop("The columns printed above are not present in input.df.
  Please adapt parameter columnsToPlot in params.R.")
}

# check if number of columns to plot is 2 (because only 2-dimensional plotting is possible)
if (length(columnsToPlot) != 2) {
  stop("It is only possible to create two-dimensional plots.
  Please adapt parameter columnsToPlot in params.R to contain exactly two elements.")
}

# check if speakerProb and listenerProb consist of one value per agent
# if speakerProb and/or listenerProb is/are not set, set them to equal probabilities
nrOfAgents <- length(unique(input.df$speaker))
if (! is.null(speakerProb)) {
  if (length(speakerProb) != nrOfAgents) {
    stop("The parameter speakerProb does not contain one value per agent.
  Please adapt this parameter in params.R.")
  }
} else {
  speakerProb = rep(1/nrOfAgents, times = nrOfAgents)
}
if (! is.null(listenerProb)) {
  if (length(listenerProb) != nrOfAgents) {
    stop("The parameter listenerProb does not contain one value per agent.
  Please adapt this parameter in params.R.")
  }
} else {
  listenerProb = rep(1/nrOfAgents, times = nrOfAgents)
}

# check if speakerProb and listenerProb contain values between 0 and 1
if (! is.null(speakerProb)) {
  for (i in 1:length(speakerProb)) {
    if (speakerProb[i] < 0 | speakerProb[i] > 1) {
      stop("There are values in the parameter speakerProb that are not between 0 and 1.
  Please adapt this parameter in params.R")
    }
  }
}
if (! is.null(listenerProb)) {
  for (i in 1:length(listenerProb)) {
    if (listenerProb[i] < 0 | listenerProb[i] > 1) {
      stop("There are values in the parameter listenerProb that are not between 0 and 1.
  Please adapt this parameter in params.R")
    }
  }
}

# check if speakerProb and listenerProb contain appropriate values 
# when taking interactionPartners into account
groupList <- unique(input.df$group)
if (interactionPartners == "random") {
  if (length(speakerProb[speakerProb > 0]) < 1 | length(listenerProb[listenerProb > 0]) < 1) {
    stop("At least one of the numbers in each speakerProb and listenerProb must 
      be higher than 0. Please adapt these parameters in params.R.")
  }
} else if (interactionPartners == "withinGroups" | interactionPartners == "betweenGroups") {
  for (group in groupList) {
    speakersInGroup <- unique(input.df[input.df$group == group, ]$speaker)
    alphabeticSpeakers <- unique(input.df$speaker)[order(unique(input.df$speaker))]
    indices <- match(speakersInGroup, alphabeticSpeakers)
    speakerProbsAtIndices <- speakerProb[indices]
    listenerProbsAtIndices <- listenerProb[indices]
    if (length(speakerProbsAtIndices[speakerProbsAtIndices > 0]) < 1 | 
        length(listenerProbsAtIndices[listenerProbsAtIndices > 0]) < 1) {
      stop("For interactionPartners == 'withinGroups' or interactionPartners == 'betweenGroups', 
  there must be at least one speaker and one listener for each group with a 
  speaking/listening probability higher than 0. 
  Please adapt speakerProb and listenerProb accordingly in params.R.")
    }
  }
} 

# check if plotSpeaker is speaker in input.df
speakers <- unique(as.character(input.df$speaker))
if (plotSpeaker != "" & sum(speakers == plotSpeaker) != 1) {
  print(plotSpeaker)
  stop("The speaker printed above is not part of input.df.
  Please adapt parameter plotSpeaker in params.R.")
}

# check if plotSegment is phoneme label in input.df
initials <- unique(as.character(input.df$initial))
if (plotSegment != "" & sum(initials == plotSegment) != 1) {
  print(plotSegment)
  stop("The segment printed above is not part of input.df.
  Please adapt parameter plotSegment in params.R.")
}


########################## Other Checks ##########################


# check if every speaker has enough word repetitions (i.e. n+1, n = nr of dimensions)
wordCountPerSpeaker <- as.data.frame(t(table(input.df$word, input.df$speaker)))
listOfFrequencies <- NULL
listOfSpeakers <- NULL
listOfWords <- NULL
for (i in 1:nrow(wordCountPerSpeaker)) {
  if (wordCountPerSpeaker[i, ]$Freq < (length(columnsNamedP) + 1)) {
    listOfFrequencies <- c(listOfFrequencies, as.numeric(wordCountPerSpeaker[i, ]$Freq))
    listOfSpeakers <- c(listOfSpeakers, as.character(wordCountPerSpeaker[i, ]$Var1))
    listOfWords <- c(listOfWords, as.character(wordCountPerSpeaker[i, ]$Var2))
  }
}
notEnoughRepetitions <- data.frame(word = listOfWords, speaker = listOfSpeakers, freq = listOfFrequencies)
if (empty(notEnoughRepetitions) == F) {
  print(notEnoughRepetitions)
  cat("\nThere are not enough repetitions of the words shown above for the 
corresponding speaker. There need to be at least n + 1 repetitions, n being 
the number of the dimensions in the ABM (i.e. number of P-columns in input.df).\n\n")
}


suppressWarnings(rm(notEnoughRepetitions, wordCountPerSpeaker, alphabeticSpeakers, col,
                    colList, column, columnsNamedP, columnsToPlotNotInDf, group, groupList,
                    i, indices, initials, listenerProbsAtIndices, listOfFrequencies,
                    listOfSpeakers, listOfWords, necessaryCols, nrOfAgents, speakerProbsAtIndices,
                    speakers, speakersInGroup, speakersNotInDf))

