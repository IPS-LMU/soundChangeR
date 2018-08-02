################################################################################
#                                                                              #
# This script is sourced by the user in order to initiate the ABM. It          #
# basically gets everything ready in order to start the ABM run.               #
#                                                                              #
# Developed by Florian Schiel and Jonathan Harrington                          #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2018, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################


# set path
ABMpath <- getwd()

# load libraries and parameters
source(paste(ABMpath, "Rcmd/loadLibraries.R", sep = "/"))
source(paste(ABMpath, "data/params.R", sep = "/"))

# check if logDir exists already
if (dir.exists(paths = logDirDate)) {
  stop("The log directory ", logDirDate, " already exists!
  Rename or delete the existing directory and run this script again!")
}

# make dir logDir/logDirDate/
system(paste("mkdir ", logDirDate, sep = ""))

# make dir logDirDate/img/
system(paste("mkdir ", img, sep = ""))

# load input dataframe from data/
input.df <- read.table(paste(ABMpath, "data/input.df", sep = "/"), stringsAsFactors = F)

# perform checks on input.df and the parameters set in params.R
source(paste(ABMpath, "Rcmd/performChecks.R", sep = "/"))

# initiate the ABM run(s)
if (runMode == "single") {
  
  # set abmName
  if (plotSpeaker != "" & plotSegment != "") {
    abmName <- paste(plotSpeaker, plotSegment, sep = "_")
  } else if (plotSpeaker != "" & plotSegment == "") {
    abmName <- plotSpeaker
  } else if (plotSegment != "" & plotSpeaker == "") {
    abmName <- plotSegment
  } else if (plotSpeaker == "" & plotSegment == "") {
    abmName <- 0
  }
  
  # run the ABM
  source(paste(ABMpath, "Rcmd/coreABM.R", sep = "/"))
  
  # save abm.df, originalPopulation, and modifiedPopulation to logDirDate
  write.table(abm.df, file.path(logDirDate, "abm.df.txt"), sep = "\t", row.names = FALSE)
  write.table(originalPopulation, file.path(logDirDate, "originalPopulation.txt"), sep = "\t", row.names = FALSE)
  write.table(modifiedPopulation, file.path(logDirDate, "modifiedPopulation.txt"), sep = "\t", row.names = FALSE)
  
} else if (runMode == "multiple") {
  
  # run the ABM for as many times as multipleABMRuns
  for (abmName in 1:multipleABMRuns) {
    
    # print ABM run and start ABM run
    cat("ABM run", abmName, "\n")
    source(paste(ABMpath, "Rcmd/coreABM.R", sep = "/"))
    
    # save abm.df, originalPopulation, and modifiedPopulation to logDirDate
    write.table(abm.df, file.path(logDirDate, paste("abm.df_", abmName, ".txt", sep = "")), sep = "\t", row.names = FALSE)
    write.table(originalPopulation, file.path(logDirDate, paste("originalPopulation_", abmName, ".txt", sep = "")), sep = "\t", row.names = FALSE)
    write.table(modifiedPopulation, file.path(logDirDate, paste("modifiedPopulation_", abmName, ".txt", sep = "")), sep = "\t", row.names = FALSE)
  }
}

# move input.df and params.R to logDirDate/
system(paste("cp ", file.path(ABMpath, "data/input.df"), " ", logDirDate, sep = ""))
cat("Your input.df has been copied to the current logDir:", logDirDate, "\n")
system(paste("cp ", file.path(ABMpath, "data/params.R"), " ", logDirDate, sep = ""))
cat("Your params.R file has been copied to the current logDir:", logDirDate, "\n")

# remove superfluous variables which were created during the ABM run
suppressWarnings(rm(colScale, colScaleAll, modifiedPlot, newEquivalenceClusters,
   originalEquivalenceClusters, originalPlot, plotComparison, plotEquivalenceLabels,
   averageDerivedLabels, axisLimits, dateTime, eclassLabels, firstParam, img, legendTitle,
   logDir, modSpeakers, nrOfAgents, nrSim, origSpeakers, plotColors, plotColorsLight,
   plotPath, rejections, secondParam, speakers))

