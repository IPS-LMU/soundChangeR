################################################################################
#                                                                              #
# This script executes a short demo of the ABM using the u-fronting data from  #
# Harrington & Schiel (2017, Language).                                        #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

##### Libraries #####

library(rlist)
library(tidyverse)
library(data.table)
library(ggthemes)
library(stringr)
library(cowplot)

##### Functions #####

source_part <- function(file, start, end, ...) {
  file.lines <- scan(file, what = character(), skip = start-1, nlines = end-start+1, sep = '\n', quiet = T)
  file.lines.collapsed <- paste(file.lines, collapse = '\n')
  source(textConnection(file.lines.collapsed), ...)
}

inv_dct_from_emuR <- function(X, N = 11) {
  0.5 * (sqrt(2) - 1) * X[1] + dtt::dct(c(X, rep(0, N - length(X))), variant = 3)
}

reconstruct_tracks <- function(df, cols) {
  setDT(df)
  valueColumns <- grep(cols, names(df), value = T)
  coeffs <- as.matrix(dplyr::select(df, all_of(valueColumns)))
  
  reconstructedTracks <- c(apply(coeffs, 1, function(x) {
    inv_dct_from_emuR(X = x, N = 21)
  }))
  track.df <- data.table(track = reconstructedTracks, 
                         time = rep(seq(0, 1, length = 21), times = nrow(coeffs))) %>% setDT
  df_long <- df %>% slice(rep(1:n(), each = 21))
  track.df <- cbind(track.df, df_long)
  return(track.df)
}

##### Run simulation #####

wd <- getwd()
dirs <- list.dirs(wd)
if(!file.path(wd, "Rcmd") %in% dirs | !file.path(wd, "functions") %in% dirs) {
  cat("The current working directory does not contain the ABM code (i.e. folders Rcmd/ and functions/). Please use setwd() to navigate to the ABM directory.\n")
  stop()
}

cat("Welcome to a quick demo of the ABM.\nWe will model a /u/-fronting process with data from Harrington & Schiel (2017, Language).\nPress ESC to stop anytime.\n")

# load libraries
cat("Now we will source Rcmd/loadLibraries.R which loads all necessary packages... \n")
source(file.path("Rcmd", "loadLibraries.R"))
cat("Done.\n")

paramsFile <- "data/params.R"
if (!file.exists(paramsFile)) {
  cat("There is no file ", paramsFile, ". Please make sure that there is a file called params.R in data/.\n")
  stop()
} else {
  cat("We will now load the params file... ")
}

source(paramsFile)

cat("Done.\nLet's run the simulation!\nIn progress... ")

con <- file(file.path("Rcmd", "ABMmain.R"), "r")
lines <- readLines(con)
startIdx <- grep('# create root logging directory', lines)
close(con)
source_part(file.path("Rcmd", "ABMmain.R"), startIdx, length(lines))

cat("Done.\nThe results are in ", params$rootLogDir, ".\n", sep = "")

##### Analysis #####

makeAnalysis <- readline(prompt = "Would you like to proceed and take a look at the results? [yes/no] ")
if (makeAnalysis %in% c("no", "n", FALSE, "nein")) {
  cat("Thanks for taking the time to run this demo!")
  stop()
}

# load parameters and input.df for the simulation
logDir <- file.path(params$rootLogDir, params$simulationName)
input.df <- readRDS(file.path(params$rootLogDir, params$simulationName, "input.rds"))
Pcols <- grep("^P[0-9]+$", colnames(input.df), value = TRUE)

if (params$runMode == "single") {
  params$multipleABMRuns <- 1
}

Speaker <- readRDS(file.path(params$rootLogDir, params$simulationName, "1/pop.0.rds")) %>%
  .[, .(speaker, agentID)] %>%
  unique %>%
  .[input.df[, .(speaker, group)] %>% unique, on = "speaker"]

# load population snapshots
p <- rbindlist(lapply(1:params$multipleABMRuns, function(Run_) { 
  rbindlist(lapply(seq(0, params$nrOfSnapshots, by = max(params$nrOfSnapshots)), function(snap) {
    readRDS(file.path(params$rootLogDir, params$simulationName, Run_, paste("pop", snap, "rds", sep = '.'))) %>%
      .[valid == TRUE] %>%
      .[, state := ifelse(snap == 0, "before", "after")]
  }))
}), idcol = "Run")

cat("We have successfully loaded the results. Here is what the snapshots of the population look like:\n\n")
print(p)
cat("\n")

# plot starting and final conditions
cat("First some boxplots to compare the values before and after the interactions. This gives us a first impression of the changes resulting from the interactions.\n")

p$state <- factor(p$state, levels = c("before", "after"))
suppressWarnings(print(ggplot(p %>% pivot_longer(cols = c("P1", "P2", "P3"), names_to = "P")) + 
  aes(x = group, y = value, fill = group, linetype = state) + geom_boxplot() + xlab("") + ylab("") +
  facet_grid(P~initial, scales = "free_y", 
             labeller = labeller(P = as_labeller(function(x) paste0("DCT-", as.numeric(sub("P", "", x)) -1)))) + 
  scale_fill_manual(values = colorblind_pal()(8), name = "group") + 
  scale_linetype_manual(name = "state", values = c("solid", "dotted")) + 
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom",
        strip.text.x = element_text(color = "black"), strip.text.y = element_text(color = "black"))))

# plot the course of the interactions
makePlot <- readline(prompt = "Would you like to proceed? [yes/no] ")
if (makePlot %in% c("no", "n", FALSE, "nein")) {
  cat("Thanks for taking the time to run this demo!")
  stop()
}

cat("The next plots shows the development of the feature values over time, i.e. nr of interactions.\n")

centroids <- rbindlist(lapply(1:params$multipleABMRuns, function(Run_) {
  rbindlist(lapply(seq(0, params$nrOfSnapshots, by = 1), function(snap) {
    readRDS(file.path(params$rootLogDir, params$simulationName, Run_, paste("pop", snap, "rds", sep = '.'))) %>%
      .[valid == TRUE] %>%
      .[, lapply(.SD, mean), by = .(initial, condition, group), .SDcols = Pcols] %>%
      .[, state := ifelse(snap == 0, "before", "after")]
  })) %>%
    setnames("condition", "snapshot")
}), idcol = "Run")

suppressWarnings(print(ggplot(centroids %>% pivot_longer(cols = all_of(Pcols), names_to = "P")) +
  aes(x = snapshot * params$interactionsPerSnapshot, y = value, color = group, group = interaction(group, Run)) +
  geom_line(size = 1.2) + scale_color_manual(values = colorblind_pal()(8), name = "group") +
  facet_grid(P ~ initial, scales = "free_y", 
             labeller = labeller(P = as_labeller(function(x) paste0("DCT-", as.numeric(sub("P", "", x)) -1)))) + 
    xlab("interactions") + ylab("") + 
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom",
        strip.text.x = element_text(color = "black"), strip.text.y = element_text(color = "black"))))

# plot rejection rate
makePlot <- readline(prompt = "Would you like to proceed? [yes/no] ")
if (makePlot %in% c("no", "n", FALSE, "nein")) {
  cat("Thanks for taking the time to run this demo!")
  stop()
}

cat("Looking at the rejection rate for each speaker often helps in understanding why changes happened or didn't happen.\n")
cat("For the purpose of this demo, we will only plot the rejection rate for phoneme /u:/.\n")

# load interaction log
intLog <- rbindlist(lapply(1:params$multipleABMRuns, function(Run_) {
  readRDS(file.path(params$rootLogDir, params$simulationName, Run_, "intLog.rds"))
}), idcol = "Run") %>%
  .[Speaker, on = c(perceiverID = "agentID")]

# choose label to be plotted (try: unique(intLog$perceiverLabel))
label <- "u:"

# compute rejection rate and plot
suppressWarnings(print(ggplot(intLog[perceiverLabel == label, .(Rejection = 1 - (sum(accepted)/.N)), by = .(Run, simulationNr, speaker)] %>%
         complete(speaker, simulationNr, fill = list(Rejection = NA)) %>%
         setDT %>% .[Speaker, on = "speaker"]) +
  aes(x = simulationNr * params$interactionsPerSnapshot, y = Rejection, color = group, group = interaction(group, Run)) +
  geom_line(size = 1.2) + xlab("interactions") + ylab("rejection rate") + facet_wrap(~speaker) + 
  scale_color_manual(values = colorblind_pal()(8), name = "group") + ggtitle(label) +
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom", axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5), strip.text.x = element_text(color = "black"))))

# reconstruct F2 tracks
makePlot <- readline(prompt = "Would you like to proceed? [yes/no] ")
if (makePlot %in% c("no", "n", FALSE, "nein")) {
  cat("Thanks for taking the time to run this demo!")
  stop()
}

cat("Since we are dealing with the first three DCTs derived from the F2 trajectory, we can also reconstruct the latter.\n\n")

# if the values in Pcols are DCTs, we can reconstruct the original trajectories
tracks <- reconstruct_tracks(centroids %>% filter(snapshot %in% c(0, max(snapshot))), "P")

# plot
tracks$state <- factor(tracks$state, levels = c("before", "after"))
suppressWarnings(print(ggplot(tracks) + aes(x = time, y = track, col = group, linetype = state, group = interaction(group, snapshot, Run)) +
  geom_line(size = 1.2) + ylab("reconstructed F2 [bark]") + facet_wrap(~initial) + 
  scale_color_manual(values = colorblind_pal()(8), name = "group") + 
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom",
        strip.text.x = element_text(color = "black"))))

cat("That's it! Thanks for taking the time to go through this demo!")

suppressWarnings(rm(con, dirs, label, lines, makeAnalysis, makePlot, Pcols, SIM_REG_FILENAME, startIdx))

