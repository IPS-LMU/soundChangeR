################################################################################
#                                                                              #
# This script shows some examples of how to load, analyse, and plot data that  #
# was created during the simulations.                                          #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

library(rlist)
library(tidyverse)
library(data.table)
library(ggthemes)

##### functions #####
source("functions/simulations.R")

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


##### choose simulation to be analysed #####

rootLogDir <- "./logs"

# filter all simulations in rootLogDir for certain parameters; all parameters can be used in this search
simulationName <- filter_simulations(rootLogDir,
                                     initialMemoryResampling == T
                                     & memoryIntakeStrategy == "mahalanobisDistance"
                                     & rememberOwnTokens == F)

# check that there is one simulation that fits the parameters
if (length(simulationName) > 1) {
  simulationName <- simulationName[1]
} else if (length(simulationName) == 0) {
  stop("There is no simulation that fits the given parameters.")
}

# load parameters and input.df for the simulation
logDir <- file.path(rootLogDir, simulationName)
params  <- get_params(rootLogDir, simulationName)
input.df <- readRDS(file.path(rootLogDir, simulationName, "input.rds"))
Pcols <- grep("^P[0-9]+$", colnames(input.df), value = TRUE)

if (params$runMode == "single") {
  params$multipleABMRuns <- 1
}

Speaker <- readRDS(file.path(rootLogDir, simulationName, "1/pop.0.rds")) %>%
  .[, .(speaker, agentID)] %>%
  unique %>%
  .[input.df[, .(speaker, group)] %>% unique, on = "speaker"]

### 1. rejection rate

# load interaction log
intLog <- rbindlist(lapply(1:params$multipleABMRuns, function(Run_) {
  readRDS(file.path(rootLogDir, simulationName, Run_, "intLog.rds"))
}), idcol = "Run") %>%
  .[Speaker, on = c(perceiverID = "agentID")]

# choose label to be plotted (try: unique(intLog$perceiverLabel))
label <- "u:"

# compute rejection rate and plot
ggplot(intLog[perceiverLabel == label, .(Rejection = 1 - (sum(accepted)/.N)), by = .(Run, simulationNr, speaker)] %>%
         complete(speaker, simulationNr, fill = list(Rejection = NA)) %>%
         setDT %>% .[Speaker, on = "speaker"]) +
  aes(x = simulationNr * params$interactionsPerSnapshot, y = Rejection, color = group, group = interaction(group, Run)) +
  geom_line(size = 1.2) + xlab("interactions") + ylab("rejection rate") + facet_wrap(~speaker) + 
  scale_color_manual(values = colorblind_pal()(8), name = "group") + ggtitle(label) +
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom", axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5), strip.text.x = element_text(color = "black"))

### 2. centroids over interactions

# load population snapshots and compute mean values per snapshot
# adapt skip_ if params$nrOfSnapshots is large
skip_ <- 1
centroids <- rbindlist(lapply(1:params$multipleABMRuns, function(Run_) {
  rbindlist(lapply(seq(0, params$nrOfSnapshots, by = skip_), function(snap) {
    readRDS(file.path(rootLogDir, simulationName, Run_, paste("pop", snap, "rds", sep = "."))) %>%
      .[valid == TRUE] %>%
      .[, lapply(.SD, mean), by = .(initial, condition, group), .SDcols = Pcols]
  })) %>%
    setnames("condition", "snapshot")
}), idcol = "Run")

ggplot(centroids %>% pivot_longer(cols = all_of(Pcols), names_to = "P")) +
  aes(x = snapshot * params$interactionsPerSnapshot, y = value, color = group, group = interaction(group, Run)) +
  geom_line(size = 1.2) + scale_color_manual(values = colorblind_pal()(8), name = "group") +
  facet_grid(P ~ initial, scales = "free_y") + xlab("interactions") + ylab("") + 
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom",
        strip.text.x = element_text(color = "black"), strip.text.y = element_text(color = "black"))

### 3. reconstruct tracks from DCTs (if applicable)

# if the values in Pcols are DCTs, we can reconstruct the original trajectories
tracks <- reconstruct_tracks(centroids %>% filter(snapshot %in% c(0, max(snapshot))), "P")

# plot
ggplot(tracks) + aes(x = time, y = track, col = group, linetype = as.factor(snapshot), group = interaction(group, snapshot, Run)) +
  geom_line(size = 1.2) + ylab("reconstructed track") + facet_wrap(~initial) + 
  scale_linetype_discrete(name = "snapshot") +
  scale_color_manual(values = colorblind_pal()(8), name = "group") + 
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "bottom",
        strip.text.x = element_text(color = "black"))

### 4. individual agents (2D)

# load first and last population snapshots
pop <- rbindlist(lapply(1:params$multipleABMRuns, function(Run_) { 
  rbindlist(lapply(seq(0, params$nrOfSnapshots, by = max(params$nrOfSnapshots)), function(snap) {
    readRDS(file.path(rootLogDir, simulationName, Run_, paste("pop", snap, "rds", sep = "."))) %>%
      .[valid == TRUE] %>%
      .[, state := ifelse(snap == 0, "before", "after")]
  }))
}), idcol = "Run")

# plot all agents' memories before and after interactions only for Run 1
pop$state <- factor(pop$state, levels = c("before", "after"))
ggplot(pop[Run == 1,]) + stat_ellipse(aes(x = P1, y = P3, col = speaker)) + facet_grid(initial~state) + 
  theme_light(base_size = 16, base_family = "Verdana") +
  theme(text = element_text(size = 15), legend.position = "none",
        strip.text.y = element_text(color = "black"), strip.text.x = element_text(color = "black"))

