################################################################################
#                                                                              #
# Demo simulation with multiple-phoneme dataset.                               #
#                                                                              #
# Developers: Jonathan Harrington, Michele Gubian, Johanna Cronenberg          #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

##### Libraries

library(stringr)
library(ggplot2)
library(data.table)
library(dplyr)
library(cowplot)

##### Functions

source_part <- function(file, start, end, ...) {
  file.lines <- scan(file, what = character(), skip = start-1, nlines = end-start+1, sep = '\n', quiet = T)
  file.lines.collapsed <- paste(file.lines, collapse = '\n')
  source(textConnection(file.lines.collapsed), ...)
}

inv_dct_from_emuR <- function(X, N = 11) {
  0.5 * (sqrt(2) - 1) * X[1] + dtt::dct(c(X, rep(0, N - length(X))), variant = 3)
}

reconstruct_tracks <- function(df, cols) {
  valueColumns <- grep(cols, names(df), value = T)
  coeffs <- as.matrix(dplyr::select(df, valueColumns))
  
  reconstructedTracks <- c(apply(coeffs, 1, function(x) {
    inv_dct_from_emuR(X = x, N = 21)
  }))
  
  track.df <- data.frame(track = reconstructedTracks, time = rep(seq(0, 1, length = 21), times = nrow(coeffs)),
                         initial = rep(as.character(df$initial), each = 21), label = rep(as.character(df$label), each = 21),
                         word = rep(as.character(df$word), each = 21), speaker = rep(as.character(df$speaker), each = 21),
                         group = rep(as.character(df$group), each = 21), nrInteractions = rep(as.numeric(df$nrInteractions), each = 21))
  result <- track.df %>% group_by(time, initial, group, nrInteractions) %>% dplyr::summarise(mean = mean(track), sd1 = mean+1.96*sd(track), sd2 = mean-1.96*sd(track))
  return(result)
}

##### Run simulation

cat("Welcome to a quick demo of the ABM.\nWe will model a /u/-fronting process with data from Harrington & Schiel (2017, Language).\nPress ESC to stop anytime.\n")

# set path to code directory
ABMpath <- ""
while(dir.exists(ABMpath) == F) {
  enteredPath <- readline(prompt = "Enter path to code directory, e.g. '/homes/myName/Downloads/ABM' (in quotes): ")
  ABMpath <- str_sub(enteredPath, 2, -2)
  if(str_sub(ABMpath, -1) == "/") {
    ABMpath <- str_sub(ABMpath, 1, -2)
  }
  dirs <- list.dirs(ABMpath)
  if(!file.path(ABMpath, "Rcmd") %in% dirs | !file.path(ABMpath, "functions") %in% dirs) {
    cat("The entered directory does not contain the ABM code (i.e. folders Rcmd/ and functions/). Try again or press ESC.\n")
    ABMpath <- ""
  }
}

setwd(ABMpath)

# load libraries
cat("Thanks! Now we will run Rcmd/loadLibraries.R which loads all necessary packages... ")
source(file.path("Rcmd", "loadLibraries.R"))
cat("Done.\n")

cat("Next, you will be asked whether you want to model a single phoneme, i.e. /u/.\nIf not, the simulation will use all three phonemes of the original study, i.e. /i, ju, u/.\n")

# load correct params file
answer <- F
while(answer == F) {
  single <- readline(prompt = "Do you want to model a single phoneme? [yes/no] ")
  if (single %in% c("yes", "y", TRUE, "ja", "j", "ok")) {
    paramsFile <- file.path(ABMpath, "data/params.R")
    if (!file.exists(paramsFile)) {
      cat("There is no file ", paramsFile, ".\n")
      stop()
    } else {
      cat("Thanks. We will now load the params file for the single phoneme simulation... ")
      answer <- T
    }
  } else if (single %in% c("no", "n", FALSE, "nein")) {
    paramsFile <- file.path(ABMpath, "data/params_multiple_phoneme.R")
    if (!file.exists(paramsFile)) {
      cat("There is no file ", paramsFile, ".\n")
      stop()
    } else {
      cat("Thanks. We will now load the params file for the multiple phonemes simulation... ")
      answer <- T
    }
  } else {
    cat("Please enter a valid answer (yes, no) or press ESC.\n")
  }
}

## enter any params file
# paramsFile <- ""
# while(file.exists(paramsFile) == F) {
#   enteredFile <- readline(prompt = "Enter path to params file (in quotes): ")
#   paramsFile <- str_sub(enteredFile, 2, -2)
#   if(str_sub(paramsFile, -1) == "/") {
#     paramsFile <- str_sub(paramsFile, 1, -2)
#   }
#   if(str_sub(paramsFile, 1, 1) == ".") {
#     paramsFile <- str_sub(paramsFile, 3)
#   }
# }

source(paramsFile)

cat("Done.\nLet's run the simulation!\nIn progress... ")

source_part(file.path("Rcmd", "ABMmain.R"), 21, 65)

cat("Done.\nThe results are in ", params$rootLogDir, ".\n", sep = "")

##### Analyis

makeAnalysis <- readline(prompt = "Would you like to proceed and take a look at the results? [yes/no] ")
if (makeAnalysis %in% c("no", "n", FALSE, "nein")) {
  stop()
}

# load population snapshots
filePaths <- Sys.glob(file.path(params$rootLogDir, params$simulationName, "pop.*.rds"))
p <- rbindlist(lapply(filePaths, function(x) {
  r <- readRDS(x)
  r[, `:=` (nrInteractions = condition * params$interactionsPerSnapshot,
            DCT0 = P1, DCT1 = P2, DCT2 = P3,
            P1 = NULL, P2 = NULL, P3 = NULL)]
  r[valid == TRUE,]
}))

cat("We have successfully loaded the results. Here is what the snapshots of the population look like:\n\n")
print(p)
cat("\n")

# plot starting and final conditions
cat("First some boxplots to compare the values before and after the interactions. This gives us a first impression of the changes resulting from the interactions.\n")

df1 <- p[nrInteractions %in% c(0, max(p$nrInteractions)),]
df1$nrInteractions <- as.factor(df1$nrInteractions)
p1 <- ggplot(df1) + aes(x = group, y = DCT0, fill = group, linetype = nrInteractions) + geom_boxplot() + 
  facet_wrap(~initial) + theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "none", strip.text.x = element_text(color = "black")) + xlab("")
p2 <- ggplot(df1) + aes(x = group, y = DCT1, fill = group, linetype = nrInteractions) + geom_boxplot() + 
  facet_wrap(~initial) + theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "none", strip.text.x = element_text(color = "black")) + xlab("")
p3 <- ggplot(df1) + aes(x = group, y = DCT2, fill = group, linetype = nrInteractions) + geom_boxplot() + 
  facet_wrap(~initial) + theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "bottom", strip.text.x = element_text(color = "black")) + xlab("")

suppressWarnings(print(plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(0.85, 0.85, 1))))

# plot the course of the interactions
makePlot <- readline(prompt = "Would you like to proceed? [yes/no] ")
if (makePlot %in% c("no", "n", FALSE, "nein")) {
  stop()
}

cat("The next plots shows the development of the feature values over time, i.e. nr of interactions.\n")

df.avg1 <- p %>% group_by(group, initial, nrInteractions) %>% 
  dplyr::summarise(DCT0 = mean(DCT0), DCT1 = mean(DCT1), DCT2 = mean(DCT2))

p1 <- ggplot(df.avg1) + aes(y = DCT0, x = nrInteractions, col = group) + 
  geom_line(size = 1.2) + ylab("DCT0") + xlab("") + facet_grid(~initial) + 
  theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "none", strip.text.x = element_text(color = "black"))
p2 <- ggplot(df.avg1) + aes(y = DCT1, x = nrInteractions, col = group) + 
  geom_line(size = 1.2) + ylab("DCT1") + xlab("") + facet_grid(~initial) + 
  theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "none", strip.text.x = element_text(color = "black"))
p3 <- ggplot(df.avg1) + aes(y = DCT2, x = nrInteractions, col = group) + 
  geom_line(size = 1.2) + ylab("DCT2") +  xlab("") + facet_grid(~initial) + 
  theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "bottom", strip.text.x = element_text(color = "black"))

suppressWarnings(print(plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(0.85, 0.85, 1))))

# rejection rate
makePlot <- readline(prompt = "Would you like to proceed? [yes/no] ")
if (makePlot %in% c("no", "n", FALSE, "nein")) {
  stop()
}

cat("Looking at the rejection rate, by group or speaker, often helps in understanding why changes happened or didn't happen.\n")

intLog <- readRDS(file.path(params$rootLogDir, params$simulationName, "intLog.rds"))
pop <- readRDS(file.path(params$rootLogDir, params$simulationName, "pop.0.rds"))

groups <- pop %>% group_by(agentID, group) %>% dplyr::summarise()
intLog <- groups %>% plyr::rename(c("agentID"="perceiverID")) %>% dplyr::left_join(intLog, by = "perceiverID")
rate.df <- intLog %>% dplyr::mutate(perceiverLabel = ifelse(word %in% c("seep", "heed", "keyed", "feed"), "i:", 
                                                            ifelse(word %in% c("soup", "who'd", "cooed", "food"), "u:", "ju:"))) %>%
  group_by(simulationNr, perceiverLabel, group) %>% dplyr::summarise(rate = 1-(sum(accepted)/n()))

print(ggplot(rate.df) + aes(x = simulationNr * params$interactionsPerSnapshot, y = rate, col = group) + 
  geom_line(size = 1.2) + xlab("number of interactions") + ylab("rejection rate") + 
  facet_wrap(~perceiverLabel) + theme_light(base_size = 16, base_family = "Verdana") + 
  theme(legend.position = "bottom", strip.text.x = element_text(color = "black")))

# reconstruct F2 tracks
makePlot <- readline(prompt = "Would you like to proceed? [yes/no] ")
if (makePlot %in% c("no", "n", FALSE, "nein")) {
  stop()
}

cat("Since we are dealing with the first three DCTs derived from the F2 trajectory, we can also reconstruct the latter:\n")

tracks <- reconstruct_tracks(p[nrInteractions %in% c(0, max(p$nrInteractions))], "DCT")
print(ggplot(tracks) + aes(x = time) +
  geom_line(aes(y = mean, col = group, linetype = as.factor(nrInteractions)), size = 1.2) +
  theme_light(base_size = 16, base_family = "Verdana") + theme(legend.position = "bottom", strip.text.x = element_text(color = "black")) + 
  ylab("reconstructed F2 [bark]") + facet_wrap(~initial) + scale_linetype_discrete(name = "nr of interactions"))

cat("Thanks for taking the time to go through this demo!")

