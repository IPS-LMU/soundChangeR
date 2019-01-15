# Analysis of REAL and ABM simulation data 

library(data.table)
library(dplyr)
library(car)
library(emmeans)
library(lme4)
library(proxy)
library(tidyr)
library(ggplot2)

rootLogDir <- "/vdata/Projects/ABM/simulations/Michele/Antarctica"
simulationName <- "ABM20181204185316"
params  <- getParams(rootLogDir, simulationName)
logDir <- file.path(rootLogDir, simulationName)
# human data
sessions.df <- fread(params[['inputDataFile']], stringsAsFactors = F)
setFeatureNames(sessions.df, Cs(kF10, kF11, kF12, kF20, kF21, kF22))
sessions.df[, origin := "REAL"]
sessions.df[, valid := TRUE]
Pcols <- paste0("P", 1:6)

# simulation data
# origin == "ABM", session == 0 are all the same, pick one
pop.0 <- readRDS(file.path(logDir, 1, paste("pop", 0, "rds", sep = "."))) %>%
  .[, session := 0] %>%
  .[, Run := 0]
# define origin == "ABM", session == 1 to be at 5000 interactions 
finalSnapshot <- 5 # == 5000 / interactionsPerSnapshot

# let agents produce at their final stage
# first collect their memory snapshots 
pop.1.list <- lapply(seq_len(params$multipleABMRuns), function (Run) {
  readRDS(file.path(logDir, Run, paste("pop", finalSnapshot, "rds", sep = "."))) %>%
    convert_pop_dt_to_list
})
# produce 1000 tokens per agent
# this simulates the recording at session 1 based on the memory of agents at that stage
nTokensPerSpeaker <- 1000
require(parallel)
numCores <- detectCores() - 1
cl <- makeCluster(numCores, type = "FORK")
clusterSetRNGStream(cl)
pop.2 <- rbindlist(parLapply(cl, seq_len(params$multipleABMRuns), function (Run) { 
  rbindlist(lapply(pop.1.list[[Run]] %>% seq_along(), function(id) {
    agent <- pop.1.list[[Run]][[id]]
    rbindlist(lapply(seq_len(nTokensPerSpeaker), function(i) {
      token <- produce_token(agent)
      cbind(token$features, token$labels)
    })) %>% .[, `:=`(agentID = agent$agentID, speaker = agent$speaker, group = agent$group)]
  })) %>% .[, Run := Run]
})) %>% .[, `:=`(session = 1, valid = TRUE)] %>% setnames(Pcols %>% sub("^P", "V", .), Pcols)
stopCluster(cl)  
# collect all in one data.table 
pop <- rbindlist(list(pop.0, pop.2), use.names = TRUE, fill = TRUE) %>%
  .[, origin := "ABM"]
pop[, speaker := as.character(speaker)]
REALandABM.df <- rbindlist(list(sessions.df, pop), use.names = TRUE, fill = TRUE) %>%
  .[, origin := factor(origin, levels = Cs(REAL, ABM))]




# real data + all runs collected here
# this is the version where ABM session 1 is based on the agents' memory at 5000 interactions
REALandABM.df <- fread(file.path(logDir, "REALandABM.df"))
# alternatively, ABM session 1 based on production after 5000 interactions
REALandABM.df <- fread(file.path(logDir, "REALandABM.production.df"))
REALandABM.df[, `:=`(speaker = factor(speaker),
                     session = factor(session),
                     origin = factor(origin, levels = c("REAL", "ABM")))]

# choose which acoustic features to use to compute distances
Pcols <- paste0("P", 1:6)
Icols <- Pcols # dimensions used to compute projection on i:-I 
Ucols <-  paste0("P", 4:6) # dimensions used to compute /*u/ distance from i:

# centroid of i: computed on Icols
i.mean.Icols <- REALandABM.df[origin == "REAL" & label == "i:",
                            lapply(.SD, mean), .SDcols = Icols]
# centroid of I computed on Icols
I.mean.Icols <- REALandABM.df[origin == "REAL" & label == "I",
                            lapply(.SD, mean), .SDcols = Icols]
# centroid of i: computed on Ucols
i.mean.Ucols <- REALandABM.df[origin == "REAL" & label == "i:",
                            lapply(.SD, mean), .SDcols = Ucols]

i.mean.F20 <- REALandABM.df[origin == "REAL" & label == "i:", mean(P4)]


# compute distance to i: (for /u/-phonemes)
REALandABM.df[label %in% c("i:", "ju", "u", "ou"),
              `:=`(euclDist_i = proxy::dist(.SD, i.mean.Ucols)),
                   # mahalDist_i = mahalanobis(.SD, i.Gauss.Ucols$mean, i.Gauss.Ucols$cov)),
              .SDcols = Ucols]

REALandABM.df[label %in% c("i:", "ju", "u", "ou"),
              proj_F20_i := P4 - i.mean.F20]

# function to compute projection on a segment (based on Jonathan's code)
projectOnSegment <- function(data, A, B) {
  if (ncol(A) != ncol(B)) stop()
  if (ncol(data) != ncol(A)) stop()
  if (nrow(A) != 1) stop()
  if (nrow(B) != 1) stop()
  
  m <- as.matrix(B - A)
  m2 <- m %*% t(m) %>% as.numeric()
  D <- as.matrix(data) - (matrix(1, nrow = nrow(data)) %*% as.matrix(A))
  -1 + 2 * (D %*% t(m)) / m2
}

# compute projection on i:-I (for happy phoneme)
REALandABM.df[label %in% c("i:", "I:", "I"),
              projection_i := projectOnSegment(.SD, i.mean.Icols, I.mean.Icols) %>% as.numeric,
              .SDcols = Icols]

# lmer stats
dist.i.response <- "euclDist_i"
# dist.i.response <- "proj_F20_i"
dist.i.response <- "P4"
lmer.formula <- list(
  u = paste(dist.i.response, "~ session + (1| word) + (session |speaker)"),
  ju = paste(dist.i.response, "~ session + (1| word) + (session |speaker)"),
  ou = paste(dist.i.response, "~ session + (1| word) + (session |speaker)"),
  `I:` = "projection_i ~ session + (1| word) + (session |speaker)"
)

phonemes <- c("u", "ju", "ou", "I:")

# lmer on REAL data
REAL.lmer <- sapply(phonemes, function(ph) {
  lmer(as.formula(lmer.formula[[ph]]), data = REALandABM.df[origin == "REAL" & label == ph])
}, USE.NAMES = TRUE)
# get fixed effects and their significance:
sapply(REAL.lmer, fixef)
sapply(REAL.lmer, Anova)

# random slopes for speaker
REAL.slopes <- rbindlist(lapply(phonemes, function(ph) {
  REAL.lmer[[ph]] %>% ranef %>% .[["speaker"]] %>%
    setDT(keep.rownames = TRUE) %>% .[, phoneme := ph]
})) %>% setnames("rn", "speaker") %>% .[, `(Intercept)` := NULL]

# ABM.Runs.lmer contains lmer on ABMs, one lmer per run per phoneme. 
REALandABM.df[origin == "ABM" & label %in% phonemes & Run %in% 1:10,
              {
                label_ <- label
                .(ABM.lmer = list(lmer(as.formula(lmer.formula[[label]]),
                                       data = rbindlist(list(.SD, REALandABM.df[origin == "REAL" &
                                                                                  label == label_ & 
                                                                                  session == 0]), use.names = TRUE, fill = TRUE))
                ))
              },
              by = .(label, Run)] -> ABM.Runs.lmer

# to extract a specific lmer, do as follows:
ABM.Runs.lmer[Run == 3 & label == "u", ABM.lmer][[1]]

# ABM.Runs.fixedEffects contains fixed effect values and their p.value
ABM.Runs.lmer[, .(
  session.fixed.eff = ABM.lmer[[1]] %>% summary %>% .$coefficients %>% .["session", "Estimate"],
  session.p.val = Anova(ABM.lmer[[1]], type = "III") %>% .["session",'Pr(>Chisq)']
),
by = .(label, Run)] -> ABM.Runs.fixedEffects

# ABM.Runs.slopes contains all random slopes for (session | speaker) in the 'session' column
ABM.Runs.lmer[, ABM.lmer[[1]] %>% ranef %>% .[["speaker"]] %>%
                setDT(keep.rownames = TRUE) %>% setnames("rn", "speaker") %>% .[, `(Intercept)` := NULL]
              ,
              by = .(label, Run)] -> ABM.Runs.slopes

# ABM.Runs.cor contains correlations between REAL and each Run ABM (session | speaker) coeff
ABM.Runs.slopes[REAL.slopes, on = c("speaker", label = "phoneme"), nomatch = 0][
  , .(session.slopes.cor = cor(session, i.session)), by = .(label, Run)] -> ABM.Runs.cor


# how many run-phoneme specific lmer report a negative fixed effect for session?
ABM.Runs.fixedEffects[session.fixed.eff < 0, .N, by = label]
# how many run-phoneme specific lmer report a significant fixed effect for session?
ABM.Runs.fixedEffects[session.p.val < 0.05, .N, by = label]
# what is the median of speaker random slopes cor(REAL, ABM), specific for phoneme?
ABM.Runs.cor[, median(session.slopes.cor), by = label]

# lmer on global ABM, all runs
ABM.lmer <- sapply(c("u", "ju", "ou"), function(ph) {
  lmer(as.formula(paste(lmer.formula[[ph]], "+ (session - 1 | Run)")),
       data = REALandABM.df[origin == "ABM" & label == ph])
}, USE.NAMES = TRUE)
# 'I:' does not converge using the default lmer optimizer (bobyqa)
# Using optimx.nlminb instead.
library(optimx)
ABM.lmer[["I:"]] <- lmer(as.formula(paste(lmer.formula[["I:"]], "+ (session - 1 | Run)")),
       data = REALandABM.df[origin == "ABM" & label == "I:"],
       control=lmerControl(optimizer = "optimx", optCtrl=list(method = "nlminb"))
       )
# Note: the values from the not converged model based on bobyqa 
# and those from the converged one based on nlminb are practically identical.

# get fixed effects and their significance:
sapply(ABM.lmer, fixef)
sapply(ABM.lmer, Anova)

# random slopes for speaker
ABM.slopes <- rbindlist(lapply(phonemes, function(ph) {
  ABM.lmer[[ph]] %>% ranef %>% .[["speaker"]] %>%
    setDT(keep.rownames = TRUE) %>% .[, phoneme := ph]
})) %>% setnames("rn", "speaker") %>% .[, `(Intercept)` := NULL]

# code speakers with letters
Vpn <- LETTERS[REALandABM.df$speaker %>% unique %>% as.character %>% as.numeric]
Vpn[Vpn %in% c("C", "I", "L")] <- c("O", "M", "X")
Vpn <- data.table(speaker = REALandABM.df$speaker %>% unique %>% sort %>% as.character,
                  Vpn = Vpn)

# all slopes in one table
slopes <- rbindlist(list(
  REAL.slopes[, origin := "REAL"],
  ABM.slopes[, origin := "ABM"]
))
slopes <- slopes[Vpn, on = "speaker", nomatch = 0]

# plot REAL vs ABM slopes
ggplot(slopes %>% spread(origin, session)) +
  aes(x = ABM, y = REAL) +
  geom_text(aes(label = Vpn)) +
  facet_grid(~ phoneme)

# add position prior to Antarctica as Position0
slopes <- slopes[
REALandABM.df[session == 0 & label %in% phonemes,
              {
                if (label == "I:") {
                  Pos <- mean(projection_i)
                } else {
                  Pos <- log(mean(euclDist_i))
                }
                .(Position0 = Pos)
                },
              by = .(speaker, label)] %>%
  .[, speaker := as.character(speaker)],
on = c("speaker", phoneme = "label"),
nomatch=0]

# correlation between REAL and ABM random slopes for speaker
slopes.lm <- lm()


# plot position 0 against slopes
ggplot(slopes) +
  aes(x = Position0, y = session) +
  geom_text(aes(label = Vpn)) +
  facet_grid(origin ~ phoneme)

########## formant trajectories in time

library(dtt)
# inverse DCT function
inv_dct_from_emuR <- function(X, N = 11) {
  0.5*(sqrt(2) - 1) * X[1] + dtt::dct(c(X, rep(0, N - length(X))), variant = 3)
}

# compute mean F2 trajectories
Fcols <- paste0("P", 4:6, sep = '')
meanDCT <- REALandABM.df[valid == TRUE,
               lapply(.SD, mean), by = .(session, label, origin), .SDcols = Fcols]
# compute inverse DCT
N_samples <- 11
REALandABM.traj <- meanDCT[, .(F2 = inv_dct_from_emuR(.SD %>% as.numeric, N = N_samples),
                        time = seq(0, 1, length.out = N_samples)),
                    by = .(session, label, origin), .SDcols = Fcols] 

# plot F2 in time
ggplot(REALandABM.traj[!(session == 0 & origin == "ABM") & label %in% phonemes]  %>%
         .[, origin := factor(origin, levels = c("REAL", "ABM"))]) +
  aes(x = time, y = F2, color = interaction(session, origin), group = interaction(session, origin)) +
  facet_grid( ~ label) +
  geom_line() +
  ggtitle("F2 trajectories") +
  ylab("F2")


