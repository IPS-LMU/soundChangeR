# Analysis of REAL and ABM simulation data 

library(data.table)
library(dplyr)
library(car)
library(emmeans)
library(lme4)
library(proxy)
library(tidyr)
library(ggplot2)
library(dtt)
library(Tmisc)
library(optimx)

# If you ran the simulations, rootLogDir is the top directory where you ran the simulations.
# Otherwise, you can load the results from data/Antarctica_results.zip.
# In the latter case, skip to the line where REALandABM.df is read from file

rootLogDir <- "/vdata/Projects/ABM/simulations/Michele/Antarctica" # change manually
simulationName <- "ABM20181204185316" # change manually
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


# Alternatively, read REALandABM.df from file
# logDir is the directory where you extracted data/Antarctica_results.zip

# real data + all runs collected here
# ABM session 1 based on production after 5000 interactions
REALandABM.df <- fread(file.path(logDir, "REALandABM.production.df"))
REALandABM.df[, `:=`(speaker = factor(speaker),
                     session = session %>% as.character %>% as.numeric, # this is needed in the lmer  
                     origin = factor(origin, levels = c("REAL", "ABM")))]

# Note: session has to be coded as numeric (integer) for later use in lmer
# see: https://stats.stackexchange.com/questions/122336/coding-of-categorical-random-effects-in-r-int-vs-factor

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

# compute distance to i: (for /u/-phonemes)
REALandABM.df[label %in% c("i:", "ju", "u", "ou"),
              euclDist_i := proxy::dist(.SD, i.mean.Ucols),
              .SDcols = Ucols]

# function to compute projection on a segment 
# values in data near A, B will take values near -1, 1.
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
lmer.formula <- list(
  u = "log(euclDist_i) ~ session + (1|word) + (session|speaker)",
  ju = "log(euclDist_i) ~ session + (1|word) + (session|speaker)",
  ou = "log(euclDist_i) ~ session + (1|word) + (session|speaker)",
  `I:` = "projection_i ~ session + (1|word) + (session|speaker)"
)

phonemes <- c("u", "ju", "ou", "I:")

# lmer on REAL data

REAL.lmer <- sapply(phonemes, function(ph) {
  lmer(as.formula(lmer.formula[[ph]]), data = REALandABM.df[origin == "REAL" & label == ph])
}, USE.NAMES = TRUE)
# get fixed effects and their significance:
sapply(REAL.lmer, fixef)
# stats reported in caption of Fig. 1
sapply(REAL.lmer, Anova)

# random slopes for speaker
REAL.slopes <- rbindlist(lapply(phonemes, function(ph) {
  REAL.lmer[[ph]] %>% ranef %>% .[["speaker"]] %>%
    setDT(keep.rownames = TRUE) %>% .[, phoneme := ph]
})) %>% setnames("rn", "speaker") %>% .[, `(Intercept)` := NULL]


# lmer on global ABM, all runs

require(parallel)
numCores <- detectCores() - 1
cl <- makeCluster(numCores, type = "FORK")
clusterSetRNGStream(cl)
ABM.lmer <- parSapply(cl, c("u", "ju", "ou"), function(ph) {
  lmer(as.formula(paste(lmer.formula[[ph]], "+ (0 + session | Run)")),
       data = REALandABM.df[origin == "ABM" & label == ph])
}, USE.NAMES = TRUE)
stopCluster(cl)  


# 'I:' does not converge using the default lmer optimizer (bobyqa)
# Using optimx.nlminb instead it does.
library(optimx)
ABM.lmer[["I:"]] <- lmer(as.formula(paste(lmer.formula[["I:"]], "+ (0 + session | Run)")),
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

# add position prior to Antarctica as Position0
slopes <- slopes[
  REALandABM.df[session == 0 & origin == "REAL" & label %in% phonemes,
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


########################################## Fig. 1
# plot F2 in time
cols = c("black", "slategray", "black")
lty = c(3, 1, 1)

p = with(REALandABM.traj, as.character(interaction(session, origin)))
p[p == "0.REAL"] = "Baseline"
p[p == "1.REAL"] = "Antarctica"
p[p == "1.ABM"] = "ABM"
z = cbind(REALandABM.traj, p = factor(p))
labs3 = z$label
labs3[labs3 == "I:"] = "ɪ:"
z = cbind(z, labs3)


ggplot(z[!(session == 0 & origin == "ABM") & label %in% phonemes]  %>%
         .[, origin := factor(origin, levels = c("REAL", "ABM"))]) +
  aes(x = time, y = F2, linetype = p, color = p, group = p) +
  facet_grid( ~ labs3) +
  geom_line(size=.9) +  scale_colour_manual(values = cols) +  scale_linetype_manual(values = lty) + 
  ylab("Speaker-normalised F2")  + xlab("Normalised time") + 
  theme(text = element_text(size=14), legend.title=element_blank())


########################################## Fig. 2

slopes[phoneme == "I:", phoneme := "ɪ:"]
slopes[origin == "REAL", origin := "In Antarctica"]

ggplot(slopes[origin == "In Antarctica"]) +
  aes(x = Position0, y = session) +
  stat_smooth(method="lm", se=FALSE,  color = 'grey') +
  geom_text(aes(label = Vpn)) +
  facet_grid( ~ phoneme) + xlab("Position in baseline") + ylab("Size of change relative to baseline")

slopes[origin == "In Antarctica",
       {print(phoneme); lm(session ~ Position0) %>% summary %>% print},
       by = phoneme]

########################################## Fig. 3

ggplot(slopes %>% spread(origin, session)) +
  aes(x = ABM, y = `In Antarctica`) +
  stat_smooth(method="lm", se=FALSE,  color = 'grey') +
  geom_text(aes(label = Vpn)) +
  facet_grid(~ phoneme) + ylab("Size of change in Antarctica") + xlab("Size of change in ABM") 
  


########################################## Stats reported in Figures
dunnsidak = function(alpha = .05, N = 4)
{
  1 - (1 - alpha)^(1/N)
}

dunnsidak()
#  0.01274146

# Fig. 1
# this shows that from 0 to 1 /ou/is significant
sapply(REAL.lmer, Anova)
# Chisq      0.7883375 0.796133  10.58769    0.5853313
# Df         1         1         1           1        
# Pr(>Chisq) 0.3746033 0.3722521 0.001138432 0.4442302

# Fig. 2
# Correlation tests between changes in Antarctica and position before Antarctica
slopes[origin == "In Antarctica"] %>% .[, cor.test(Position0, session), by = phoneme] %>%
  .[c(1, 3, 5, 7),1:5]
# phoneme statistic parameter     p.value   estimate
# 1:      ɪ: -4.751163         9 0.001042492 -0.8455479
# 2:      ju -4.658539         9 0.001187678 -0.8407493
# 3:       u -2.547947         9 0.031300019 -0.6473455
# 4:      ou -1.349585         9 0.210114306 -0.4102599

# Fig 3 
# Correlation tests between changes in ABM and 'In Antarctica'
slopes %>% spread(origin, session) %>% .[, cor.test(ABM, `In Antarctica`), by = phoneme] %>%
  .[c(1, 3, 5, 7),1:5]
# phoneme statistic parameter      p.value  estimate
# 1:      ɪ:  4.662467         9 0.0011810986 0.8409569
# 2:      ju  4.889689         9 0.0008597959 0.8523609
# 3:      ou  1.286290         9 0.2304445365 0.3940683
# 4:       u  2.473898         9 0.0353435162 0.6362143






########### Extra analysis (not used in the manuscript) ###########

# lmer on ABMs, one lmer per run,
# collect results in a table called REALandABM.stats,
# which has 3 results column, each row is specific to a single run and a single phoneme:
# session.fixed.eff : the estimated fixed effect for session in the lmer on ABM
# session.p.val : the significance of session.fixed.eff
# session.slopes.cor : cor() between REAL and ABM session slopes 

# (it may take 1-2 minutes)
REALandABM.df[origin == "ABM" & label %in% phonemes & Run != 0 ,
              {
                label_ <- label
                ABM.lmer <- lmer(as.formula(lmer.formula[[label]]),
                                 data = rbindlist(list(.SD, REALandABM.df[origin == "REAL" &
                                                                            label == label_ & 
                                                                            session == 0]), use.names = TRUE, fill = TRUE))
                ABM.slopes <- ABM.lmer %>% ranef %>% .[["speaker"]] %>%
                  setDT(keep.rownames = TRUE) %>% setnames("rn", "speaker") %>% .[, `(Intercept)` := NULL]
                slopes <- ABM.slopes[REAL.slopes[phoneme == label], on = "speaker", nomatch = 0]
                .(session.slopes.cor = cor(slopes$session, slopes$i.session),
                  session.fixed.eff = ABM.lmer %>% summary %>% .$coefficients %>% .["session1", "Estimate"],
                  session.p.val = Anova(ABM.lmer, type = "III") %>% .["session",'Pr(>Chisq)'])
              },
              by = .(label, Run)] -> REALandABM.stats 

# how many run-phoneme specific lmer report a negative fixed effect for session?
REALandABM.stats[session.fixed.eff < 0, .N, by = label]
# how many run-phoneme specific lmer report a significant fixed effect for session?
REALandABM.stats[session.p.val < 0.05, .N, by = label]
# what is the median of speaker random slopes cor(REAL, ABM), specific for phoneme?
REALandABM.stats[, median(session.slopes.cor), by = label]
