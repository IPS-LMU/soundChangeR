library(devtools)
library(data.table)
library(tidyverse)
library(gridExtra)
library(cowplot)

load_all()

##### production #####

p1 <- u_fronting %>% 
  filter(speaker == "albr" & word == "food") %>% 
  ggplot() + 
  aes(x = DCT0, y = DCT1, label = word) + 
  geom_text() + 
  stat_ellipse() + 
  geom_text(data = data.frame(DCT0 = 1268.7, DCT1 = -6.6, word = "food"), 
            color = "darkorange2", fontface = "bold") +
  xlim(1200, 1300) + ylim(-20, 12) +
  theme_light()
p2 <- u_fronting %>% 
  filter(speaker == "albr" & word == "food") %>% 
  ggplot() + 
  aes(x = DCT0, y = DCT2, label = word) + 
  geom_text() + 
  stat_ellipse() + 
  geom_text(data = data.frame(DCT0 = 1268.7, DCT2 = -7.1, word = "food"), 
            color = "darkorange2", fontface = "bold") +
  xlim(1200, 1300) + ylim(-12, 22) +
  theme_light()
pl <- grid.arrange(p1, p2, nrow = 1)
ggsave(plot = pl, file = "vignettes/prod.png", width = 20, height = 10, units = "cm")

##### perception #####

p1 <- u_fronting %>% 
  filter(speaker == "elwi") %>% 
  mutate(phoneme = case_when(word %in% c("seep", "heed", "keyed", "feed") ~ 1,
                             word %in% c("soup", "hewed", "queued", "feud") ~ 2,
                             TRUE ~ 3),
         phoneme = as.factor(phoneme)) %>%
  ggplot() + 
  aes(x = DCT0, y = DCT1, col = phoneme, label = word) + 
  geom_text() + 
  stat_ellipse() + 
  geom_text(mapping = aes(x = DCT0, y = DCT1, label = word), 
            data = data.frame(DCT0 = 1268.7, DCT1 = -6.6, word = "food"), 
            color = "darkorange2", fontface = "bold") + 
  xlim(800, 1550) + ylim(-60, 140) +
  theme_light() + theme(legend.position = "none")
p2 <- u_fronting %>% 
  filter(speaker == "elwi") %>% 
  mutate(phoneme = case_when(word %in% c("seep", "heed", "keyed", "feed") ~ 1,
                             word %in% c("soup", "hewed", "queued", "feud") ~ 2,
                             TRUE ~ 3),
         phoneme = as.factor(phoneme)) %>%
  ggplot() + 
  aes(x = DCT0, y = DCT2, col = phoneme, label = word) + 
  geom_text() + 
  stat_ellipse() + 
  geom_text(mapping = aes(x = DCT0, y = DCT2, label = word), 
            data = data.frame(DCT0 = 1268.7, DCT2 = -7.1, word = "food"), 
            color = "darkorange2", fontface = "bold") + 
  xlim(800, 1550) + ylim(-25, 100) +
  theme_light() + theme(legend.position = "none")
pl <- grid.arrange(p1, p2, nrow = 1)
ggsave(plot = pl, file = "vignettes/perc.png", width = 20, height = 10, units = "cm")

##### phonology #####

params <- get_params("/vdata/ABM/simulations/Johanna/test_soundChangeR/logDir", "ABM20211111100942")
pop <- load_pop("/vdata/ABM/simulations/Johanna/test_soundChangeR/logDir/ABM20211111100942",
                runs = 2,
                snaps = 100)

p <- pop %>% filter(speaker == "phfo")
phfo <- list(
  agentID = 19,
  group = "older",
  speaker = "phfo",
  cache = data.table(name = "GMM", value = list(), valid = FALSE),
  memory = data.table(word = p$word,
                      phoneme = p$phoneme,
                      valid = TRUE,
                      nrOfTimesHeard = 1,
                      producerID = 19,
                      exemplar = p$exemplar),
  features = data.table(P1 = p$P1,
                        P2 = p$P2,
                        P3 = p$P3)
)

rawGMM <- estimate_raw_clusters(phfo, params)
fullWordClusters <- get_full_word_clusters(rawGMM, phfo, params)
fullWordClusters
#         cluster
# word     1 2 3  4
#   cooed  2 0 7  0
#   feed   0 9 0  0
#   feud   4 0 0  5
#   food   0 0 9  0
#   heed   0 9 0  0
#   hewed  1 0 0  8
#   keyed  0 9 0  0
#   queued 6 0 0  3
#   seep   0 9 0  0
#   soup   0 0 0 13
#   who'd  1 0 8  0

reducedWordClustersIncidenceMatrix <- estimate_reduced_clusters_incidence_matrix(fullWordClusters, params[["purityRepetitions"]], params[["purityThreshold"]])
reducedWordClusters <- get_reduced_clusters(fullWordClusters, reducedWordClustersIncidenceMatrix)
reducedWordClusters
# word     [,1] [,2] [,3]
#   cooed     2    7    0
#   feed      0    0    9
#   feud      9    0    0
#   food      0    9    0
#   heed      0    0    9
#   hewed     9    0    0
#   keyed     0    0    9
#   queued    9    0    0
#   seep      0    0    9
#   soup     13    0    0
#   who'd     1    8    0

excludedClass <- reducedWordClusters %>% base::apply(2, base::sum)
excludedClassIdx <- (excludedClass < rawGMM$d) %>% base::which()

# saveRDS(phfo, "vignettes/phfo.rds")
# saveRDS(fullWordClusters, "vignettes/fullWordClusters.rds")
# saveRDS(reducedWordClusters, "vignettes/reducedWordClusters.rds")
# saveRDS(rawGMM, "vignettes/rawGMM.rds")

GMM <- reestimate_GMM(rawGMM, reducedWordClustersIncidenceMatrix, phfo, params)
set_cache_value(phfo, "GMM", GMM)

wordLabels <- assign_words_to_phonemes(reducedWordClusters)
wordLabels %>% arrange(phoneme)
#       word phoneme
#  1:   feud       1
#  2:  hewed       1
#  3: queued       1
#  4:   soup       1
#  5:  cooed       2
#  6:   food       2
#  7:  who'd       2
#  8:   feed       3
#  9:   heed       3
# 10:  keyed       3
# 11:   seep       3

phfo$memory[wordLabels, on = "word", phoneme := i.phoneme]
# saveRDS(phfo, "vignettes/phfo_update.rds")

d <- data.table(agentID = phfo$agentID,
                group = phfo$group,
                speaker = phfo$speaker,
                word = phfo$memory$word[1:103],
                phoneme = phfo$memory$phoneme[1:103],
                cluster = paste0("a", rawGMM$classification),
                DCT0 = phfo$features$P1[1:103],
                DCT1 = phfo$features$P2[1:103],
                DCT2 = phfo$features$P3[1:103])
# saveRDS(d, "vignettes/GMM_NMF.rds")

d %>% filter(cluster %in% c("a1", "a4")) %>% group_by(word, phoneme, cluster) %>% summarise(n = n()) %>% arrange(word, cluster, phoneme)

p1 <- ggplot(d) + aes(x = DCT0, y = DCT1, group = cluster) + 
        geom_point() + stat_ellipse() +
        annotate("text", x = 900, y = 50, label = "a1", fontface = "italic") +
        annotate("text", x = 1350, y = -25, label = "a2", fontface = "italic") +
        annotate("text", x = 870, y = -35, label = "a3", fontface = "italic") +
        annotate("text", x = 1100, y = 35, label = "a4", fontface = "italic") +
        xlim(800, 1400) + ylim(-60, 125) +
        theme_light() + theme(legend.position = "none")
p2 <- ggplot(d) + aes(x = DCT0, y = DCT2, group = cluster) + 
        geom_point() + stat_ellipse() +
        annotate("text", x = 920, y = 60, label = "a1", fontface = "italic") +
        annotate("text", x = 1350, y = -20, label = "a2", fontface = "italic") +
        annotate("text", x = 820, y = 25, label = "a3", fontface = "italic") +
        annotate("text", x = 1110, y = -10, label = "a4", fontface = "italic") +
        xlim(800, 1400) + ylim(-25, 90) +
        theme_light() + theme(legend.position = "none")
pl <- grid.arrange(p1, p2, nrow = 1)
ggsave(plot = pl, file = "vignettes/cluster.png", width = 20, height = 10, units = "cm")

p1 <- ggplot(d) + 
        aes(x = DCT0, y = DCT1, label = word, col = phoneme, group = cluster) + 
        geom_text() + 
        stat_ellipse(data = d %>% mutate(phoneme = ifelse(cluster == "a1", "1", phoneme)),
                     mapping = aes(x = DCT0, y = DCT1, col = phoneme)) +
        xlim(800, 1400) + ylim(-60, 125) +
        theme_light() + theme(legend.position = "none")
p2 <- ggplot(d) + 
        aes(x = DCT0, y = DCT2, label = word, col = phoneme, group = cluster) + 
        geom_text() + 
        stat_ellipse(data = d %>% mutate(phoneme = ifelse(cluster == "a1", "1", phoneme)),
                     mapping = aes(x = DCT0, y = DCT2, col = phoneme)) +
        xlim(800, 1400) + ylim(-25, 90) +
        theme_light() + theme(legend.position = "none")
pl <- grid.arrange(p1, p2, nrow = 1)
ggsave(plot = pl, file = "vignettes/sub-phon.png", width = 20, height = 10, units = "cm")

##### analysis #####

rootLogDir <- "/vdata/ABM/simulations/Johanna/test_soundChangeR/logDir"
simulationName <- "ABM20211111100942"
logDir <- file.path(rootLogDir, simulationName)
params <- get_params(rootLogDir, simulationName)
r <- 4:8
s <- 250

pop <- load_pop(logDir, runs = r, snaps = seq(0, s, by = 10)) %>% 
  mutate(canonical = case_when(word %in% c("seep", "heed", "keyed", "feed") ~ "i:",
                               word %in% c("soup", "who'd", "cooed", "food") ~ "u:",
                               word %in% c("hewed", "queued", "feud") ~ "ju:"))
Pcols <- get_Pcols(pop)
intLog <- load_intLog(logDir, runs = r, snaps = seq(1, s, by = 10)) %>% 
  left_join(
    pop %>% 
      select(speaker, group, word, canonical, agentID) %>% 
      unique(),
    by = c("perceiverID"="agentID", "word")
  )

pl <- plot_centroids(pop, Pcols, groupVar = c("snapshot", "run"), params)
ggsave(plot = pl, file = "vignettes/centroids1.png")
pl <- plot_centroids(pop, Pcols, groupVar = c("snapshot", "run", "group"), params)
ggsave(plot = pl, file = "vignettes/centroids2.png", width = 2500, height = 2100, units = "px")
pl <- plot_centroids(pop, Pcols, groupVar = c("snapshot", "run", "group", "canonical"), params)
ggsave(plot = pl, file = "vignettes/centroids3.png", width = 2700, height = 2100, units = "px")

pl <- plot_rejection(intLog, groupVar = c("snapshot", "run"), params)
ggsave(plot = pl, file = "vignettes/rejection1.png")
pl <- plot_rejection(intLog, groupVar = c("snapshot", "run", "group"), params)
ggsave(plot = pl, file = "vignettes/rejection2.png", width = 2500, height = 2100, units = "px")
pl <- plot_rejection(intLog, groupVar = c("snapshot", "run", "group", "canonical"), params)
ggsave(plot = pl, file = "vignettes/rejection3.png", width = 2700, height = 2100, units = "px")

pl <- plot_phonology(pop, canonical = "canonical", params)
ggsave(plot = pl, file = "vignettes/phonology1.png")



# pop <- load_pop(logDir, runs = r, snaps = seq(0, s, by = 10)) %>% 
#   mutate(
#     state = case_when(snapshot == 0 ~ "baseline",
#                       snapshot == s ~ "post-run",
#                       TRUE ~ "during"),
#     snapshot = as.numeric(snapshot)
#   ) %>% 
#   # rename(DCT0 = P1, DCT1 = P2, DCT2 = P3) %>% 
#   mutate(canonical = case_when(word %in% c("seep", "heed", "keyed", "feed") ~ "i:",
#                                word %in% c("soup", "who'd", "cooed", "food") ~ "u:",
#                                word %in% c("hewed", "queued", "feud") ~ "ju:"))
#   # left_join(
#   #   input.df %>%
#   #     select(word, phoneme) %>% 
#   #     unique() %>%
#   #     rename(canonical = phoneme),
#   #   by = "word"
#   # )
# 
# intLog <- load_intLog(logDir, runs = r, snaps = seq(1, s, by = 10)) %>%
#   mutate(snapshot = as.numeric(snapshot)) %>% 
#   left_join(
#     pop %>% 
#       select(speaker, group, word, canonical, agentID) %>% 
#       unique(),
#     by = c("perceiverID"="agentID", "word")
#   )
# 
# # cache <- load_cache(logDir, runs = r, snaps = c(0, s)) %>%
# #   filter(name == "GMM") %>%
# #   mutate(
# #     state = case_when(snapshot == 0 ~ "baseline",
# #                       snapshot == s ~ "post-run",
# #                       TRUE ~ "during"),
# #     snapshot = as.numeric(snapshot)
# #   ) %>% 
# #   left_join(pop %>% 
# #               select(speaker, agentID) %>% 
# #               unique(),
# #             by = "agentID")
# 
# 
# # centroids
# pl <- pop %>% 
#   group_by(canonical, snapshot, run, group) %>%
#   summarise_at(all_of(c("DCT0", "DCT1", "DCT2")), mean) %>% 
#   pivot_longer(cols = all_of(c("DCT0", "DCT1", "DCT2"))) %>%
#   ggplot() +
#   aes(x = snapshot*params$interactionsPerSnapshot, y = value, col = canonical, 
#       group = interaction(group, canonical, run)) +
#   geom_line() + 
#   facet_grid(name~group, scales = "free_y") + 
#   xlab("Interactions") + ylab("") + 
#   scale_x_continuous(breaks = c(0, 125000, 250000)) +
#   theme_light() + 
#   theme(strip.text.x = element_text(color = "black"), 
#         strip.text.y = element_text(color = "black"))
# ggsave(plot = pl, file = "vignettes/centroids.png", width = 18, height = 15, units = "cm")
# 
# # ellipses (fixed phonemes)
# pl1 <- pop %>% 
#   filter(state != "during" & run == 4) %>% 
#   ggplot() +
#   aes(x = DCT0, y = DCT1, col = canonical) +
#   stat_ellipse() +
#   geom_point(size = 0.3) +
#   facet_grid(state~group) +
#   theme_light() + 
#   theme(strip.text.x = element_text(color = "black"), 
#         strip.text.y = element_text(color = "black"),
#         legend.position = "none")
# pl2 <- pop %>% 
#   filter(state != "during" & run == 4) %>% 
#   ggplot() +
#   aes(x = DCT0, y = DCT2, col = canonical) +
#   stat_ellipse() +
#   geom_point(size = 0.3) +
#   facet_grid(state~group) +
#   theme_light() + 
#   theme(strip.text.x = element_text(color = "black"), 
#         strip.text.y = element_text(color = "black"))
# pl <- plot_grid(pl1, pl2, align = "h", rel_widths = c(0.85, 1))
# ggsave(plot = pl, file = "vignettes/ellipses.png", width = 20, height = 10, units = "cm")
# 
# # rejection
# pl <- intLog %>% 
#   group_by(snapshot, run, canonical, group) %>% 
#   summarise(rejection = 1-(sum(accepted)/n())) %>% 
#   ungroup() %>%
#   ggplot() + 
#   aes(x = snapshot*params$interactionsPerSnapshot, y = rejection, col = canonical, 
#       group = interaction(canonical, run)) + 
#   geom_line() + facet_grid(~group) + 
#   xlab("Interactions") + ylab("Rejection Rate") +
#   scale_x_continuous(breaks = c(0, 125000, 250000)) +
#   ylim(0, 1) + theme_light() +
#   theme(strip.text.x = element_text(color = "black"), 
#         strip.text.y = element_text(color = "black"))
# ggsave(plot = pl, file = "vignettes/rejection.png", width = 20, height = 10, units = "cm")
# 
# 
# 
# pl <- plot_phonology(pop, canonical = "canonical", params)
# ggsave(plot = pl, file = "vignettes/test.png")
# 
# 
# 
# 
# # purity
# library(NMF)
# nPhon <- pop %>% 
#   group_by(speaker, group, run, snapshot) %>% 
#   summarise(`Number of Sub-Phonemes` = n_distinct(phoneme), .groups = "drop")
# agree <- pop %>% 
#   group_by(speaker, group, run, snapshot) %>%
#   group_modify(~ {
#     .x %>% 
#       select(word, phoneme, canonical) %>% 
#       unique() %>% 
#       select(phoneme, canonical) %>% 
#       table() %>% 
#       purity() %>% 
#       as.data.frame()
#     }) %>% 
#   rename(Agreement = ".")
# 
# # PhonQuality <- pop[run %in% r, {
# #   D <- .SD[, .(word, phoneme, canonical)] %>% unique %>% .[, .(phoneme, canonical)]
# #   .(Agreement = D %>% table %>% purity)
# # },
# # by = .(speaker, group, run, snapshot)]
# 
# pl <- inner_join(nPhon, agree, by = c("speaker", "group", "snapshot", "run")) %>%
#   group_by(group, snapshot, run) %>% 
#   summarise(Agreement = mean(Agreement),
#             `Number of Sub-Phonemes` = mean(`Number of Sub-Phonemes`)) %>%
#   ungroup() %>%
#   pivot_longer(cols = c(`Number of Sub-Phonemes`, Agreement)) %>% 
#   ggplot() +
#   aes(x = snapshot*params$interactionsPerSnapshot, y = value, group = run) +
#   geom_line() + ylab("") + xlab("Interactions") +
#   facet_grid(name~group, scales = "free_y", switch = "y") +
#   scale_x_continuous(breaks = c(0, 125000, 250000)) +
#   theme_light() +
#   theme(strip.background.y = element_rect(fill = "white"), 
#         strip.placement = "outside",
#         strip.text.x = element_text(color = "black"), 
#         strip.text.y = element_text(color = "black"))
# ggsave(plot = pl, file = "vignettes/purity.png", width = 18, height = 15, units = "cm")

######################################################################################
params <- list(inputDataFile = "data-raw/u_fronting.csv", 
               speaker = "speaker", 
               group = "age", 
               word = "word", 
               phoneme = "label", 
               features = c("DCT0", "DCT1", "DCT2"), 
               subsetSpeakers = NULL,
               subsetPhonemes = NULL,
               createBootstrappedPopulation = FALSE,
               bootstrapPopulationSize = 50,
               expandMemory = FALSE,
               expandMemoryFactor = 2.0,
               removeOriginalExemplars = FALSE,
               useSMOTE = TRUE,
               fallBackOnPhoneme = TRUE,
               minTokens = 10,
               SMOTENN = 5,
               memoryIntakeStrategy = c("mahalanobisDistance", "maxPosteriorProb"),
               mahalanobisProbThreshold = .95,
               posteriorProbThreshold = 1/3,
               perceptionOOVNN = 5,
               forgettingRate = 1, 
               useFlexiblePhonology = F, 
               computeGMMsInterval = 100,
               purityRepetitions = 5,
               purityThreshold = 0.75,
               interactionPartners = "withinGroups",
               speakerProb = NULL,
               listenerProb = NULL,
               runSingleSimulation = TRUE,
               multipleABMRuns = 3, 
               nrOfSnapshots = 5, 
               interactionsPerSnapshot = 500, 
               rootLogDir = "./logDir",
               notes = "",
               logDir = "./logDir",
               featureExtractionMethod = "identity",
               nrOfInteractions = 2500)
i <- load_input_data(params)


#  1:    albr younger
#  2:    alha younger
#  3:    anhe younger
#  4:    brwa younger
#  5:    cajo younger
#  6:    jach younger 
#  7:    jeny younger
#  8:    kapo younger 
#  9:    rohi younger
# 10:    rusy younger
# 11:    shle younger ? boring
# 12:    dami   older ? soup grouped with i
# 13:    jiwo   older 
# 14:    nata   older ? boring & clean
# 15:    phfo   older ? boring & clean --> currently chosen
# 16:    dapr   older
# 17:    elwi   older 
# 18:    frwa   older
# 19:    joda   older 
# 20:    mapr   older ? too many acoustic clusters
# 21:    mibo   older ?
# 22:    pema   older ?
