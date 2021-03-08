context("Population data structures manipulation")

input1.dt <- fread("input1.csv") %>%
  # from data.table::fread man page:
  # "NB: sep2 is not yet implemented."
  .[, exemplar := strsplit(exemplar, "|", fixed = TRUE) %>% lapply(as.numeric)]


params <- list(
  featureExtractionMethod = "identity",
  initialMemoryResampling = FALSE,
  proportionGroupTokens = 0.0,
  logDir = "logDir"
)

agent.initial <- data.table(word = letters[1:4], initial = rep(LETTERS[1:2], each = 2))
agent.memory.counts <- data.table(word = c(letters[1:4], NA), label = c(rep(LETTERS[1:2], each = 2), NA), N = 4)

for (perceptionModels in c("singleGaussian", "GMMs")) {
  params$perceptionModels <- perceptionModels
  if (params$perceptionModels == "GMMs") {
    params$purityRepetitions <- 5
    params$purityThreshold <- .95
    fm <- matrix(rnorm(3*48), ncol = 3)
    input1.dt[, exemplar := matrix2exemplar(fm)]
    
  }
  agent <- create_agent(2, input1.dt, "SP2", 20, params)
  
  test_that("create_agent produces expected metadata", {
    expect_equal(agent$agentID, 2)
    expect_equal(agent$group, "all")
    expect_equal(agent$speaker, "SP2")
    expect_equal(agent$initial[order(word)], agent.initial)
  })
  
  test_that("create_agent produces memory with expected item counts", {
    expect_equal(agent$memory %>% nrow, 20)
    expect_equal(agent$memory[valid == TRUE, .N], 16)
    if (params$perceptionModels == "singleGaussian") {
      expect_equal(agent$memory[order(word), .N, by= .(word,label)], agent.memory.counts)
    } else if (params$perceptionModels == "GMMs") {
      expect_equal(agent$memory[order(word), .N, by= .(word,label)][, .(word, N)], agent.memory.counts[, .(word, N)])
    }
  })
  
  test_that("create_agent produces exemplar column features table with expected content (identity case)", {
    expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(length) %>% unlist %>% unique, 3)
    if (params$perceptionModels == "singleGaussian") {
      expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(function(x) {x[1]}) %>% unlist %>% sort,
                   0.1 + 17:32)
    }
    expect_equal(colnames(agent$features), paste0("P", 1:3))
    expect_equal(nrow(agent$features), nrow(agent$memory))
    expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(function(x) {x[2]}) %>% unlist,
                 agent$features[agent$memory$valid, P2])
  })
  
  test_that("apply_resampling produces the right number of tokens", {
    params$productionBasis <- "word"
    params$productionResampling <- "SMOTE"
    params$productionResamplingFallback <- "label"
    params$productionMinTokens <- 10
    params$productionSMOTENN <- 5 
    for (removeOriginalExemplarsAfterResampling in c(FALSE, TRUE)) {
      agent <- create_agent(2, input1.dt, "SP2", 100, params)
      initialN <- agent$memory[valid == TRUE, .N]
      finalN <- 3 * initialN
      params$removeOriginalExemplarsAfterResampling <- removeOriginalExemplarsAfterResampling
      apply_resampling(agent, finalN, params)
      if (removeOriginalExemplarsAfterResampling) {
        expect_equal(agent$memory[valid == TRUE, .N], finalN - initialN)
      } else {
        expect_equal(agent$memory[valid == TRUE, .N], finalN)
      }
    }
  })
}

input2.dt <- rbindlist(
  sapply(
    paste0('gr', 1:4), function(x) {
      input1.dt[, .SD, .SDcols = -"group"][, speaker := paste(x, speaker, sep = '.')]
    }, simplify = FALSE, USE.NAMES = TRUE)
  , idcol = "group")

mock_create_agent <- function (id, input.df, selectedSpeaker, maxMemorySize, params) {selectedSpeaker}

test_that("create_population handles speaker_is_agent method correctly", {
  params <- list(
    initialMemoryResampling = FALSE,
    nrOfInteractions = 1000,
    rememberOwnTokens = FALSE
  )
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_equal(pop %>% unlist %>% sort, input2.dt$speaker %>% unique %>% sort)
})

test_that("create_population handles bootstrap method correctly when total pop size provided", {
  params <- list(
    initialMemoryResampling = FALSE,
    nrOfInteractions = 1000,
    rememberOwnTokens = FALSE,
    createPopulationMethod = "bootstrap",
    bootstrapPopulationSize = 20
  )
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_length(pop, 20)
  expect_true(all(unlist(pop) %in% unique(input2.dt$speaker)))
})

test_that("create_population handles bootstrap method correctly when by-group pop sizes provided", {
  params <- list(
    initialMemoryResampling = FALSE,
    nrOfInteractions = 1000,
    rememberOwnTokens = FALSE,
    createPopulationMethod = "bootstrap",
    bootstrapPopulationSize = c(gr3 = 1, gr2 = 2)
  )
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_length(pop, 3)
  expect_length(grep("^gr3", unlist(pop)), 1)
  expect_length(grep("^gr2", unlist(pop)), 2)
  expect_true(all(unlist(pop) %in% unique(input2.dt$speaker)))
  
  params$bootstrapPopulationSize = c(gr1 = 3, gr2 = 20, gr3 = 12, gr4 = 1)
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_length(pop, 3+20+12+1)
  expect_length(grep("^gr1", unlist(pop)), 3)
  expect_length(grep("^gr2", unlist(pop)), 20)
  expect_length(grep("^gr3", unlist(pop)), 12)
  expect_length(grep("^gr4", unlist(pop)), 1)
  expect_true(all(unlist(pop) %in% unique(input2.dt$speaker)))
  
  params$bootstrapPopulationSize = c(gr4 = 1)
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_length(pop, 1)
  expect_length(grep("^gr4", unlist(pop)), 1)
  expect_true(all(unlist(pop) %in% unique(input2.dt$speaker)))
  
  params$bootstrapPopulationSize = c(gr1 = 10)
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_length(pop, 10)
  expect_length(grep("^gr1", unlist(pop)), 10)
  expect_true(all(unlist(pop) %in% unique(input2.dt$speaker)))
  
  
})

