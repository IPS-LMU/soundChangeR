context("Population data structures manipulation")

input1.dt <- fread("input1.csv") %>%
  # from data.table::fread man page:
  # "NB: sep2 is not yet implemented."
  .[, exemplar := strsplit(exemplar, "|", fixed = TRUE) %>% lapply(as.numeric)]


params <- list(
  featureExtractionMethod = "identity",
  initialMemoryResampling = FALSE,
  proportionGroupTokens = 0.0
)

agent <- create_agent(2, input1.dt, "SP2", 20, params)
# expected results
agent.initial <- data.table(word = letters[1:4], initial = rep(LETTERS[1:2], each = 2))
agent.memory.counts <- data.table(word = c(letters[1:4], NA), label = c(rep(LETTERS[1:2], each = 2), NA), N = 4)

test_that("create_agent produces expected metadata", {
  expect_equal(agent$agentID, 2)
  expect_equal(agent$group, "all")
  expect_equal(agent$speaker, "SP2")
  expect_equal(agent$initial[order(word)], agent.initial)
})

test_that("create_agent produces memory with expected item counts", {
  expect_equal(agent$memory %>% nrow, 20)
  expect_equal(agent$memory[valid == TRUE, .N], 16)
  expect_equal(agent$memory[order(word), .N, by= .(word,label)], agent.memory.counts)
})

test_that("create_agent produces exemplar column features table with expected content (identity case)", {
  expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(length) %>% unlist %>% unique, 3)
  expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(function(x) {x[1]}) %>% unlist %>% sort,
               0.1 + 17:32)
  expect_equal(colnames(agent$features), paste0("P", 1:3))
  expect_equal(nrow(agent$features), nrow(agent$memory))
  expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(function(x) {x[2]}) %>% unlist,
               agent$features[agent$memory$valid, P2])
})

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
    bootstrapPopulationSize = c(gr1 = 3, gr2 = 20)
  )
  pop <- with_mock(
    create_agent = mock_create_agent,
    create_population(input2.dt, params)
  )
  expect_length(pop, 23)
  expect_length(grep("^gr1", unlist(pop)), 3)
  expect_length(grep("^gr2", unlist(pop)), 20)
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
})

