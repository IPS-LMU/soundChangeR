context("Production")

agent <- readRDS("agent1d.rds")

test_that("choose_word throws error if memory does not have both a 'word' and a 'valid' column", {
  expect_error(choose_word(data.table(a = 1)))
  expect_error(choose_word(data.table(valid = TRUE)))
  expect_error(choose_word(data.table(word = "a")))
})

test_that("choose_word throws error to an unknown method", {
  expect_error(choose_word(agent$memory, method = "_unknown_method_"), "Unknown method", ignore.case = TRUE)
})

test_that("choose_word returns NULL on empty memory (labels)", {
  empty_mem <- agent$memory[FALSE]
  expect_null(choose_word(empty_mem))
})

test_that("choose_word picks one word", {
  word <- choose_word(agent$memory)
  expect_length(word, 1)
  expect_true(word %in% agent$memory[valid == TRUE, word])
})


test_that("produce_token returns NULL on an empty memory" , {
  agent <- list(memory = data.table(word = "a", valid = FALSE))
  expect_null(produce_token(agent, NULL))
})

test_that("produce_token returns a well-formed token", {
  # GMM version
  params <- list()
  params[["perceptionModels"]] <- "GMM"
  params[["featureExtractionMethod"]] <- "identity"
  # case 1-dim features
  agent <- readRDS("agent1d.rds")
  token <- produce_token(agent, params)
  expect_equal(nrow(token), 1)
  expect_true(token$word %in% agent$memory[valid == TRUE, word])
  expect_equal(token$label, agent$memory[word == token$word & valid == TRUE, label][1] )
  features <- one_exemplar2obj(token[1, exemplar])
  expect_length(features, ncol(agent$features))
  # case 3-dim features
  agent <- readRDS("agent3d.rds")
  token <- produce_token(agent, params)
  expect_equal(nrow(token), 1)
  expect_true(token$word %in% agent$memory[valid == TRUE, word])
  expect_equal(token$label, agent$memory[word == token$word & valid == TRUE, label][1] )
  features <- one_exemplar2obj(token[1, exemplar])
  expect_length(features, ncol(agent$features))
})
