# testthat folder
ABMpath <- "/homes/m.gubian/ABM/ABM" # your ABM home dir here
testDir <- file.path(ABMpath, "tests", "testthat")
# load all the funcions and libraries (this may not be needed when the code becomes a package)
source(file.path(ABMpath, "Rcmd", "loadLibraries.R"))


context("Production")

# example agent (from Antarctica)
agent <- readRDS(file.path(testDir, "agent1.rds")) 

test_that("choose_word throws error if labels does not have both a 'word' and a 'valid' column", {
  expect_error(choose_word(data.table(a = 1)))
  expect_error(choose_word(data.table(valid = TRUE)))
  expect_error(choose_word(data.table(word = "a")))
})

test_that("choose_word throws error to an unknown method", {
  expect_error(choose_word(agent$labels, method = "_unknown_method_"), "Unknown method", ignore.case = TRUE)
})

test_that("choose_word returns NULL on empty memory (labels)", {
  empty_mem <- agent$labels[FALSE]
  expect_null(choose_word(empty_mem))
})

test_that("choose_word picks one word", {
  word <- choose_word(agent$labels)
  expect_length(word, 1)
  expect_true(word %in% agent$labels[valid == TRUE, word])
})


test_that("produce_token returns NULL on an empty memory" , {
  agent <- list(labels = data.table(word = "a", valid = FALSE))
  expect_null(produce_token(agent, NULL))
})


