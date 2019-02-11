context("Production")

# load all the funcions and libraries (this may not be needed when the code becomes a package)
# ABMpath <- "/homes/m.gubian/ABM/ABM"
# source(file.path(ABMpath, "Rcmd", "loadLibraries.R"))
# example agent (from Antarctica)
agent <- readRDS("agent1.rds") # /homes/m.gubian/ABM/ABM/tests/testthat/

test_that("choose_word fails if labels does not have a word column", {
  expect_error(choose_word(data.table(a = 1)))
})

test_that("choose_word throws error to an unknown method", {
  expect_error(choose_word(agent$labels, method = "_unknown_method_"), "Unknown method", ignore.case = TRUE)
})

test_that("choose_word throws error on empty memory", {
  empty_mem <- agent$labels[FALSE]
  expect_error(choose_word(empty_mem), "empty", ignore.case = TRUE)
})

test_that("choose_word picks one word", {
  word <- choose_word(agent$labels)
  expect_length(word, 1)
  expect_true(word %in% agent$labels[valid == TRUE, word])
})
