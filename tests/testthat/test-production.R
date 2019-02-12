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

# test data for knearest_Fallback
data_cross <- rbind(c(0,0))
cross_points <- rbind( c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
for (i in 1:3) {
  data_cross <- rbind(data_cross, i * cross_points)
}

test_that("knearest_Fallback throws error when cloud is empty", {
  expect_error(knearest_Fallback(data_cross[FALSE,], 0, 0), "Empty input cloud")
  expect_error(knearest_Fallback(NULL, 0, 0))
})

test_that("knearest_Fallback throws error for targetIndices out of bound", {
  N <- nrow(data_cross)
  expect_error(knearest_Fallback(data_cross, c(1, 2, N + 1), 2), "targetIndices out of bound")
  expect_error(knearest_Fallback(data_cross, c(1, 2, N + 1), 3), "targetIndices out of bound")
  expect_error(knearest_Fallback(data_cross, c(1, 2, 0), 3), "targetIndices out of bound")
  expect_error(knearest_Fallback(data_cross, c(-1), 3), "targetIndices out of bound")
})

test_that("knearest_Fallback throws error when required KNN is out of bound", {
  N <- nrow(data_cross)
  expect_error(knearest_Fallback(data_cross, 1:3, N + 1), "number of nearest neighbours")
  expect_error(knearest_Fallback(data_cross, 1:3, 0), "number of nearest neighbours")
  expect_error(knearest_Fallback(data_cross, 1:3, -1), "number of nearest neighbours")
})

test_that("knearest_Fallback returns targetIndices when length(targetIndices) >= K + 1", {
  expect_equal(sort(knearest_Fallback(data_cross, c(2, 4), 1)), c(2, 4))
  expect_equal(sort(knearest_Fallback(data_cross, 1:3, 2)), 1:3)
  N <- nrow(data_cross)
  expect_equal(sort(knearest_Fallback(data_cross, 1:N, N-1)), 1:N)
})

test_that("knearest_Fallback returns correct NN", {
  expect_length(knearest_Fallback(data_cross, 1, 4), 5)
  expect_equal(sort(knearest_Fallback(data_cross, 1, 4)), 1:5)
  expect_length(knearest_Fallback(data_cross, 1, 2), 3)
  expect_true(all(knearest_Fallback(data_cross, 1, 2) %in% 1:5))
  expect_length(knearest_Fallback(data_cross, 13, 3), 4)
  expect_equal(sort(knearest_Fallback(data_cross, 13, 3)), c(1, 5, 9, 13))
  expect_length(knearest_Fallback(data_cross, c(3, 1), 2), 3)
  expect_true(all(knearest_Fallback(data_cross, c(3, 1), 2) %in% c(1:5, 7)))
})
