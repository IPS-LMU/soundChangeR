# testthat folder
ABMpath <- "/homes/m.gubian/ABM/ABM" # your ABM home dir here
testDir <- file.path(ABMpath, "tests", "testthat")
# load all the funcions and libraries (this may not be needed when the code becomes a package)
source(file.path(rootDir, "Rcmd", "loadLibraries.R"))


context("SMOTE")

# test data
# a cross made of 13 points, centered in c(0,0), 4 arms made of 3 points each,
# e.g. the right arm is c(0,1), c(0,2) and c(0,3)
# points are in clockwise order from 9 o'clock, from inside out, centre is the first one.

#         11
#         7
#         3
# 10  6 2 1 4 8 12     
#         5
#         9
#         13

data_cross <- rbind(c(0,0))
cross_points <- rbind( c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
for (i in 1:3) {
  data_cross <- rbind(data_cross, i * cross_points)
}

test_that("knearest_fallback throws error for cloud that is not a table", {
  expect_error(knearest_fallback(1, 1, 1))
})

test_that("knearest_fallback returns NULL when cloud is empty", {
  expect_null(knearest_fallback(data_cross[FALSE,], 0, 0))
})


test_that("knearest_fallback throws error for targetIndices out of bound", {
  N <- nrow(data_cross)
  expect_error(knearest_fallback(data_cross, c(1, 2, N + 1), 2), "targetIndices out of bound")
  expect_error(knearest_fallback(data_cross, c(1, 2, N + 1), 3), "targetIndices out of bound")
  expect_error(knearest_fallback(data_cross, c(1, 2, 0), 3), "targetIndices out of bound")
  expect_error(knearest_fallback(data_cross, c(-1), 3), "targetIndices out of bound")
})

test_that("knearest_fallback throws error when K <= 0", {
  expect_error(knearest_fallback(data_cross, 1:3, 0), "invalid number of nearest neighbours")
  expect_error(knearest_fallback(data_cross, 1:3, -1), "invalid number of nearest neighbours")
})

test_that("knearest_fallback returns cloud when K > nrow(cloud)", {
  N <- nrow(data_cross)
  expect_equal(knearest_fallback(data_cross, 1:3, N + 1), 1:N)
})

test_that("knearest_fallback returns targetIndices when length(targetIndices) >= K + 1", {
  expect_equal(sort(knearest_fallback(data_cross, c(2, 4), 1)), c(2, 4))
  expect_equal(sort(knearest_fallback(data_cross, 1:3, 2)), 1:3)
  N <- nrow(data_cross)
  expect_equal(sort(knearest_fallback(data_cross, 1:N, N-1)), 1:N)
})

test_that("knearest_fallback returns correct NN", {
  expect_length(knearest_fallback(data_cross, 1, 4), 5)
  expect_equal(sort(knearest_fallback(data_cross, 1, 4)), 1:5)
  expect_length(knearest_fallback(data_cross, 1, 2), 3)
  expect_true(all(knearest_fallback(data_cross, 1, 2) %in% 1:5))
  expect_length(knearest_fallback(data_cross, 13, 3), 4)
  expect_equal(sort(knearest_fallback(data_cross, 13, 3)), c(1, 5, 9, 13))
  expect_length(knearest_fallback(data_cross, c(3, 1), 2), 3)
  expect_true(all(knearest_fallback(data_cross, c(3, 1), 2) %in% c(1:5, 7)))
})

test_that("smote_one_class throws error when K <= 0", {
  expect_error(smote_one_class(data_cross, 0, 1))
})

test_that("smote_one_class throws error when N <= 0", {
  expect_error(smote_one_class(data_cross, 1, 0))
})

test_that("smote_one_class returns NULL when features is empty", {
  expect_null(smote_one_class(data.frame(data_cross)[FALSE,], 1, 1))
})

test_that("smote_one_class returns N copies of the only token when features has only one token (row)", {
  features <- matrix(rep(1:2, 3), ncol = 2, byrow = TRUE)
  expect_equal(smote_one_class(features[1,,drop=FALSE], 10, 3), features)
})

test_that("smote_one_class returns N tokens when there are no errors", {
  expect_equal(nrow(smote_one_class(data_cross, 3, 7)), 7)
  expect_equal(nrow(smote_one_class(data_cross, 1, 77)), 77)
})

test_that("SMOTE geometry: all N points within the diamond connecting the external points of data_cross", {
  N <- 100
  expect_equal(smote_one_class(data_cross, 3, N) %>%
                 data.table %>%
                 setnames(c('x','y')) %>%
                 .[data.table::between(y, x - 3, x + 3) & data.table::between(y, - x - 3, - x + 3), .N],
               N)
})

test_that("SMOTE geometry: all N points within the diamond connecting the points of inner cross", {
  N <- 100
  expect_equal(smote_one_class(data_cross[1:5,], 3, N) %>%
                 data.table %>%
                 setnames(c('x','y')) %>%
                 .[data.table::between(y, x - 1, x + 1) & data.table::between(y, - x - 1, - x + 1), .N],
               N)
})

test_that("SMOTE geometry: all N points on y=0 line", {
  N <- 100
  expect_equal(smote_one_class(data_cross[c(10, 6, 2, 1, 4, 8, 12),], 3, N) %>%
                 data.table %>%
                 setnames(c('x','y')) %>%
                 .[y ==0, .N],
               N)
})

test_that("SMOTE geometry: all N points on x=0 line", {
  N <- 100
  expect_equal(smote_one_class(data_cross[c(11, 7, 3, 1, 5, 9, 13),], 3, N) %>%
                 data.table %>%
                 setnames(c('x','y')) %>%
                 .[x ==0, .N],
               N)
})
