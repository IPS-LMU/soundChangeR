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
N <- nrow(data_cross)
cross_inner_idx <- 1:5
cross_mid_idx <- 1:9

test_that("knearest_fallback throws error for cloud that is not a table/matrix", {
  expect_error(knearest_fallback(1, 1, 1, 1))
})

test_that("knearest_fallback returns NULL when cloud is empty", {
  expect_null(knearest_fallback(data_cross[FALSE,], 0, 0, 0))
})

test_that("knearest_fallback throws error when targetIndices are NULL, NA or length 0", {
  expect_error(knearest_fallback(data_cross, cross_mid_idx, NULL, 1))
  expect_error(knearest_fallback(data_cross, cross_mid_idx, NA, 1))
  expect_error(knearest_fallback(data_cross, cross_mid_idx, (1:5)[NULL], 1))
})

test_that("knearest_fallback throws error when extendedIndices is NA", {
  expect_error(knearest_fallback(data_cross, NA, cross_inner_idx, 2))
})

test_that("knearest_fallback returns targetIndices when extendedIndices is NULL or length 0", {
  expect_equal(knearest_fallback(data_cross, NULL, cross_inner_idx, 2), cross_inner_idx)
  expect_equal(knearest_fallback(data_cross, (1:5)[NULL], cross_inner_idx, 4), cross_inner_idx)
})

test_that("knearest_fallback throws error for targetIndices out of bound with respect to extendedIndices", {
  expect_error(knearest_fallback(data_cross, cross_inner_idx, cross_mid_idx, 2))
  expect_error(knearest_fallback(data_cross, 1:N, c(cross_inner_idx, N + 1), 3))
  expect_error(knearest_fallback(data_cross, 1:N, c(cross_inner_idx, 0), 3))
  expect_error(knearest_fallback(data_cross, 1:N, c(cross_inner_idx, -1), 3))
})

test_that("knearest_fallback throws error for extendedIndices out of bound", {
  expect_error(knearest_fallback(data_cross, cross_inner_idx, cross_mid_idx, 2))
  expect_error(knearest_fallback(data_cross, c(cross_inner_idx, N + 1), cross_inner_idx, 3))
  expect_error(knearest_fallback(data_cross, c(cross_inner_idx, 0), cross_inner_idx, 3))
  expect_error(knearest_fallback(data_cross, c(cross_inner_idx, -1), cross_inner_idx, 3))
})

test_that("knearest_fallback throws error for targetIndices or extendedIndices using negative notation", {
  expect_error(knearest_fallback(data_cross, -cross_inner_idx, -cross_mid_idx, 2))
  expect_error(knearest_fallback(data_cross, c(-1, 2), 1, 2))
  expect_error(knearest_fallback(data_cross, -N, cross_inner_idx, 2))
})

test_that("knearest_fallback throws error when K <= 0", {
  expect_error(knearest_fallback(data_cross, cross_mid_idx, cross_inner_idx, 0), "invalid number of nearest neighbours")
  expect_error(knearest_fallback(data_cross, cross_mid_idx, cross_inner_idx, -1), "invalid number of nearest neighbours")
})

test_that("knearest_fallback returns extendedIndices when K + 1 > length(extendedIndices)", {
  expect_equal(sort(knearest_fallback(data_cross, cross_mid_idx, cross_inner_idx, length(cross_mid_idx))),
               sort(cross_mid_idx))
})

test_that("knearest_fallback returns targetIndices when length(targetIndices) >= K + 1", {
  expect_equal(sort(knearest_fallback(data_cross, cross_mid_idx, cross_inner_idx, length(cross_inner_idx) - 1)),
               sort(cross_inner_idx))
  expect_equal(sort(knearest_fallback(data_cross, 1:N, 1:N, N-1)), 1:N)
})

test_that("knearest_fallback returns correct NN", {
  expect_length(knearest_fallback(data_cross, sample(cross_inner_idx), 1, 4), 5)
  expect_equal(sort(knearest_fallback(data_cross, sample(cross_inner_idx), 1, 4)), 1:5)
  expect_length(knearest_fallback(data_cross, sample(cross_inner_idx), 1, 2), 3)
  expect_true(all(knearest_fallback(data_cross, sample(cross_inner_idx), 1, 2) %in% cross_inner_idx))
  expect_length(knearest_fallback(data_cross, sample(1:N), 13, 3), 4)
  expect_equal(sort(knearest_fallback(data_cross, sample(1:N), 13, 3)), c(1, 5, 9, 13))
  expect_length(knearest_fallback(data_cross, sample(cross_mid_idx), c(3, 1), 2), 3)
  expect_true(all(knearest_fallback(data_cross, sample(cross_mid_idx), c(3, 1), 2) %in% c(1:5, 7)))
})

test_that("knearest_fallback works the same when points is a data.frame or data.table instead of a matrix", {
  expect_true(all(knearest_fallback(data.frame(data_cross), sample(cross_mid_idx), c(3, 1), 2) %in% c(1:5, 7)))
  expect_true(all(knearest_fallback(data.table(data_cross), sample(cross_mid_idx), c(3, 1), 2) %in% c(1:5, 7)))
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
