context("Exemplar from/to feature conversions")

input1.dt <- fread("input1.csv") %>%
  # from data.table::fread man page:
  # "NB: sep2 is not yet implemented."
  .[, exemplar := strsplit(exemplar, "|", fixed = TRUE) %>% lapply(as.numeric)]

features <- fread("features1_matrix.csv", header = FALSE) %>% as.matrix %>% `dimnames<-`(NULL)


test_that("exemplar2vector produces expected vector and matrix", {
  expect_equal(exemplar2vector(list(1:2, 3:4)), matrix(1:4, nrow = 2, byrow = TRUE))
  expect_equal(exemplar2vector(list(1:2)), matrix(1:2, nrow = 1))
  expect_equal(exemplar2vector(input1.dt[, exemplar]), features)
  expect_equal(exemplar2vector(input1.dt[3, exemplar]), features[3, , drop = FALSE])
})

test_that("vector2exemplar produces expected list", {
  expect_equal(vector2exemplar(matrix(1:2, nrow = 1)), list(1:2))
  expect_equal(vector2exemplar(matrix(1:2, nrow = 1) %>% data.table), list(1:2))
  expect_equal(vector2exemplar(1:2), list(1:2))
  expect_equal(vector2exemplar(features[3, , drop = FALSE]), input1.dt[3, exemplar])
})

test_that("matrix2exemplar produces expected list", {
  expect_equal(matrix2exemplar(matrix(1:4, nrow = 2, byrow = TRUE)), list(1:2, 3:4))
  expect_equal(matrix2exemplar(matrix(1:4, nrow = 2, byrow = TRUE) %>% data.table), list(1:2, 3:4))
  expect_equal(matrix2exemplar(features), input1.dt[, exemplar])
})

test_that("all_fd2exemplar produces expected list with 1D curves", {
  # ex1 <- all_fd2exemplar(fd(coef = matrix(1:8, nrow = 8), basisobj = create.bspline.basis(0:1, 8)))
  # expect_is(ex1, "list")
  # expect_length(ex1, 1)
  # expect_identical(ex1[[1]]$basis, create.bspline.basis(0:1, 8))
  # expect_identical(ex1[[1]]$coefs %>% `dimnames<-`(NULL), matrix(1:8, ncol = 1))
  
  ex2 <- all_fd2exemplar(fd(coef = matrix(1:16, nrow = 8), basisobj = create.bspline.basis(0:1, 8)))
  expect_is(ex2, "list")
  expect_length(ex2, 2)
  for (i in 1:2) {
    expect_identical(ex2[[i]]$basis, create.bspline.basis(0:1, 8))
  }
  expect_identical(ex2[[1]]$coefs %>% `dimnames<-`(NULL), matrix(1:8, ncol = 1))
  expect_identical(ex2[[2]]$coefs %>% `dimnames<-`(NULL), matrix(9:16, ncol = 1))
})

test_that("all_fd2exemplar produces expected list with 2D curves", {
  # ex1 <- all_fd2exemplar(fd(coef = array(1:16, dim = c(8,1,2)), basisobj = create.bspline.basis(0:1, 8)))
  # expect_is(ex1, "list")
  # expect_length(ex1, 1)
  # expect_identical(ex1[[1]]$basis, create.bspline.basis(0:1, 8))
  # expect_identical(ex1[[1]]$coefs %>% `dimnames<-`(NULL) %>% adrop(2), matrix(1:16, ncol = 2))
  
  ex2 <- all_fd2exemplar(fd(coef = array(1:32, dim = c(8,2,2)), basisobj = create.bspline.basis(0:1, 8)))
  expect_is(ex2, "list")
  expect_length(ex2, 2)
  for (i in 1:2) {
    expect_identical(ex2[[i]]$basis, create.bspline.basis(0:1, 8))
  }
  expect_identical(ex2[[1]]$coefs %>% `dimnames<-`(NULL), matrix(c(1:8, 17:24), ncol = 2))
  expect_identical(ex2[[2]]$coefs %>% `dimnames<-`(NULL), matrix(c(9:16, 25:32), ncol = 2))
})

test_that("all_exemplar2fd produces expected fd with 1D curves", {
  fd1 <- all_exemplar2fd(list(
    fd(coef = matrix(1:8, nrow = 8), basisobj = create.bspline.basis(0:1, 8)),
    fd(coef = matrix(9:16, nrow = 8), basisobj = create.bspline.basis(0:1, 8))
  ))
  expect_is(fd1, "fd")
  expect_identical(fd1$basis, create.bspline.basis(0:1, 8))
  expect_identical(fd1$coefs %>% dim, c(8L, 2L))
  expect_identical(fd1$coefs %>% `dimnames<-`(NULL), matrix(1:16, nrow = 8))
})

test_that("all_exemplar2fd produces expected fd with 2D curves", {
  fd1 <- all_exemplar2fd(list(
    fd(coef = matrix(1:16, nrow = 8), basisobj = create.bspline.basis(0:1, 8)),
    fd(coef = matrix(17:32, nrow = 8), basisobj = create.bspline.basis(0:1, 8))
  ))
  expect_is(fd1, "fd")
  expect_identical(fd1$basis, create.bspline.basis(0:1, 8))
  expect_identical(fd1$coefs %>% dim, c(8L, 2L, 2L))
  expect_identical(fd1$coefs %>% `dimnames<-`(NULL), array(c(1:8, 17:24, 9:16, 25:32), dim = c(8,2,2)))
})
