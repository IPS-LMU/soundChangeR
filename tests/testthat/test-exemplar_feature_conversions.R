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

test_that("fdMSE throws error when fd coefs dimensions don't match", {
  basis0 <- create.bspline.basis(c(1.21, 20.92), 8)
  fd0 <- fd(coef = rnorm(8), basisobj = basis0)
  fd1 <- fd(coef = array(rnorm(32), dim = c(8,2,2)), basisobj = basis0)
  expect_error(fdMSE(fd0, fd1))
  fd2 <- fd(coef = array(rnorm(8), dim = c(8,1,1)), basisobj = basis0)
  expect_error(fdMSE(fd0, fd2))
})

test_that("fdMSE returns 0 with identical arguments", {
  basis0 <- create.bspline.basis(c(1.21, 20.92), 8)
  fd0 <- fd(coef = rnorm(8), basisobj = basis0)
  expect_equal(fdMSE(fd0, fd0), 0)
  fd1 <- fd(coef = array(rnorm(32), dim = c(8,2,2)), basisobj = basis0)
  expect_equal(fdMSE(fd1, fd1), 0)
})

test_that("fdMSE returns expected time-normalised MSE values", {
  basis0 <- create.bspline.basis(c(1.21, 20.92), 8)
  # all flat horizontal lines
  fd0 <- fd(coef = rep(3, 8), basisobj = basis0)
  fd1 <- fd(coef = rep(3.5, 8), basisobj = basis0)
  expect_equal(fdMSE(fd0, fd1), (3.5 - 3)^2)
  expect_equal(fdMSE(fd1, fd0), (3.5 - 3)^2)
  fd0 <- fd(coef = array(rep(3, 32), dim = c(8,2,2)), basisobj = basis0)
  fd1 <- fd(coef = array(rep(c(3, 3.5, 2.5, 4), each = 8), dim = c(8,2,2)), basisobj = basis0)
  expect_equal(fdMSE(fd0, fd1), (3-3)^2+(3-3.5)^2+(3-2.5)^2+(3-4)^2)
  expect_equal(fdMSE(fd0, fd1), fdMSE(fd1, fd0))
})

y_fd <- readRDS("ex4_fd.rds")
yz_fd <- readRDS("ex8_fd.rds")
y_pcafd <- readRDS("ex4_fpca.rds")
yz_pcafd <- readRDS("ex8_fpca.rds")

test_that("compute_FPCscores produces values close to actual scores on training set", {
  expect_equal(
    sapply(seq_len(dim(y_fd$coefs)[2]) , function(i) {compute_FPCscores(y_fd[i], y_pcafd, 2)}) %>% t,
    y_pcafd$scores[, 1:2] %>% `dimnames<-`(NULL),
    tolerance = 1e-3
  )
  expect_equal(
    sapply(seq_len(dim(yz_fd$coefs)[2]) , function(i) {compute_FPCscores(yz_fd[i], yz_pcafd, 2)}) %>% t,
    yz_pcafd$scores[, 1:2] %>% `dimnames<-`(NULL),
    tolerance = 1e-3
  )
})

test_that("FPCscores2fd accepts scores as vector or matrix", {
  expect_identical(
    FPCscores2fd(rep(0, 7), y_pcafd),
    FPCscores2fd(rep(0, 7) %>% as.matrix(ncol = 1), y_pcafd)
  )
  expect_identical(
    FPCscores2fd(rep(0, 7), y_pcafd),
    FPCscores2fd(rep(0, 7) %>% as.matrix(nrow = 1), y_pcafd)
  )
})

test_that("FPCscores2fd returns the mean curve when PC scores are all 0", {
  fdObj <- FPCscores2fd(rep(0, 7), y_pcafd)
  expect_is(fdObj, "fd")
  expect_identical(fdObj$basis, y_pcafd$meanfd$basis)
  expect_identical(fdObj$coefs %>% `dimnames<-`(NULL), y_pcafd$meanfd$coefs %>% `dimnames<-`(NULL))
  
  fdObj <- FPCscores2fd(rep(0, 7), yz_pcafd)
  expect_is(fdObj, "fd")
  expect_identical(fdObj$basis, yz_pcafd$meanfd$basis)
  expect_identical(fdObj$coefs %>% `dimnames<-`(NULL), yz_pcafd$meanfd$coefs %>% drop %>% `dimnames<-`(NULL))
})

test_that("FPCscores2fd produces curves close to training set from their corresponding scores", {
  res <- sapply(seq_len(dim(y_fd$coefs)[2]) , function(i) {
    FPCscores2fd(y_pcafd$scores[i,], y_pcafd)$coefs 
  }) %>% `dimnames<-`(NULL)
  expect_equal(res, y_fd$coefs %>% `dimnames<-`(NULL), tolerance = 1e-6)
  
  res <- sapply(seq_len(dim(yz_fd$coefs)[2]) , function(i) {
    FPCscores2fd(yz_pcafd$scores[i,], yz_pcafd)$coefs 
  }, simplify = "array") %>% aperm(c(1,3,2)) %>% `dimnames<-`(NULL)
  expect_equal(res, yz_fd$coefs %>% `dimnames<-`(NULL), tolerance = 1e-6)
})

agent <- list()
agent$cache <- data.table(name = c("FPCA", "nPC", "MSE"), value = list(), valid = FALSE)
params <- list(
  lambdaFPCA = 1e-08,
  varCutoffFPCA = .995
)

test_that("compute_fpca produces expected results", {
  features <- compute_fpca(y_fd %>% all_fd2exemplar, agent, params)
  expect_is(features, "matrix")
  expect_identical(features %>% nrow, y_pcafd$scores %>% nrow)
  expect_identical(features[, 1], y_pcafd$scores[, 1])
  expect_identical(agent$cache[name == "FPCA", value][[1]], y_pcafd)
  expect_equal(agent$cache[name == "nPC", value][[1]], 3) # with lambdaFPCA = 1e-08, varCutoffFPCA = .995
  expect_identical(features, y_pcafd$scores[, 1:(agent$cache[name == "nPC", value][[1]])])
  expect_equal(agent$cache[name == "MSE", value][[1]] %>% length, y_fd$coefs %>% dim %>% .[2])
  
  features <- compute_fpca(yz_fd %>% all_fd2exemplar, agent, params)
  expect_is(features, "matrix")
  expect_identical(features %>% nrow, y_pcafd$scores %>% nrow)
  expect_identical(features[, 1], yz_pcafd$scores[, 1])
  expect_identical(agent$cache[name == "FPCA", value][[1]], yz_pcafd)
  expect_equal(agent$cache[name == "nPC", value][[1]], 6) # with lambdaFPCA = 1e-08, varCutoffFPCA = .995
  expect_identical(features, yz_pcafd$scores[, 1:(agent$cache[name == "nPC", value][[1]])])
  expect_equal(agent$cache[name == "MSE", value][[1]] %>% length, yz_fd$coefs %>% dim %>% .[2])
})

agent <- list()
agent$cache <- data.table(name = c("FPCA", "nPC", "MSE"),
                          value = c(list(y_pcafd), list(3), list(readRDS("ex4_MSE.rds"))),
                          valid = TRUE)
# ex4_MSE.rds contains FPCA MSE computed on ex4_fd.rds with 3 PCs
params <- list(
  memoryIntakeStrategy = c("MSEthreshold")
)

test_that("below_MSE_threshold produces expected results", {
  nItems <- dim(y_fd$coefs)[2]
  sapply(c(0.5, 1, 2), function(th) {
    params[["MSEthresholdMaxCoef"]] <- th
    sapply(seq_len(nItems), function(i) {
      below_MSE_threshold(exemplar = one_fd2exemplar(y_fd[i]),
                          features = y_pcafd$scores[i, 1:3],
                          agent = agent, params = params)
    })
  }) %>%
    apply(2L, sum) -> res
  expect_lt(res[1], nItems)
  expect_equal(res[2], nItems)
  expect_equal(res[3], nItems)
})
