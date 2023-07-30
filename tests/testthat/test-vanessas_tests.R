test_that("logical_max produces expected result in ordinary conditions", {
  x <- c(1, 1.2, 0) 
  m <- c(FALSE, TRUE, FALSE)
  expect_equal(logical_max(x), m)
})

test_that("reduced_clusters_incidence_matrix returns a sound incidence matrix with correct dimensions", {
  NWords <- 20
  Nclusters <- 4
  wc <- matrix(runif(NWords * Nclusters, 0, 30) %>% round, nrow = NWords)
  rownames(wc) <- letters[1:NWords]
  for (rank in 2:4) {
    im <- reduced_clusters_incidence_matrix(wc, rank)
    expect_equal(dim(im), c(rank, Nclusters))
    expect_equal(apply(im, 2, sum), rep(1, ncol(im)))
  }
}) 

test_that("reduced_clusters_incidence_matrix produces expected result when one cluster contains only one element", {
  NWordsCluster <- 6
  wc <- do.call(rbind, lapply(1:3, function(i) {matrix(diag(3)[i,], nrow = NWordsCluster, ncol = 3, byrow = TRUE) })) %*%
    diag(c(50, 52, 53))
  wc <- cbind(wc, 0)
  wc[1, 4] <- 1
  rownames(wc) <- letters[1:(3*NWordsCluster)]
  im <- reduced_clusters_incidence_matrix(wc, 3)
  # cluster 4 should be merged to one of the others
  expect_equal(sum(im[which(im[,4]), ]), 2)
})

test_that("reduced_clusters_incidence_matrix tolerates zero columns when the result would not contain zero columns", {
  NWordsCluster <- 6
  wc <- do.call(rbind, lapply(1:3, function(i) {matrix(diag(3)[i,], nrow = NWordsCluster, ncol = 3, byrow = TRUE) })) %*%
    diag(c(50, 52, 53))
  wc <- cbind(wc[, 1], 0, wc[, 2], wc[, 3])
  rownames(wc) <- letters[1:(3*NWordsCluster)]
  im <- reduced_clusters_incidence_matrix(wc, 3)
  expect_equal(dim(im), c(3, 4))
  expect_equal(apply(im, 2, sum), c(1,0,1,1))
})

test_that("compute_purity produces expected results", {
  NWordsCluster <- 5
  wc <- do.call(rbind, lapply(1:3, function(i) {matrix(20 * diag(3)[i,], nrow = NWordsCluster, ncol = 3, byrow = TRUE) }))
  expect_equal(compute_purity(wc, min), 1)
  expect_equal(compute_purity(wc, mean), 1)
  wc[1, 1] <- 19
  wc[NWordsCluster + 1, 1] <- 1
  expect_equal(compute_purity(wc, min), 0.99)
  expect_equal(compute_purity(wc, mean), 1 - 0.01/3)
})  
