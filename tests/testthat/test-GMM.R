context("GMM")

test_that("logical_max produces expected result in ordinary conditions", {
  x <- c(1, 1.2, 0) 
  m <- c(FALSE, TRUE, FALSE)
  expect_equal(logical_max(x), m)
})

test_that("logical_max produces a vector with only one TRUE value in case of a tie", {
  x <- c(-1, 1.2, 1.2) 
  expect_equal(sum(logical_max(x)), 1)
})

test_that("logical_max produces expected result also with row or column matrix", {
  x <- matrix(c(1, 1.2, 0) , nrow = 1)
  m <- c(FALSE, TRUE, FALSE)
  expect_equal(logical_max(x), m)
  expect_equal(logical_max(t(x)), m)
})  

test_that("reduced_word_cluster produces consistent results in ordinary conditions", {
  NWords <- 20
  wc <- matrix(runif(NWords * 4, 0, 30) %>% round, nrow = NWords)
  rownames(wc) <- letters[1:NWords]
  for (rank in 2:4) {
    rwc <- reduced_word_clusters(wc, rank)
    expect_equal(dim(rwc), c(NWords, rank))
    expect_equal(rownames(rwc), rownames(wc))
    expect_equal(apply(rwc, 1, sum), apply(wc, 1, sum))
  }
}) 

test_that("reduced_word_cluster produces expected result when one cluster contains only one element", {
  NWordsCluster <- 6
  wc <- do.call(rbind, lapply(1:3, function(i) {matrix(diag(3)[i,], nrow = NWordsCluster, ncol = 3, byrow = TRUE) })) %*%
    diag(c(50, 52, 53))
  wc <- cbind(wc, 0)
  wc[1, 4] <- 1
  rownames(wc) <- letters[1:(3*NWordsCluster)]
  rwc <- reduced_word_clusters(wc, 3)
  wc.sortedCols <- wc[-1, -4] %>% apply(2, paste0, collapse = '') %>% sort
  rwc.sortedCols <- rwc[-1, ] %>% apply(2, paste0, collapse = '') %>% sort
  expect_equal(wc.sortedCols, rwc.sortedCols)
})

test_that("reduced_word_cluster tolerates zero columns when the result would not contain zero columns", {
  NWordsCluster <- 6
  wc <- do.call(rbind, lapply(1:3, function(i) {matrix(diag(3)[i,], nrow = NWordsCluster, ncol = 3, byrow = TRUE) })) %*%
    diag(c(50, 52, 53))
  wc <- cbind(wc, 0)
  rownames(wc) <- letters[1:(3*NWordsCluster)]
  rwc <- reduced_word_clusters(wc, 3)
  wc.sortedCols <- wc[, -4] %>% apply(2, paste0, collapse = '') %>% sort
  rwc.sortedCols <- rwc %>% apply(2, paste0, collapse = '') %>% sort
  expect_equal(wc.sortedCols, rwc.sortedCols)
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

test_that("estimate_number_of_labels_from_purity_matrix produces expected results", {
  nCol <- 5
  pm <- matrix(c(.98, .94, .83, .73), nrow = 4, ncol = nCol, byrow = FALSE) + runif(4 * nCol, -.01, .01)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, 1), 1)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .995), 1)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .96), 2)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .90), 3)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .80), 4)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .70), 5)
  pm[4,1] <- NaN
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, 1), 1)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .995), 1)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .96), 2)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .90), 3)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .80), 4)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .70), 4)
  pm[2,3] <- NaN
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, 1), 1)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .995), 1)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .96), 2)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .90), 2)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .80), 4)
  expect_equal(estimate_number_of_labels_from_purity_matrix(pm, .70), 4)
})

test_that("compute_mahal_distance produces expected results depending on dimensionalities", {
  agent <- list(cache = data.table(name = "GMM", value = list(), valid = FALSE))
  # case multidim features, 1 GMM component
  gmm <- readRDS("GMM.model1.G1.d3.rds")
  set_cache_value(agent, "GMM", gmm)
  features <- gmm$models$`1`$parameters$mean %>% t # the mean should give distance = 0
  expect_equal(compute_mahal_distance(agent, features, label = 1, method = "GMM"), 0)
  features <- gmm$data[1,]
  expect_gt(compute_mahal_distance(agent, features, label = 1, method = "GMM"), 0)
  # case 1-dim features, 2 GMM components
  gmm <- readRDS("GMM.model2.G2.2.d1.rds")
  set_cache_value(agent, "GMM", gmm)
  features <- gmm$models$`1`$parameters$mean[1]  # the mean should give distance = 0
  expect_equal(compute_mahal_distance(agent, features, label = 1, method = "GMM"), 0)
  features <- gmm$data[1,]
  expect_gt(compute_mahal_distance(agent, features, label = 1, method = "GMM"), 0)
  # case multidim features, many GMM components
  gmm <- readRDS("GMM.model4.G3.4.3.3.d3.rds")
  set_cache_value(agent, "GMM", gmm)
  features <- gmm$models$`2`$parameters$mean[,3] %>% t # the mean should give distance = 0
  expect_equal(compute_mahal_distance(agent, features, label = 2, method = "GMM"), 0)
  features <- gmm$data[1,]
  expect_gt(compute_mahal_distance(agent, features, label = 1, method = "GMM"), 0)
})

test_that("compute_posterior_probabilities produces expected results depending on dimensionalities", {
  agent <- list(cache = data.table(name = "GMM", value = list(), valid = FALSE))
  # case multidim features, 1 GMM component, 1 label (model)
  gmm <- readRDS("GMM.model1.G1.d3.rds")
  set_cache_value(agent, "GMM", gmm)
  # 1 model, so we get prob == 1 anyway
  features <- gmm$data[1,,drop = FALSE]
  posteriorProb <- compute_posterior_probabilities(agent, features, method = "GMM")
  expect_equal(dim(posteriorProb), c(1,1))
  expect_equal(posteriorProb[1,1] %>% as.numeric, 1)
  expect_true(recognize_posterior_probabilities(posteriorProb, 1, "maxPosteriorProb"))
  # case 1-dim features, 2 GMM components, 2 models (labels)
  gmm <- readRDS("GMM.model2.G2.2.d1.rds")
  set_cache_value(agent, "GMM", gmm)
  features <- gmm$data[1,,drop = FALSE]
  posteriorProb <- compute_posterior_probabilities(agent, features, method = "GMM")
  expect_equal(dim(posteriorProb), c(1,2))
  expect_equal(posteriorProb %>% as.numeric %>% sum, 1)
  # case multidim features, many GMM components, 4 models (labels) 
  gmm <- readRDS("GMM.model4.G3.4.3.3.d3.rds")
  set_cache_value(agent, "GMM", gmm)
  features <- gmm$data[1,,drop = FALSE]
  posteriorProb <- compute_posterior_probabilities(agent, features, method = "GMM")
  expect_equal(dim(posteriorProb), c(1,4))
  expect_equal(posteriorProb %>% as.numeric %>% sum, 1)
})

test_that("assign_words_to_labels and estimate_GMM produces expected results depending on dimensionalities", {
  params <- list()
  params[['purityRepetitions']] <- 5
  params[['purityThreshold']] <- 0.9
  # case 1-dim features
  agent <- readRDS("agent1d.rds")
  nValid <- agent$memory[valid == TRUE, .N]
  words <- agent$memory[valid == TRUE, word] %>% unique %>% sort
  w2l <- assign_words_to_labels(agent, params)
  expect_equal(names(w2l) %>% sort, c("label", "word"))
  expect_equal(w2l$word %>% sort, words)
  expect_lte(w2l$label %>% uniqueN, length(words))
  estimate_GMM(agent, params)
  expect_equal(agent$memory[valid == TRUE, .N], nValid)
  nlabels <- agent$memory[valid == TRUE, label] %>% uniqueN
  wordLabels <- agent$memory[valid == TRUE, .(word, label)] %>% table %>% unclass
  expect_equal(apply(wordLabels, 1, function(x) {(x > 0) %>% sum}) %>% as.integer, rep(1, length(words)))
  expect_equal(apply(wordLabels, 1, function(x) {(x == 0) %>% sum}) %>% as.integer, rep(nlabels - 1, length(words)))
  agent$memory[valid == FALSE, word := "_invalid_word_"]
  expect_error(estimate_GMM(agent, params), NA)
  expect_equal(agent$memory[valid == TRUE, .N], nValid)
  # case 3-dim features
  agent <- readRDS("agent3d.rds")
  nValid <- agent$memory[valid == TRUE, .N]
  words <- agent$memory[valid == TRUE, word] %>% unique %>% sort
  w2l <- assign_words_to_labels(agent, params)
  expect_equal(names(w2l) %>% sort, c("label", "word"))
  expect_equal(w2l$word %>% sort, words)
  expect_lte(w2l$label %>% uniqueN, length(words))
  estimate_GMM(agent, params)
  expect_equal(agent$memory[valid == TRUE, .N], nValid)
  nlabels <- agent$memory[valid == TRUE, label] %>% uniqueN
  wordLabels <- agent$memory[valid == TRUE, .(word, label)] %>% table %>% unclass
  expect_equal(apply(wordLabels, 1, function(x) {(x > 0) %>% sum}) %>% as.integer, rep(1, length(words)))
  expect_equal(apply(wordLabels, 1, function(x) {(x == 0) %>% sum}) %>% as.integer, rep(nlabels - 1, length(words)))
  agent$memory[valid == FALSE, word := "_invalid_word_"]
  expect_error(estimate_GMM(agent, params), NA)
  expect_equal(agent$memory[valid == TRUE, .N], nValid)
})


