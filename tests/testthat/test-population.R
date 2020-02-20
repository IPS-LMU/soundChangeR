context("Population data structures manipulation")

input1.dt <- fread("input1.csv") %>%
  # from data.table::fread man page:
  # "NB: sep2 is not yet implemented."
  .[, exemplar := strsplit(exemplar, "|", fixed = TRUE) %>% lapply(as.numeric)]

# pop1_list <- readRDS('pop1_list.rds')
# pop1_dt <- readRDS('pop1_dt.rds')

# pop1_list_res <- create_population(input1.dt, params)
# 
# test_that("create_population matches expected output", {
#   
#   expect_equal(pop1_list_res %>% length, pop1_list %>% length)
#   for (i in seq_along(pop1_list)) {
#     expect_equal(pop1_list_res[[i]] %>% names, pop1_list[[i]] %>% names)
#     expect_equal(pop1_list_res[[i]]$agentID, pop1_list[[i]]$agentID)
#     expect_equal(pop1_list_res[[i]]$group, pop1_list[[i]]$group)
#     expect_equal(pop1_list_res[[i]]$speaker, pop1_list[[i]]$speaker)
#     expect_true(all_equal(pop1_list_res[[i]]$labels, pop1_list[[i]]$labels))
#     expect_true(all_equal(pop1_list_res[[i]]$features, pop1_list[[i]]$features))
#     expect_true(all_equal(pop1_list_res[[i]]$cache[, .(name, valid)], pop1_list[[i]]$cache[, .(name, valid)]))
#     expect_null(pop1_list_res[[i]]$cache[name == 'qda', value][[1]])
#   }
# })

params <- list(
  featureExtractionMethod = "identity",
  initialMemoryResampling = FALSE,
  proportionGroupTokens = 0.0
)

agent <- create_agent(2, input1.dt, "SP2", 20, params)
# expected results
agent.initial <- data.table(word = letters[1:4], initial = rep(LETTERS[1:2], each = 2))
agent.memory.counts <- data.table(word = c(letters[1:4], NA), label = c(rep(LETTERS[1:2], each = 2), NA), N = 4)

test_that("create_agent produces expected metadata", {
  expect_equal(agent$agentID, 2)
  expect_equal(agent$group, "all")
  expect_equal(agent$speaker, "SP2")
  expect_equal(agent$initial[order(word)], agent.initial)
})

test_that("create_agent produces memory with expected item counts", {
  expect_equal(agent$memory %>% nrow, 20)
  expect_equal(agent$memory[valid == TRUE, .N], 16)
  expect_equal(agent$memory[order(word), .N, by= .(word,label)], agent.memory.counts)
})

test_that("create_agent produces exemplar column features table with expected content (identity case)", {
  expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(length) %>% unlist %>% unique, 3)
  expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(function(x) {x[1]}) %>% unlist %>% sort,
               0.1 + 17:32)
  expect_equal(colnames(agent$features), paste0("P", 1:3))
  expect_equal(nrow(agent$features), nrow(agent$memory))
  expect_equal(agent$memory[valid == TRUE, exemplar] %>% lapply(function(x) {x[2]}) %>% unlist,
               agent$features[agent$memory$valid, P2])
})

