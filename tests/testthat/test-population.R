context("Population data structures manipulation")

input1.dt <- fread("str_Arezzo.csv")
pop1_list <- readRDS('pop1_list.rds')
pop1_dt <- readRDS('pop1_dt.rds')

params <- list(maxMemoryExpansion = 1.1)
pop1_list_res <- create_population(input1.dt, params)

test_that("create_population matches expected output", {
  
  expect_equal(pop1_list_res %>% length, pop1_list %>% length)
  for (i in seq_along(pop1_list)) {
    expect_equal(pop1_list_res[[i]] %>% names, pop1_list[[i]] %>% names)
    expect_equal(pop1_list_res[[i]]$agentID, pop1_list[[i]]$agentID)
    expect_equal(pop1_list_res[[i]]$group, pop1_list[[i]]$group)
    expect_equal(pop1_list_res[[i]]$speaker, pop1_list[[i]]$speaker)
    expect_true(all_equal(pop1_list_res[[i]]$labels, pop1_list[[i]]$labels))
    expect_true(all_equal(pop1_list_res[[i]]$features, pop1_list[[i]]$features))
    expect_true(all_equal(pop1_list_res[[i]]$cache[, .(name, valid)], pop1_list[[i]]$cache[, .(name, valid)]))
    expect_null(pop1_list_res[[i]]$cache[name == 'qda', value][[1]])
  }
})


# pop.dt <- convert_pop_list_to_dt(pop.list, extraCols = list())
# pop.list_res <- convert_pop_dt_to_list(pop.dt)

