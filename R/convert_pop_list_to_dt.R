convert_pop_list_to_dt <- function(pop, extraCols = list(condition = "x")) {

  rbindlist(lapply(seq_along(pop), function (i) {
    cbind(pop[[i]]$features, pop[[i]]$memory) %>%
      .[valid == TRUE] %>%
      inner_join(pop[[i]]$initial, by = "word") %>%
      setDT %>%
      .[, `:=`(agentID = pop[[i]]$agentID,
               speaker = pop[[i]]$speaker,
               group = pop[[i]]$group)] %>%
      .[, equivalence := equal_class(initial, label)]
  }), fill = TRUE) %>% {
    for (col in names(extraCols)) {
      .[, (col) := extraCols[[col]]]
    }
    .[]
  }
}
