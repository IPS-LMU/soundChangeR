assign_words_to_labels <- function(wordClusters) {
  data.table::data.table(word = wordClusters %>% base::rownames,
                         label = wordClusters %>% base::apply(1, base::which.max) %>% base::factor)
}
