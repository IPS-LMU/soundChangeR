assign_words_to_phonemes <- function(wordClusters) {
  data.table::data.table(word = wordClusters %>% base::rownames(),
                         phoneme = wordClusters %>% base::apply(1, base::which.max) %>% base::factor())
}
