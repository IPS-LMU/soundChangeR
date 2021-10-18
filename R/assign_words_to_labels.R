assign_words_to_labels <- function(wordClusters) {
  data.table(word = wordClusters %>% rownames,
             label = wordClusters %>% apply(1, which.max) %>% factor)
}
