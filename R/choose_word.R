choose_word <- function(memory, method = "random_word") {

  if (!base::all(base::c("word", "valid") %in% base::names(memory))) {
    stop("choose_word: memory data.table should have a 'word' and a 'valid' column")
  }

  if (base::sum(memory$valid == TRUE) == 0) {
    return (NULL)
  }

  if (method == "random_word" | is.null(method)) {
    memory$word[memory$valid == TRUE] %>% base::unique() %>% base::sample(1)
  } else {
    stop(base::paste("choose_word: Unknown method", method))
  }
}
