choose_word <- function(labels, method = "random_word") {

  if (!base::all(base::c("word", "valid") %in% base::names(labels))) {
    stop("choose_word: labels data.table should have a 'word' and a 'valid' column")
  }

  if (base::sum(labels$valid == TRUE) == 0) {
    return (NULL)
  }

  if (method == "random_word" | is.null(method)) {
    labels$word[labels$valid == TRUE] %>% base::unique() %>% base::sample(1)
  } else {
    stop(base::paste("choose_word: Unknown method", method))
  }
}
