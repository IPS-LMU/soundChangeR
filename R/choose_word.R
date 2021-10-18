choose_word <- function(labels, method = "random_word") {

  if (!all(c("word", "valid") %in% names(labels))) {
    stop("choose_word: labels data.table should have a 'word' and a 'valid' column")
  }

  if (sum(labels$valid == TRUE) == 0) {
    return (NULL)
  }

  if (method == "random_word" | is.null(method)) {
    labels$word[labels$valid == TRUE] %>% unique %>% sample(1)
  } else {
    stop(paste("choose_word: Unknown method", method))
  }
}
