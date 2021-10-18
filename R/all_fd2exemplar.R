all_fd2exemplar <- function(fdObj) {

  nItems <- fdObj$coefs %>% dim %>% .[2]
  sapply(seq_len(nItems), function(i) {fdObj[i]}, simplify = FALSE)
}
