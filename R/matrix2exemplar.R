matrix2exemplar <- function(mat) {
  apply(mat, 1, rowMatrix2exemplar) %>% unlist(recursive = FALSE)
}
