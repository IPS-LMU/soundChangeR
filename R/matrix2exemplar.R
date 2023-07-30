matrix2exemplar <- function(mat) {
  
  base::apply(mat, 1, rowMatrix2exemplar) %>% base::unlist(recursive = FALSE)
  # rowMatrix2exemplar wird auf jede Zeile von input.df (= mat) ausgeführt
}
