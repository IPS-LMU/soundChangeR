estimate_number_of_clusters_from_purity_matrix <- function(purityMatrix, purityThr) {
  1 + Position(function(x) {!is.na(x) & x > purityThr},
               purityMatrix %>% apply(1, mean),
               right = TRUE,
               nomatch = 0)
}
