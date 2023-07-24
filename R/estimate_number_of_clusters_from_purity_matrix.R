estimate_number_of_clusters_from_purity_matrix <- function(purityMatrix, purityThr) {

  1 + base::Position(function(x) {!base::is.na(x) & x > purityThr}, 
                     purityMatrix,
                     right = TRUE,
                     nomatch = 0)
}
