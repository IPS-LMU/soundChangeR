water_filling <- function(mat, eps = 1e-6) {
  if (!is.square.matrix(mat)) stop("water_filling: input mat is not a square matrix")
  eps <- max(eps, min(diag(mat)))
  while(is.singular.matrix(mat)) {
    mat <- mat + eps * diag(nrow(mat))
  }
  return(mat)
}
