water_filling <- function(mat, eps = 1e-6) {
  
  if (!matrixcalc::is.square.matrix(mat)) stop("water_filling: input mat is not a square matrix")
  eps <- base::max(eps, base::min(base::diag(mat)))
  while(matrixcalc::is.singular.matrix(mat)) {
    mat <- mat + eps * base::diag(base::nrow(mat))
  }
  return(mat)
}
