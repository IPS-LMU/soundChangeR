estimate_gaussian <- function(features, epsilon_diag = 1e-6) {

  gaussParams <- base::list(
    mean = base::apply(features, 2, base::mean),
    cov = stats::cov(features))

  epsilon_diag <- 1e-6
  while (!matrixcalc::is.positive.definite(gaussParams$cov)) {
    gaussParams$cov <- gaussParams$cov + epsilon_diag * base::diag(base::nrow(gaussParams$cov))
    epsilon_diag <- 2 * epsilon_diag
  }
  gaussParams$invcov <- base::solve(gaussParams$cov)
  return(gaussParams)
}
