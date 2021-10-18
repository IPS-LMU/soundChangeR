estimate_gaussian <- function(features, epsilon_diag = 1e-6) {

  gaussParams <- list(
    mean = apply(features, 2, mean),
    cov = cov(features))

  epsilon_diag <- 1e-6
  while (!is.positive.definite(gaussParams$cov)) {
    gaussParams$cov <- gaussParams$cov + epsilon_diag * diag(nrow(gaussParams$cov))
    epsilon_diag <- 2 * epsilon_diag
  }
  gaussParams$invcov <- solve(gaussParams$cov)
  return(gaussParams)
}
