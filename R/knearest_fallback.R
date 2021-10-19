knearest_fallback <- function(points, extendedIndices, targetIndices, K) {

  if (base::any(base::c(targetIndices, extendedIndices) < 0)) {
    stop("knearest_fallback: negative index notation not supported for targetIndices and extendedIndices")
  }
  if (base::nrow(points) == 0) {
    return (NULL)
  }
  if (K <= 0) {
    stop(base::paste("knearest_fallback: invalid number of nearest neighbours requested: K =", K))
  }

  if (!base::all(extendedIndices %in% base::seq_len(base::nrow(points)))) {
    stop("knearest_fallback: extendedIndices out of bound")
  }
  if (base::length(targetIndices) <= 0) {
    stop("knearest_fallback: empty targetIndices")
  }
  if (base::is.null(extendedIndices) | base::length(extendedIndices) == 0) {
    return(targetIndices)
  }
  if (!base::all(targetIndices %in% extendedIndices)) {
    stop("knearest_fallback: targetIndices out of bound")
  }
  if (K + 1 > base::length(extendedIndices)) {
    return(extendedIndices)
  }

  nFallback <- K + 1 - base::length(targetIndices)

  if (nFallback <= 0) {
    return(targetIndices)
  }

  fallbackIndices <- FNN::knnx.index(points[extendedIndices, , drop = FALSE], points[targetIndices, , drop = FALSE], K + 1) %>%
    base::as.vector() %>%
    extendedIndices[.] %>%
    base::setdiff(targetIndices) %>%
    base::sample(nFallback)

  return(base::c(targetIndices, fallbackIndices))
}
