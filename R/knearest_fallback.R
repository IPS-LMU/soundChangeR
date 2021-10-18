knearest_fallback <- function(points, extendedIndices, targetIndices, K) {

  if (any(c(targetIndices, extendedIndices) < 0)) {
    stop("knearest_fallback: negative index notation not supported for targetIndices and extendedIndices")
  }
  if (nrow(points) == 0) {
    return (NULL)
  }
  if (K <= 0) {
    stop(paste("knearest_fallback: invalid number of nearest neighbours requested: K =", K))
  }

  if (!all(extendedIndices %in% seq_len(nrow(points)))) {

    stop("knearest_fallback: extendedIndices out of bound")
  }
  if (length(targetIndices) <= 0) {
    stop("knearest_fallback: empty targetIndices")
  }
  if (is.null(extendedIndices) | length(extendedIndices) == 0) {
    return(targetIndices)
  }
  if (!all(targetIndices %in% extendedIndices)) {
    stop("knearest_fallback: targetIndices out of bound")
  }
  if (K + 1 > length(extendedIndices)) {
    return(extendedIndices)
  }

  nFallback <- K + 1 - length(targetIndices)

  if (nFallback <= 0) {
    return(targetIndices)
  }

  fallbackIndices <- knnx.index(points[extendedIndices, , drop=FALSE], points[targetIndices, , drop=FALSE], K + 1) %>%
    as.vector %>%
    extendedIndices[.] %>%
    setdiff(targetIndices) %>%
    sample(nFallback)

  return(c(targetIndices, fallbackIndices))
}
