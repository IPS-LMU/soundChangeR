smote_resampling <- function(points, extendedIndices = NULL, targetIndices, K, N) {

  fallbackIndices <- knearest_fallback(points, extendedIndices, targetIndices, K)
  smote_one_class(points[fallbackIndices, , drop = FALSE], K, N)
}
