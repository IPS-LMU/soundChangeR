logical_max <- function(x, vec = vector(mode = "logical", length = length(x))) {
  vec[which.max(x)] <- TRUE; vec
}
