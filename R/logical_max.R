logical_max <- function(x, vec = base::vector(mode = "logical", length = base::length(x))) {
  vec[base::which.max(x)] <- TRUE; vec
}
