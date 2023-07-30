write_features <- function(agent, features, rowToWrite) {
 # features ist eine Matrix: die linken Spalten gehören zu Cuespace 1; die recgten zu Cuespace 2
 # Wenn nur 1 Cuespace verwendet wird, gehören alle Spalten zum Cuespace 1
  for (colIdx in 1:base::ncol(agent$features)) {
    data.table::set(agent$features, base::as.integer(rowToWrite), colIdx, features[1, colIdx])
  }
}
