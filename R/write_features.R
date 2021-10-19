write_features <- function(agent, features, rowToWrite) {

  for (colIdx in 1:base::ncol(agent$features)) {
    data.table::set(agent$features, base::as.integer(rowToWrite), colIdx, features[1, colIdx])
  }
}
