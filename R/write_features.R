write_features <- function(agent, features, rowToWrite) {

  for (colIdx in 1:ncol(agent$features)) {
    set(agent$features, as.integer(rowToWrite), colIdx, features[1, colIdx])
  }
}
