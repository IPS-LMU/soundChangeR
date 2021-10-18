getPcols <- function(memory) {
  grep("^P[0-9]+$", colnames(memory), value = TRUE)
}
