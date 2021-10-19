getPcols <- function(memory) {
  
  base::grep("^P[0-9]+$", base::colnames(memory), value = TRUE)
}
