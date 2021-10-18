getNPcols <-  function(memory) {
  Pcols <- getPcols(memory)
  len <- length(Pcols)
  if (len == 0) return(0)
  if(!all.equal(Pcols, paste0("P", 1:len))) {
    stop(paste("Not contiguous sequence of feature column names:", Pcols))
  }
  return(len)
}
