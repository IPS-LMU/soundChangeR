getNPcols <-  function(memory) {
  
  Pcols <- getPcols(memory)
  len <- base::length(Pcols)
  if (len == 0) return(0)
  if(!base::all.equal(Pcols, base::paste0("P", 1:len))) {
    stop(base::paste("Not contiguous sequence of feature column names:", Pcols))
  }
  return(len)
}
