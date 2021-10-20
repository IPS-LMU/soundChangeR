get_N_Pcols <-  function(memory) {
  
  Pcols <- get_Pcols(memory)
  len <- base::length(Pcols)
  if (len == 0) return(0)
  if(!base::all.equal(Pcols, base::paste0("P", 1:len))) {
    stop(base::paste("Not contiguous sequence of feature column names:", Pcols))
  }
  return(len)
}
