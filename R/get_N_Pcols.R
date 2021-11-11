#' Compute number of columns with acoustic features
#'
#' @param data a data frame; usually either input data as loaded by load_input_data() or population as loaded by load_pop()
#'
#' @return number of columns
#' @export
get_N_Pcols <-  function(data) {
  
  Pcols <- get_Pcols(data)
  len <- base::length(Pcols)
  if (len == 0) return(0)
  if(!base::all.equal(Pcols, base::paste0("P", 1:len))) {
    stop(base::paste("Not contiguous sequence of feature column names:", Pcols))
  }
  return(len)
}
