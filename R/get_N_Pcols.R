#' Compute number of columns with acoustic features
#'
#' @param input.df input data frame as loaded by load_input_data()
#'
#' @return number of columns
#' @export
get_N_Pcols <-  function(input.df) {
  
  Pcols <- get_Pcols(input.df)
  len <- base::length(Pcols)
  if (len == 0) return(0)
  if(!base::all.equal(Pcols, base::paste0("P", 1:len))) {
    stop(base::paste("Not contiguous sequence of feature column names:", Pcols))
  }
  return(len)
}
