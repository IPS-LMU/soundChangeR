#' Get columns with acoustic features
#'
#' @param data a data frame; usually either input data as loaded by load_input_data() or population as loaded by load_pop()
#'
#' @return names of acoustic feature column(s)
#' @export
get_Pcols <- function(data) {
  
  base::grep("^P[0-9]+$", base::colnames(data), value = TRUE)
}
