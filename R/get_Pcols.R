#' Get columns with acoustic features
#'
#' @param input.df input data frame as loaded by load_input_data()
#'
#' @return names of acoustic feature column(s)
#' @export
get_Pcols <- function(input.df) {
  
  base::grep("^P[0-9]+$", base::colnames(input.df), value = TRUE)
}
