#' Title
#'
#' @param memory agent memory
#'
#' @return
#' @export
get_Pcols <- function(memory) {
  
  base::grep("^P[0-9]+$", base::colnames(memory), value = TRUE)
}
