#' Run demo simulation
#'
#' @export
run_demo_simulation <- function() {
  paramsFile <- base::system.file("extdata", "params.yaml", package = "soundChangeR")
  run_simulation(paramsFile)
}