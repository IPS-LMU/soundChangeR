generate_simulation_name <- function(prefix = "ABM") {
  paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S"))
}
