generate_simulation_name <- function(prefix = "ABM") {

  base::paste0(prefix, base::format(base::Sys.time(), "%Y%m%d%H%M%S"))
}
