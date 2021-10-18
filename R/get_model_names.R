get_model_names <- function(agent, params) {
  if (length(params$features) == 1) {
    mdl <- c("E", "V")
  } else {
    mdl <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "EEV", "VEV", "EVV", "VVV")
  }
}
