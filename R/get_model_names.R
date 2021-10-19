get_model_names <- function(agent, params) {
  
  if (base::length(params$features) == 1) {
    mdl <- base::c("E", "V")
  } else {
    mdl <- base::c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "EEV", "VEV", "EVV", "VVV")
  }
}
