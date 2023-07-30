get_model_names <- function(agent, params) {
  
  if (base::length(c(params$feature_set1, params$feature_set2)) == 1) {
    mdl <- base::c("E", "V")
  } else {
    mdl <- base::c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "EEV", "VEV", "EVV", "VVV")
  }
}
