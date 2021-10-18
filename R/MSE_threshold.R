MSE_threshold  <- function(exemplar, features, label, agent, params) {
  if (is.numeric(params[["MSEthresholdMaxCoef"]])) {
    return(
      fdMSE(one_exemplar2fd(exemplar), FPCscores2fd(features, get_cache_value(agent, "FPCA"))) <=
        params[["MSEthresholdMaxCoef"]] * max(get_cache_value(agent, "MSE"))
    )
  } else if (is.numeric(params[["MSEthresholdQuantile"]])) {
    return(
      fdMSE(one_exemplar2fd(exemplar), FPCscores2fd(features, get_cache_value(agent, "FPCA"))) <=
        quantile(get_cache_value(agent, "MSE"), probs = params[["MSEthresholdQuantile"]])
    )
  }
  return(TRUE)
}
