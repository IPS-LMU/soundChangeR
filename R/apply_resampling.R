apply_resampling <- function(agent, finalN, params) {

  initialN <- agent$memory[valid == TRUE, .N]
  if (initialN >= finalN)
    return()

  extraN <- base::min(finalN, nrow(agent$memory)) - initialN

  tokens <- base::replicate(extraN, produce_token(agent, params), simplify = FALSE)
  if (params[["removeOriginalExemplarsAfterResampling"]]) {
    agent$memory[, valid := FALSE]
  }

  base::invisible(
    base::lapply(base::seq_along(tokens), function(i) {
      rowToWrite <- row_to_write(agent, tokens[[i]], params)
      write_memory(agent, params, tokens[[i]], rowToWrite, tokens[[i]]$label)
    })
  )
  if (params[["perceptionModels"]] == "GMM") {
    estimate_GMM(agent, params)
  }
}
