apply_resampling <- function(agent, finalN, params) {

  initialN <- agent$memory[valid == TRUE, .N]
  if (initialN >= finalN)
    return()

  extraN <- min(finalN, nrow(agent$memory)) - initialN

  tokens <- replicate(extraN, produce_token(agent, params), simplify = FALSE)
  if (params[["removeOriginalExemplarsAfterResampling"]]) {
    agent$memory[, valid := FALSE]
  }

  invisible(
    lapply(seq_along(tokens), function(i) {
      rowToWrite <- row_to_write(agent, tokens[[i]], params)
      write_memory(agent, tokens[[i]], rowToWrite, tokens[[i]]$label) # tokens[[i]]$memory$label
    })
  )
  if (grepl("^GMM(s)?", params[["perceptionModels"]])) {
    estimate_GMM(agent, params)
  }
}
