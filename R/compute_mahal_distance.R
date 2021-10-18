compute_mahal_distance <- function(agent, features, label, method = NULL) {
  if (is.null(method) | method == "singleGaussian") {
    mahalanobis(features,
                apply(as.matrix(agent$features)[agent$memory$valid == TRUE &
                                                  agent$memory$label == label, , drop = FALSE], 2, mean),
                cov(as.matrix(agent$features)[agent$memory$valid == TRUE &
                                                agent$memory$label == label, , drop = FALSE]), tol=1e-30)

  } else if (grepl("^GMM(s)?", method)) {
    GMM <- get_cache_value(agent, "GMM")

    tryCatch({
      compute_mahal_distances_GMM(GMM$models[[label]], features) %>% min
    }, error = function(c) {
      write_log(paste("compute_mahal_distances_GMM", "label", label, conditionMessage(c),
                      paste(conditionCall(c), collapse = " "), sep = "\n"), agent, params)
      dump_obj(GMM, "GMM", agent, params)
      stop(c)
    })
  }
}
