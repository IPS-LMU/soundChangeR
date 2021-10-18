compute_qda <- function(agent) {

  qda(as.matrix(agent$features)[agent$memory$valid == TRUE, , drop = FALSE],
      grouping = agent$memory$label[agent$memory$valid == TRUE])
}
