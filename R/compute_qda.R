compute_qda <- function(agent) {

  MASS::qda(base::as.matrix(agent$features)[agent$memory$valid == TRUE, , drop = FALSE],
            grouping = agent$memory$label[agent$memory$valid == TRUE])
}
