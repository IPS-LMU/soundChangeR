smote_one_class <- function(features, K, N) {

  if (K <= 0) {
    stop(paste("smote_one_class: invalid nearest neighbour parameter K =", K))
  }
  if (N <= 0) {
    stop(paste("smote_one_class: invalid number of samples requested N =", N))
  }

  if (!is.data.frame(features)) {
    features <- data.frame(features)
  }

  if (nrow(features) == 0) {
    return (NULL)
  }
  if (nrow(features) == 1) {
    return(matrix(rep(as.numeric(features), N), ncol = ncol(features), byrow = TRUE))
  }

  K <- min(K, nrow(features) - 1)

  smote_res <- SMOTE(
    if (ncol(features) == 1) {
      cbind(as.data.frame(features), XCOL = 0)
    } else {
      as.data.frame(features)
    },
    rep("a", nrow(features)),
    K,
    ceiling(N/nrow(features))
  )
  smote_res %>%
    .$syn_data %>%
    .[sample(nrow(.), N), 1:ncol(features)] %>%
    as.matrix
}
