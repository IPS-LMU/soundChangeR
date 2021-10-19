smote_one_class <- function(features, K, N) {

  if (K <= 0) {
    stop(base::paste("smote_one_class: invalid nearest neighbour parameter K =", K))
  }
  if (N <= 0) {
    stop(base::paste("smote_one_class: invalid number of samples requested N =", N))
  }

  if (!base::is.data.frame(features)) {
    features <- base::data.frame(features)
  }

  if (base::nrow(features) == 0) {
    return (NULL)
  }
  if (base::nrow(features) == 1) {
    return (base::matrix(base::rep(base::as.numeric(features), N), ncol = base::ncol(features), byrow = TRUE))
  }

  K <- base::min(K, base::nrow(features) - 1)

  smote_res <- smotefamily::SMOTE(
    if (base::ncol(features) == 1) {
      base::cbind(base::as.data.frame(features), XCOL = 0)
    } else {
      base::as.data.frame(features)
    },
    base::rep("a", base::nrow(features)),
    K,
    base::ceiling(N/base::nrow(features))
  )
  smote_res %>%
    .$syn_data %>%
    .[base::sample(base::nrow(.), N), 1:base::ncol(features)] %>%
    base::as.matrix()
}
