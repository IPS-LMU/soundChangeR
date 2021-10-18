all_exemplar2fd <- function(exemplars) {
  dim2 <- exemplars[[1]][["coefs"]] %>% dim %>% .[2]
  if (dim2 == 1) {
    coefs <- abind(lapply(exemplars, `[[`, "coefs"), along = 2)
  } else {
    coefs <- abind(lapply(exemplars, `[[`, "coefs"), along = 3) %>% aperm(c(1,3,2))
  }
  fd(coef = coefs, basisobj = exemplars[[1]][["basis"]])
}
