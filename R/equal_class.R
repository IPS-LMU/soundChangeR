equal_class <- function(orig, derived) {

  tab <- base::t(base::table(orig, derived))
  namesOfOrig <- base::colnames(tab)
  namesOfDerived <- base::rownames(tab)
  bintab <- base::matrix(FALSE, nrow = base::nrow(tab), ncol = base::ncol(tab))
  for (j in 1:base::ncol(tab)) {
    bintab[,j] <- tab[,j] != 0
  }
  for (j in 1:base::nrow(bintab)) {
    lab.equivalent <- base::paste(namesOfOrig[bintab[j, ]], collapse = "+")
    derived[derived == namesOfDerived[j]] <- lab.equivalent
  }
  return(derived)
}
