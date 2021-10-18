equal_class <- function(orig, derived) {

  tab <- t(table(orig, derived))
  namesOfOrig <- colnames(tab)
  namesOfDerived <- rownames(tab)
  bintab <- matrix(FALSE, nrow = nrow(tab), ncol = ncol(tab))
  for (j in 1:ncol(tab)) {
    bintab[,j] <- tab[,j] != 0
  }
  for (j in 1:nrow(bintab)) {
    lab.equivalent <- paste(namesOfOrig[bintab[j, ]], collapse="+")
    derived[derived == namesOfDerived[j]] <- lab.equivalent
  }
  return(derived)
}
