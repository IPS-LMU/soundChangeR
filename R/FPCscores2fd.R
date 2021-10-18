FPCscores2fd <- function(scores, fpcaObj) {
  if (is.matrix(fpcaObj$meanfd$coefs)) { # 1D curve
    coefs <- fpcaObj$meanfd$coefs +
      (sapply(seq_along(scores), function(pc) {
        scores[pc] * fpcaObj$harmonics$coefs[, pc]
      }, simplify = TRUE)
      ) %>%
      apply(1, sum)
  } else { # multi-D curve
    nDim <- dim(fpcaObj$meanfd$coefs)[3]
    coefs <- do.call(cbind, lapply(1:nDim, function(dimInd) {
      fpcaObj$meanfd$coefs[,1,dimInd] +
        (sapply(seq_along(scores), function(pc) {
          scores[pc] * fpcaObj$harmonics$coefs[, pc, dimInd]
        }, simplify = TRUE)
        ) %>%
        apply(1, sum)
    }))
  }
  fd(coefs, fpcaObj$meanfd$basis)
}
