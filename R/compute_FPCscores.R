compute_FPCscores <- function(fdObj, fpcaObj, nPC) {
  if (is.matrix(fpcaObj$meanfd$coefs)) { # 1D curve
    inprod(fdObj - fpcaObj$meanfd, fpcaObj$harmonics[1:nPC]) %>% as.numeric
  } else { # multi-D curve
    nDim <- dim(fpcaObj$meanfd$coefs)[3]
    sapply(1:nPC, function(pc) {
      sapply(1:nDim, function(dimInd) {
        inprod(fd(coef = fdObj$coefs[, dimInd] - fpcaObj$meanfd$coefs[, 1, dimInd], basisobj = fdObj$basis),
               fpcaObj$harmonics[pc,dimInd])
      }) %>% sum
    }, simplify = TRUE)
  }
}
