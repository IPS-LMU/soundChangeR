compute_fpca <- function(exemplars, agent, params) {

  fdObj <- all_exemplar2fd(exemplars)
  nDim <- ifelse(is.matrix(fdObj$coefs), 1, fdObj$coefs %>% dim %>% .[3])
  pcafdPar  <- fdPar(fdObj$basis, 2, params[["lambdaFPCA"]])
  fpcaObj <- pca.fd(fdobj = fdObj, nharm = fdObj$basis$nbasis * nDim, harmfdPar = pcafdPar)
  nPC <- Position(function(x) {x >= params[["varCutoffFPCA"]]}, cumsum(fpcaObj$varprop))
  MSE <- sapply(1:length(exemplars), function(i){
    fdMSE(
      FPCscores2fd(fpcaObj$scores[i, 1:nPC], fpcaObj),
      fdObj[i]
    )
  }, simplify = TRUE)
  set_cache_value(agent, "FPCA", fpcaObj)
  set_cache_value(agent, "MSE", MSE)
  fpcaObj$scores[, 1:nPC, drop = FALSE]
}
