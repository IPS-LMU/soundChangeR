fdMSE <- function(fd1, fd2) {
  apply(fd1$coefs - fd2$coefs, -1L, function(x) {
    defint.fd(exponentiate.fd(fd(x, fd1$basis), 2))
  }) %>% sum %>% `/`(diff(fd1$basis$rangeval))
}
