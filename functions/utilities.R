################################################################################
#                                                                              #
# This script contains some utility functions that are not needed during the   #
# ABM run, but may be helpful for data analysis.                               #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2019, Institute of Phonetics and Speech Processing, LMU Munich.    #
#                                                                              #
################################################################################

ellipse_confidence <- function(P, alpha) {
  # This function is a textbook way of calculating the 
  # ellipse confidence interval of a distribution.
  #
  # Args:
  #    - P:
  #    - alpha:
  #
  # Returns:
  #    - a list of...
  #

  # only 2-dim P
  eig <- eigen(cov(P), only.values = FALSE)
  Chisq <- sqrt(qchisq(1 - alpha, 2))
  return(list(
    C = colMeans(P),
    a = Chisq * sqrt(eig$values[1]),
    b = Chisq * sqrt(eig$values[2]),
    theta = atan2(eig$vectors[2,1], eig$vectors[1,1])
  ))
}

inv_dct_from_emuR <- function(X, N = 11) {
  # This function computes the inverse DCT for DCTs that were
  # calculated by means of emuR::dct.
  #
  # Args:
  #    - X:
  #    - N: Default: 11.
  #
  # Returns:
  #    -
  #

  0.5 * (sqrt(2) - 1) * X[1] + dtt::dct(c(X, rep(0, N - length(X))), variant = 3)
}