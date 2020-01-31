################################################################################
#                                                                              #
# This script contains some utility functions that are not needed during the   #
# ABM run, but may be helpful for data analysis.                               #
#                                                                              #
# ABM developed by Florian Schiel and Jonathan Harrington                      #
# Adapted by Johanna Cronenberg and Michele Gubian                             #
#                                                                              #
# Copyright 2020, Institute of Phonetics and Speech Processing, LMU Munich.    #
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
  #    - X: a row of matrix(P1, P2, P3)
  #    - N: Default: 11.
  #
  # Returns:
  #    - numeric vector
  #

  0.5 * (sqrt(2) - 1) * X[1] + dtt::dct(c(X, rep(0, N - length(X))), variant = 3)
}

reconstruct_tracks <- function(df) {
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
  valueColumns <- grep("DCT", names(df), value = T)
  coeffs <- as.matrix(dplyr::select(df, valueColumns))
  
  reconstructedTracks <- c(apply(coeffs, 1, function(x) {
    inv_dct_from_emuR(X = x, N = 21)
  }))
  
  track.df <- data.frame(track = reconstructedTracks, time = rep(seq(0, 1, length = 21), times = nrow(coeffs)),
                         initial = rep(as.character(df$initial), each = 21), label = rep(as.character(df$label), each = 21),
                         word = rep(as.character(df$word), each = 21), speaker = rep(as.character(df$speaker), each = 21),
                         group = rep(as.character(df$group), each = 21), condition = rep(as.character(df$condition), each = 21),
                         rem = rep(as.character(df$rem), each = 21))
  result <- track.df %>% group_by(time, initial, group, rem, condition) %>% summarise(track = mean(track))
  return(result)
}

