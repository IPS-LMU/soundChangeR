#' @importFrom tibble tibble
NULL

#' Vowels of 22 Standard Southern British English speakers
#'
#' A dataset containing acoustic data (DCT coefficients) for three vowels /i, u, ju/ 
#' produced by 11 older and 11 younger British English speakers.
#'
#' @format A data frame with 2390 rows and 7 variables:
#' \describe{
#'   \item{spk}{speaker code}
#'   \item{age}{age group of speaker}
#'   \item{word}{word class}
#'   \item{phoneme}{canonical phoneme associated with the vowel in given word}
#'   \item{DCT0}{first DCT coefficient computed from second formant trajectory}
#'   \item{DCT1}{second DCT coefficient computed from second formant trajectory}
#'   \item{DCT2}{third DCT coefficient computed from second formant trajectory}
#' }
#' @source Jonathan Harrington (email: jmh@@phonetik.uni-muenchen.de)
"u_fronting"

#' Parameter list
#'
#' A list of parameters to the agent-based model, written in YAML.
#'
#' @format A YAML list.
"params"
