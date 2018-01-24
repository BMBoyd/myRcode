#' Determine the critical limit
#'
#' @param B continuum counts
#' @param n peak width in channels
#' @param m channels used for background determination
#' @param k.alpha factor for probability interval, default is 1.645 for alpha of 0.05
#'
#' @description "Is the net count significant" Gilmore (2008) Eq. 5.56 p.116

CriticalLimit <- function(B, n, m, k.alpha=1.645){
  k.alpha * sqrt(B * (1 + n/(2 * m)))
}
