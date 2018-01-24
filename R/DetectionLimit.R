#' Determine the limit of detection L[D]
#'
#' @param B continuum counts
#' @param n peak width in channels
#' @param m channels used for background determination
#' @param k.alpha factor for probability interval, default is 1.645 for alpha of 0.05
#'
#' @description "What is the minimum number of counts I can be confident of detecting?" Gilmore (2008) Eq. 5.63 p.118

DetectionLimit <- function( B, n, m, k.alpha = 1.645){
  2.71 + ( 2 * k.alpha) * sqrt(B * ( 1 + n/( 2 * m)))
  }
