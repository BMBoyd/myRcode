#' Determine the upper limit
#'
#' @param A net peak counts
#' @param B continuum counts
#' @param n peak width in channels
#' @param m channels used for background determination
#' @param k.alpha factor for probability interval, default is 1.645 for alpha of 0.05
#'
#' @description "Given that this count is not statisctically significant, what is the maximum statitstically reasonable count?" Gilmore (2008) Eq. 5.59 p.117

UpperLimit <- function(A,B,n,m,k.alpha=1.645){
  A + k.alpha * sqrt(A + B * (1 + n / (2*m)))
}
