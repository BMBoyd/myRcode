#' Determine if the peak is significant
#'
#' @param A net peak counts
#' @param A.unc net peak counts
#' @param B continuum counts
#' @param n peak width in channels
#' @param m channels used for background determination
#' @param k.alpha factor for probability interval, default is 1.645 for alpha of 0.05
#'
#' @description

CountSig <- function(A, A.unc, B, n, m, k.alpha=1.645){
  ifelse(A + A.unc >= CriticalLimit(B, n, m, k.alpha), "Significant", UpperLimit(A, B, n, m, k.alpha))
}
