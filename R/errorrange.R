#' Get range for vector with errors
#'
#' @param x
#' @param x.unc
#' @param na.rm
#' @export

errorrange <- function( x, x.unc = 0.75, na.rm = T){

  range(c(range(x + x.unc, na.rm = na.rm),  range(x - x.unc, na.rm = na.rm)), na.rm = na.rm)

}
