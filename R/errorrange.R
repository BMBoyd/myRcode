#' Get range for vector with errors
#'
#' @param x numeric vector
#' @param x.unc numeric vector of uncertainties same length as x
#' @param na.rm remove na? default TRUE
#' @export

errorrange <- function( x, x.unc = 0.75, na.rm = T){

  range(c(range(x + x.unc, na.rm = na.rm),  range(x - x.unc, na.rm = na.rm)), na.rm = na.rm)

}
