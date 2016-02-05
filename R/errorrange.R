#' Get range for vector with errors
#'
#' @param x numeric vector
#' @param x.unc numeric vector of uncertainties same length as x
#' @param xlog logical; if TRUE changes nonpositive values to xlog.value
#' @param xlog.value default 0.01
#' @param na.rm remove na? default TRUE
#' @export

errorrange <- function(x, x.unc , xlog = F, xlog.value = 0.01, na.rm = T){
  if ( missing(x) | missing(x.unc)) stop("x or x.unc missing with no default")

  x.unc <- abs(x.unc)
  x.unc[is.na(x.unc)] <- 0
  x.lo <- x - x.unc
  if (xlog)  x.lo[ x.lo <= 0] <- xlog.value
  range(c(range(x + x.unc, na.rm = na.rm),  range(x.lo, na.rm = na.rm)), na.rm = na.rm)
}
