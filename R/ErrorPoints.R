#' Plot Horizontal and Vertical Line Segments on Existing Plot
#'
#' @param x a vector of x coordinates used as a centroid from which to draw segments.
#' @param x.unc.lo a vector of uncertainites added and subracted from x to create segments.
#' @param x.unc.hi a vector of uncertainites added and subracted from x to create segments.
#' @param y a vector of y coordinatesused as a centroid from which to draw segments.
#' @param y.unc.lo a vector of uncertainites added and subracted from y to create segments.
#' @param y.unc.hi a vector of uncertainites added and subracted from y to create segments.
#' @param xlog.value default 0.01
# make this a log = x y or xy in future
#' @param na.rm logical; if TRUE, NA values in x and x.unc (y and y.unc) values result in no vertical (horizontal) line being drawn.
#' @param pch if supplied, point plotted with pch character before error lines drawn
#' @param pch.cex cex to use for drawing points
#' @param ... further arguments passed to segments
#' @export ErrorPoints
ErrorPoints  <- function(x = NULL, x.unc.lo = NA, y = NULL, y.unc.lo = NA, x.unc.hi = NULL, y.unc.hi = NULL, xlog.value = 0.01, na.rm = TRUE, pch, pch.cex = 0.75,...){

  if ( is.null(x) | is.null(y)) stop("no x or y")

  if ( !missing(pch)) points( x, y, pch = pch, cex = pch.cex)

  # if no high range inputed, use low range.
  if ( is.null(x.unc.hi)) x.unc.hi <- x.unc.lo
  if ( is.null(y.unc.hi)) y.unc.hi <- y.unc.lo

  # if NAs should be removed
  if (na.rm) {
    x[ is.na(y.unc.lo) | is.na(y.unc.hi)] <- NA
    y[ is.na(x.unc.lo) | is.na(x.unc.hi)] <- NA
  }

  xlog <- par("xlog")

  if ( !xlog) {
    segments(x - x.unc.lo, y , x + x.unc.hi, y, ...)
  } else {
    x.lo <- x - x.unc.lo
    x.lo[x.lo <= 0] <- xlog.value
    segments( x.lo, y, x + x.unc.hi, y, ...)
  }
  segments(x, y - y.unc.lo, x, y + y.unc.hi, ...)
}
