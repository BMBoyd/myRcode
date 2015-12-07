#' Plot Horizontal and Vertical Line Segments on Existing Plot
#'
#' @param x a vector of x coordinates used as a centroid from which to draw segments.
#' @param x.unc a vector of uncertainites added and subracted from x to create segments.
#' @param y a vector of y coordinatesused as a centroid from which to draw segments.
#' @param y.unc a vector of uncertainites added and subracted from y to create segments.
#' @param show.hor logical; if TRUE, function draws horizontal segments
#' @param show.ver logical; if TRUE, function draws vertical segments
#' @param xlog logical; if TRUE changes nonpositive values to xlog.value
#' @param xlog.value default 0.01
# make this a log = x y or xy in future
#' @param na.rm logical; if TRUE, NA values in x and x.unc (y and y.unc) values result in no vertical (horizontal) line being drawn.
#' @param ... further arguments passed to segments
#' @export ErrorPoints
ErrorPoints<- function(x = NA, x.unc = NA, y = NA, y.unc = NA, show.hor = TRUE, show.ver = TRUE, xlog = FALSE, xlog.value = 0.01, na.rm = TRUE, ...){
  if( na.rm){
    y[is.na(x) | is.na(x.unc)] <- NA
    x[is.na(y) | is.na(y.unc)] <- NA
  }
  if( show.hor){
    if( !xlog){
      segments(x - x.unc, y , x + x.unc, y, ...)
    } else {
      xlo <- x - x.unc
      xlo[x - x.unc <= 0] <- xlog.value
      segments( xlo, y, x + x.unc, y, ...)
    }
  }
  if( show.ver){
    segments(x, y - y.unc, x, y + y.unc, ...)
  }
}
